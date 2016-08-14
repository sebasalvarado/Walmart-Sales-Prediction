#Walmart Predictive Analysis
library(dplyr)
library(ggplot2)
library(reshape2)
library(readr)
library(lubridate)
library(rpart)
library(rattle)
library(car)
library(caret)
library(corrplot)
library(rpart.plot)
train <- read_csv("train.csv")
stores <- read_csv("stores.csv")
features <- read_csv("features.csv")
#Exploratory Analysis
summary(train[,c("Weekly_Sales","IsHoliday")])
glimpse(train)
summary(stores)
glimpse(stores)
summary(features)
glimpse(features)
#Have the type of stores as factor
stores <- stores %>% transmute(Store=Store,Type=factor(Type),Size=Size)
#Join both tables based on store
stores$Store <- factor(stores$Store)
train$Store <- factor(train$Store)
train <- full_join(train,stores,by=c("Store"))
# EXPLORATORY ANALYSIS ---------------------------------------------
#Feature Engineering: -------------------------------------------------------------------------
# Add returns column and replace Date by the week number
#Add a column that indicates the year and month
train <- train %>% mutate(Year=year(ymd(train$Date)))
train <- train %>% mutate(Month=month(ymd(train$Date)))
#Replace Date for week of the year, we count the last week of December as Week #1
train$WeekNum <- as.numeric(format(train$Date+3,"%U"))
#We noticed that there were negative sales, which seem a little bit odd but they are considered as returns from previous week 
#For now separate the Weekly Sales that are greater than 0
train$Returns <- lapply(train$Weekly_Sales,function(sales){
  ifelse(sales < 0,sales,0)
})
train$Weekly_Sales <- lapply(train$Weekly_Sales,function(sales){
  ifelse(sales > 0,sales,0)
})
#Fixing the Data Types of our columns
train$Returns <- as.numeric(train$Returns)
train$Weekly_Sales <- as.numeric(train$Weekly_Sales)
train$Year <- factor(train$Year)
train$Store <- factor(train$Store)
train$WeekNum <- factor(train$WeekNum)
#Get rid of the department column and add them all together
final_data <- data.frame(Store=factor(),Date=as.Date(character()),Weekly_Sales=numeric(),IsHoliday=logical(),Type=factor(),WeekNum=factor())
aggregate_sales <- function(){
for(i in 1:45){
  store_data <- train %>% filter(Store == i)
  dates <- unique(train$Date)
  for(next_date in seq_along(dates)){
    current_date <- unique(train$Date)[[next_date]]
    date_data <- store_data %>% filter(Date==current_date)
    #Add all the weekly sales
    net_sales <- unlist(sum(date_data$Weekly_Sales)) - unlist(sum(date_data$Returns))
    #Construct the data frame and append it
    #TODO:Add all the columns
    next_row <- data.frame(Store=i,Date=current_date,Weekly_Sales=net_sales,IsHoliday=date_data$IsHoliday[[1]],Type=date_data$Type[[1]],WeekNum=date_data$WeekNum)
    next_row$Store <- factor(next_row$Store)
    final_data <- rbind(final_data,next_row)
  }
}
  return(final_data)
}
# Sum the sales by store without taking into account each departmen
final_data <- aggregate_sales()
#Merge our final_data with our features
train <- left_join(final_table,features,by=c("Store","Date","IsHoliday"))
max_sales <- max(train$Weekly_Sales)
min_sales <- min(train$Weekly_Sales)
# Make the NA markdown as 0
train$MarkDown1 <- sapply(train$MarkDown1, function(value){
  ifelse(is.na(value),0,value)
})
train$MarkDown2 <- sapply(train$MarkDown2, function(value){
  ifelse(is.na(value),0,value)
})
train$MarkDown3 <- sapply(train$MarkDown3, function(value){
  ifelse(is.na(value),0,value)
})
train$MarkDown4 <- sapply(train$MarkDown4, function(value){
  ifelse(is.na(value),0,value)
})
train$MarkDown5 <- sapply(train$MarkDown5, function(value){
  ifelse(is.na(value),0,value)
})
#Range Weekly Sales: Divide our 

#Subset our data into train and test
index <- createDataPartition(train$Weekly_Sales,list = FALSE,p=0.8)
train.train <-train[index,]
train.test <- train[-index,]
tabu <-train.train %>% arrange(Month) %>% filter(Month == 1, Store == 10)
#Find the max and min sales per year, per store type
aggregate(train.train[,"Weekly_Sales"], by=train.train[,c("Type"), drop=FALSE], mean)
aggregate(train.train[,"Weekly_Sales"], by=train.train[,c("Type"), drop=FALSE], max)
#Tabulate our categorical Data
table(train.train$IsHoliday)
my_table <- ftable(train.train[,c("IsHoliday","WeekNum")],col.vars = c(2,1))
#Know which stores belong to each type
store_types <- ftable(train.train[,c("Store","Type")])
margin.table(store_types,1)
prop.table(store_types)
#Now if we calculate the avg, quartiles we have a more sensible data
summary(train.train)
#Calculate Average weekly sales by store and by department
mean_weekly_sales <- aggregate(train.train[,"Weekly_Sales"], by = train.train[,"Store", drop=FALSE],mean)

#Generate some Exploratory Plots
#All Stores correlation
plot(train.train$Temperature,train.train$Weekly_Sales)
plot(train.train$Fuel_Price,train.train$Weekly_Sales)
plot(train.train$CPI,train.train$Weekly_Sales)
plot(train.train$Unemployment,train.train$Weekly_Sales)
plot(train.train$Store,train.train$Weekly_Sales)
plot(train.train$IsHoliday,train.train$Weekly_Sales)#Holiday is very important for increase in Sales
#finding if outliers are due to seasonality
plot(train.train$Type,train.train$Weekly_Sales, xlab="Type", ylab="Sales")
holiday <- train.train %>% filter(IsHoliday == TRUE)
plot(holiday$Type,holiday$Weekly_Sales, xlab="Type", ylab="Sales")
#Partitining Stores by Type
A_stores <- train.train %>% filter(Type=='A')
plot(A_stores$Fuel_Price,A_stores$Weekly_Sales)#Sales vary depending on the weeknum we are in
plot(A_stores$Store,A_stores$Weekly_Sales)#Different Stores have different sales

#Correlation Graph
corrplot.mixed(cor(train.train[,c(-1,-2,-5)]), lower = "ellipse",upper = "number")

#Time Series
sales <- ts(train.train$Weekly_Sales,start = c(2010,1), end=c(2012,12),freq=52)

# DECISION TREE -------------------------------------------------------------------------------------------------------
#Using a decision tree we will like to predict the Type of a store based on all the other parameters
train.rpart <-rpart(Type ~ Weekly_Sales + Returns + Size,data=train.train, control=rpart.control(minsplit=1,cp=0.005))
summary(train.rpart)
fancyRpartPlot(train.rpart)
#new.rpart <- prp(train.rpart,snip=TRUE)$obj
prediction <- predict(train.rpart,train.test, type="class")
train.test$Prediction <- prediction
#Find the percentage accuracy of our model
accur_table <- train.test %>% select(Type,Prediction) 
bool_vector <- accur_table$Type == accur_table$Prediction
accuracy <- length(which(bool_vector)) / length(bool_vector) # 97% accurate model 
#LINEAR REGRESSION  -------------------------------------------------------------------------------------------------------
#Weekly Sales for all stores
fit <- lm(Weekly_Sales ~.-Date-CPI-Type, data=train.train)
class(fit)
summary(fit)
vif(fit)
predict_fit_confidence <- predict(fit, newdata=train.test, interval="confidence", level=0.95)
predict_fit_pred <-predict(fit, newdata=train.test, interval="prediction", level=0.95)
train.test$Prediction <- predict_fit
#plot(fit)
#Weekly Sales for Type A,B and C
stores_A <- train.train %>% filter(Type == 'A')
fitA <- lm(Weekly_Sales ~ Fuel_Price +Store+ Temperature + Unemployment + IsHoliday + CPI + MarkDown1 + MarkDown2 + MarkDown3 + MarkDown4 + MarkDown5, data=stores_A)
summary(fitA)
prediction_all <- predict(fit2,train.test,interval="confidence")
#Weekly Sales in a per store basis
stores_1 <- train.train %>% filter(Store == 1) 
summary(stores_1)
fit1 <- lm(Weekly_Sales ~ Temperature + Unemployment + IsHoliday + CPI + MarkDown1 + MarkDown2 + MarkDown3 + MarkDown4 + MarkDown5, data =stores_1)
summary(fit1)
train.predict = predict(train.reg,train_1, interval="confidence")

#TASK:Predict Weekly Sales per store, department and Date---------------------------------------
#We will need to join the features because that variables are important for sales

