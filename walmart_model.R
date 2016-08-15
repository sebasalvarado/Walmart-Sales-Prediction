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
train$Store <- factor(train$Store)
train$WeekNum <- factor(train$WeekNum)
#Get rid of the department column and add them all together
final_data <- data.frame(Store=factor(),Date=as.Date(character()),Weekly_Sales=numeric(),IsHoliday=logical(),Type=factor(),WeekNum=factor())
aggregate_sales <- function(){
for(i in 1:45){
  store_data <- train %>% filter(Store == i)
  print(i)
  dates <- unique(train$Date)
  for(next_date in seq_along(dates)){
    current_date <- dates[[next_date]]
    date_data <- store_data %>% filter(Date==current_date)
    #Add all the weekly sales
    net_sales <- unlist(sum(date_data$Weekly_Sales)) - unlist(sum(date_data$Returns))
    #Construct the data frame and append it
    next_row <- data.frame(Store=i,Date=current_date,Weekly_Sales=net_sales,IsHoliday=date_data$IsHoliday[[1]],Type=date_data$Type[[1]], WeekNum=date_data$WeekNum[[1]], Size=date_data$Size[[1]])
    next_row$Store <- factor(next_row$Store)
    final_data <- rbind(final_data,next_row)
  }
}
return(final_data)
}
# Sum the sales by store without taking into account each department
final_data <- aggregate_sales()
write.csv(final_data, file="merged_train.csv",row.names = FALSE)
#Merge our final_data with our features
train <- read_csv("merged_train.csv")
train <- left_join(train,features,by=c("Store","Date","IsHoliday"))
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

#Range Weekly Sales: Divide our sales into five different groups
range_sales <- range(train$Weekly_Sales)
range <- (range_sales[[2]] - range_sales[[1]]) / 5
first <- c(range_sales[[1]], range_sales[[1]] + range)
second <- c(range_sales[[1]] + range, range_sales[[1]] + 2*range)
third <-c(range_sales[[1]] + 2*range, range_sales[[1]] + 3*range)
fourth <- c(range_sales[[1]] + 3*range, range_sales[[1]] + 4*range)
fifth <- c(range_sales[[1]] + 4*range, range_sales[[2]])
train$Rank <- sapply(train$Weekly_Sales, function(sales){
  if(sales >= first[[1]] &  sales <= first[[2]]){
    return('A')
  }
  else if(sales >= second[[1]] & sales <= second[[2]]){
    return('B')
  }
  else if(sales >= third[[1]] & sales <= third[[2]]){
    return('C')
  }
  else if(sales >= fourth[[1]] & sales <= fourth[[2]]){
    return('D')
  }
  else{
    return('E')
  }
})
#Subset our data into train and test
index <- createDataPartition(train$Weekly_Sales,list = FALSE,p=0.8)
train.train <-train[index,]
train.test <- train[-index,]


#Find the max and min sales per year, per store type
aggregate(train.train[,"Weekly_Sales"], by=train.train[,c("Store"), drop=FALSE], mean)
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


#Generate some Exploratory Plots
#All Stores correlation
ggplot(train.train,aes(x=CPI,y=Weekly_Sales)) + geom_point(aes(color=train.train$Type)) + geom_smooth() + scale_x_continuous(name="Consumer Price Index") + scale_y_continuous(name="Weekly Sales") + scale_color_discrete(name="Type")
ggplot(train.train,aes(x=Unemployment,y=Weekly_Sales)) + geom_point(aes(color=train.train$Type)) + geom_smooth()  + scale_x_continuous(name="Unemployment") + scale_y_continuous(name="Weekly Sales") + scale_color_discrete(name="Type")
ggplot(train.train,aes(x=Temperature,y=Weekly_Sales)) + geom_point(aes(color=train.train$Type)) + geom_smooth()  + scale_x_continuous(name="Temperature") + scale_y_continuous(name="Weekly Sales") + scale_color_discrete(name="Type")


ggplot(train.train,aes(x=MarkDown4,y=Weekly_Sales)) + geom_point(aes(color=train.train$Type)) + geom_smooth()  + scale_x_continuous(name="MarkDown4") + scale_y_continuous(name="Weekly Sales") + scale_color_discrete(name="Type")

#Show which types of store sell more
ggplot(train.train,aes(x=Type,y=Weekly_Sales)) + geom_point(aes(color=train.train$IsHoliday)) + scale_x_discrete(name="Type") + scale_y_continuous(name="Weekly Sales") + scale_color_discrete(name="IsHoliday")

#Finding if outliers are due to seasonality
plot(train.train$Type,train.train$Weekly_Sales, xlab="Type", ylab="Sales")
holiday <- train.train %>% filter(IsHoliday == TRUE)
plot(holiday$Type,holiday$Weekly_Sales, xlab="Type", ylab="Sales")
#Partitining Stores by Type
A_stores <- train.train %>% filter(Type=='A')
ggplot(A_stores,aes(x=CPI,y=Weekly_Sales)) + geom_point(aes(color=A_stores$IsHoliday)) + geom_smooth()#Sales vary depending on the weeknum we are in


#Partition by Store
store_graph <- train.train %>% filter(Store == 20)
ggplot(store_graph,aes(x=CPI,y=Weekly_Sales)) + geom_point(aes(color=store_graph$IsHoliday)) + geom_smooth()
#Correlation Graph
corrplot.mixed(cor(train.train[,c(-1,-2,-5)]), lower = "ellipse",upper = "number")


# DECISION TREE -------------------------------------------------------------------------------------------------------
#Using a decision tree we will like to predict the Type of a store based on all the other parameters
train.rpart <-rpart(Type ~ .,data=train.train, control=rpart.control(minsplit=1,cp=0.005))
summary(train.rpart)
fancyRpartPlot(train.rpart)
#new.rpart <- prp(train.rpart,snip=TRUE)$obj
prediction <- predict(train.rpart,train.test, type="class")
train.test$Prediction <- prediction
#Find the percentage accuracy of our model
accur_table <- train.test %>% select(Type,Prediction) 
bool_vector <- accur_table$Type == accur_table$Prediction
accuracy <- length(which(bool_vector)) / length(bool_vector) # 97% accurate model 

#Use a decision tree to predict which Rank a given store liest in
train.train$Rank <- factor(train.train$Rank)
rank.rpart <- rpart(Rank~ .-Weekly_Sales ,data=train.train,control=rpart.control(minsplit=10,cp=0.01))
summary(rank.rpart)
fancyRpartPlot(rank.rpart)

#LINEAR REGRESSION  -------------------------------------------------------------------------------------------------------
#Weekly Sales for all stores
fit <- lm(Weekly_Sales ~.-Date-Type-CPI, data=train.train)
class(fit)
summary(fit)
vif(fit)
predict_fit_confidence <- predict(fit, newdata=train.test, interval="confidence", level=0.95)
predict_fit_pred <-predict(fit, newdata=train.test, interval="prediction", level=0.95)
predict_uncertainty <- cbind(train.test,predict_fit_confidence,predict_fit_pred[,-1])
train.test <- cbind(train.test,predict_fit_confidence)
results_vector <- ifelse(train.test$Weekly_Sales >= train.test$lwr & train.test$Weekly_Sales <= train.test$upr,TRUE,FALSE)
train.test <- cbind(train.test,results_vector)
sum(results_vector)/nrow(train.test) #Accuracy value is 31%
#Weekly Sales for Type A,B and C
stores_A <- train.train %>% filter(Type == 'A')
fitA <- lm(Weekly_Sales ~ Fuel_Price +Store+ Temperature + Unemployment + IsHoliday + CPI + MarkDown1 + MarkDown2 + MarkDown3 + MarkDown4 + MarkDown5, data=stores_A)
summary(fitA)
prediction_all <- predict(fit2,train.test,interval="confidence")
#Weekly Sales in a per store basis
stores_1 <- train.train %>% filter(Store == 32) 
summary(stores_1)
fit1 <- lm(Weekly_Sales ~ Temperature + Unemployment + IsHoliday + CPI + MarkDown1 + MarkDown2 + MarkDown3 + MarkDown4 + MarkDown5, data =stores_1)
summary(fit1)
train.predict = predict(train.reg,train_1, interval="confidence")

#TASK:Predict Weekly Sales per store, department and Date---------------------------------------
#We will need to join the features because that variables are important for sales

