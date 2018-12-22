-----------------------------------------
Title: "Predicting Bike Rental Demand"
Author: 'Sahil chahal'
------------------------------------------

rm(list=ls())

#SETTING WORKING DIRECTORY
setwd("F:/project")

#IMPORTING DATA
data=read.csv('day.csv',header=T)

# Load all the packages required for the analysis
library(ggplot2)
library(dplyr)
library(e1071)
library(randomForest)
library(scales)
library(gplots)
x = c( "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees')
lapply(x, require, character.only = TRUE)
library(fastDummies)

###########Expolatry data analysis##############################

str(data)
data$season=as.factor(data$season)
data$yr=as.factor(data$yr)
data$mnth=as.factor(data$mnth)
data$weekday=as.factor(data$weekday)
data$workingday=as.factor(data$workingday)
data$weathersit=as.factor(data$weathersit)
data$holiday=as.factor(data$holiday)
data=subset(data,select = -c(casual,registered))
d1=unique(data$dteday)
df=data.frame(d1)
df$d1=as.Date(df$d1,format="%Y-%m-%d")
data$dteday=format(as.Date(df$d1,format="%Y-%m-%d"), "%d")
data$dteday=as.factor(data$dteday)
str(data)

#scatter plot temprature vs count
ggplot(data,aes(temp,cnt)) + 
  geom_point(aes(color=temp),alpha=0.8) + theme_bw()


#scaterplot for humidity vs count
ggplot(data,aes(hum,cnt)) + 
  geom_point(aes(color=temp),alpha=0.8) + 
  scale_color_gradient(high='purple',low='green') + 
  theme_bw() 

#Histogram for windspeed vs count
ggplot(data, aes_string(x = data$windspeed)) + 
  geom_histogram(fill="cornsilk", colour = "black") + geom_density() +
  scale_y_continuous(breaks=pretty_breaks(n=10)) + 
  scale_x_continuous(breaks=pretty_breaks(n=10))+
  theme_bw() + xlab("windspeed")

#Missing values analysis
# creating dataframe with missing value  persentage
Mising_val=data.frame(apply(data, 2,function(x){sum(is.na(x))}))


#####################Feature Selection#################


## Correlation Plot
numeric_index = sapply(data,is.numeric) #selecting only numeric
corrgram(data[,numeric_index], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

## Dimension Reduction

data = subset(data,select = -c(atemp))

########################  Creating Dummy variables ######################

cnames= c("instant","dteday","season","yr","mnth","holiday","weekday","workingday","weathersit","temp","hum","windspeed")
data1=data[,cnames]
cnt=data.frame(data$instant,data$cnt)
names(cnt)[1]="instant"
names(cnt)[2]="cnt"
data1 <- fastDummies::dummy_cols(data1)
data1 =merge(data1,cnt, by="instant")
data1=subset(data1,select = -c(instant,dteday,season,yr,mnth,holiday,weekday,workingday,weathersit,yr_1,holiday_1,workingday_1))

###################Model Development#####################################

#Divide the data into train and test
train_index = sample(1:nrow(data1), 0.8*nrow(data1))
train=data1[train_index,]
test=data1[-train_index,]

#LINEAR REGRESTION MODEL
lm_model = lm(cnt ~., data = train)
LR = predict(lm_model,test[,-64])

#DEFINING MAPE FUNCTION
MAPE = function(y, yhat){
  mean(abs((y - yhat)/y))
}
MAPE(test[,64], LR)
summary(lm_model)


# Random forest test
RF=randomForest(cnt ~.,train, importance= TRUE ,ntree=160)
treeList = RF2List(RF)
RF_Predictions = predict(RF, test[,-64])

MAPE(test[,64],RF_Predictions)
plot(RF)

#TO WRITE THE DATA IN FOR RESULTS
results <- data.frame(test[-64],count = RF_Predictions)
write.csv(results, file = 'BikeDemandrandom forestmnew.csv', row.names = FALSE, quote=FALSE)

###########Decision tree regression  #################################################
fit = rpart(cnt ~ ., data = train, method = "anova")
predictions_DT = predict(fit, test[,-64])

MAPE(test[,64], predictions_DT)


