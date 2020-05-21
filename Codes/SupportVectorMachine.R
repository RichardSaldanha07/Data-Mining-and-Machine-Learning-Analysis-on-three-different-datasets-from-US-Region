# Author: Richard Saldanha
# Student ID: 18183034
# Problem statement : the crowdedness of the gym at UC Berkey is during the start of the semester 
#or other duration of the semester.

data <- read.csv(file="crowdednessatcampusgym.csv",header = TRUE, sep=",")

#Install the necessary packages
install.packages("dplyr")
install.packages('caret')


#Import the packages
library(dplyr)
library('caret')
library(caTools)

# Drop the uncessary columns of the dataframe
mydata <- select (data,-c(date, timestamp, is_during_semester))

View(mydata)       

# Check whether there are any null values in the updated data
is.na(mydata)
sum(is.na(mydata))

# To check the structure of the dataset we make use of str function

str(mydata)
head(mydata)


#Split the data into training set and testing set
# we spit the dataset into 70 / 30 ratio 

set.seed(123)
split <- sample.split(mydata$is_start_of_semester,SplitRatio = 0.7)
split
trainingdata <- subset(mydata,split=="TRUE") # 0.7 part of the dataset is True
testingdata <- subset(mydata,split=="FALSE") # 0.3 part of the dataset is False


#View the training and testing dataset
View(testingdata)
View(trainingdata)
# Check the dimension of training and testing datadframe as well as validate if there is any missing values

dim(trainingdata);
dim(testingdata);
anyNA(mydata)


#Summary of the dataset
summary(mydata)

# Encoding the target feature as factor either way
# trainingdata$is_start_of_semester = factor(trainingdata$is_start_of_semester, levels = c(0, 1))

trainingdata[["is_start_of_semester"]] = factor(trainingdata[["is_start_of_semester"]])

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

svm_Linear <- train(is_start_of_semester~.,data = trainingdata,method = "svmLinear",trControl = trctrl,preProcess = c("center","scale"),tuneLength = 10)


svm_Linear

test_pred <- predict(svm_Linear, newdata = testingdata)
test_pred

#Now we can use the confusion matrix to predict the accuracy

confusionMatrix(table(test_pred, testingdata$is_start_of_semester))


grid <- expand.grid(C = c(0.05, 0.1, 0.25, 0.5, 0.75))
set.seed(123)

svm_Linear_Grid <- train(is_start_of_semester~.,data = trainingdata,method = "svmLinear",trControl = trctrl,preProcess = c("center","scale"),tuneGrid = grid,tuneLength = 10)

svm_Linear_Grid
plot(svm_Linear_Grid)


test_pred_grid <- predict(svm_Linear_Grid, newdata = testingdata)
test_pred_grid

confusionMatrix(table(test_pred_grid, testingdata$is_start_of_semester))





