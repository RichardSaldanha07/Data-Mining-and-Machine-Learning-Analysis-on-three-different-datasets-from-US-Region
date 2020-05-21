# Author: Richard Saldanha
# Student ID: 18183034
# Problem statement : the crowdedness of the gym at UC Berkey is during the start of the semester 
#or other duration of the semester.


data <- read.csv(file="crowdednessatcampusgym.csv",header = TRUE, sep=",")



# Install the necessary Packages

install.packages("dplyr")
install.packages("caTools")
install.packages("e1071")
install.packages("caret")

# Import the necessary libraries
library(dplyr)
library(caTools)
library(e1071)
library(caret)


# Drop the uncessary columns of the dataframe
mydata <- select (data,-c(date, timestamp, is_during_semester))

View(mydata)       

# Check whether there are any null values in the updated data
is.na(mydata)
sum(is.na(mydata))


#Split the data into training set and testing set
# we spit the dataset into 70 / 30 ratio 

set.seed(123)
id <- sample(2, nrow(mydata), prob = c(0.7,0.3), replace = T)
gymtrain <- mydata[id==1,]
gymtest <- mydata[id ==2,]


# We need to factor our predictor variable
gymtrain$is_start_of_semester = factor(gymtrain$is_start_of_semester, levels = c(0, 1))


# gym_nb is our variable on which Naive Baye Algorithm is applied
gym_nb <- naiveBayes(is_start_of_semester ~ number_people + day_of_week + is_weekend + is_holiday + temperature + month + hour, data =gymtrain)
gym_nb


# predition with the test data set

gympredict <- predict(gym_nb,gymtest)
gympredict

length(gympredict)

# To check for the accuracy of the model
confusionmatrix <- confusionMatrix(table(gympredict, gymtest$is_start_of_semester))
confusionmatrix

Accuracy <- (1849 + 1440)/(15214+1440+25+1849)
Accuracy


