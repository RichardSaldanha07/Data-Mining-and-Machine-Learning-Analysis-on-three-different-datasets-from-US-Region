# Author: Richard Saldanha
# Student ID: 18183034
# Problem Statement:Implementation of Random Forest on housesalesprediction 
#dataset to find out whether the houses purchased will have have water front view or not

# Implementation of Random Forest Classifier on housesalesprediction dataset

data <- read.csv(file="housesalesprediction.csv",header = TRUE, sep=",")


# Install the necessary Packages
install.packages("Rcpp")
install.packages("dplyr")
install.packages("randomForest")
install.packages("corrplot")
install.packages("e1071")

# Import the necessary packages
library(dplyr)
library(randomForest)
library(corrplot)
library(caret)
library(ROCR)

# Drop the uncessary columns of the dataframe
mydata <- select (data,-c(id,date,sqft_lot,view,condition,grade,sqft_above,sqft_basement,zipcode,lat,long,sqft_living15,sqft_lot15))

# Check whether there are any null values in the updated data
is.na(mydata)
sum(is.na(mydata))
View(mydata)

# Next we split our data into testing and training for analysis
# This is another way to split the data into training and testing dataset
set.seed(123)
id <- sample(2, nrow(mydata), prob = c(0.8,0.2), replace = TRUE)

waterfront_train <- mydata[id==1,]
waterfront_test <- mydata[id==2,]

# apply factorization on the predict variable as well as apply it on the train dataset
mydata$waterfront <- as.factor(mydata$waterfront)
waterfront_train$waterfront <- as.factor(waterfront_train$waterfront)


# We make use of the TuneRF function to find out the optimized value of m

bestmtry <- tuneRF(waterfront_train,waterfront_train$waterfront,stepFactor = 1.2,improve = 0.01, trace= T, plot = T) 



# Apply the random forest model
waterfront_forest <- randomForest(waterfront~., data = waterfront_train, family = "binomial")
summary(waterfront_forest)

# To find out about the significance of the variables we make use importance()
importance(waterfront_forest)


#To Plot it we make use of varImpPlot

varImpPlot(waterfront_forest)



# Predict the values for the test dataset and compare with the test dataset

predictdata <- predict(waterfront_forest, newdata = waterfront_test, type="class")
predictdata
View(waterfront_test)



# Creating the Confusion Matrix  and finding out the Accuracy using the carot library
confusionmatrix <- confusionMatrix(table(predictdata, waterfront_test$waterfront))
confusionmatrix

# Now Lets find out the Accuracy

Accuracy <- (4233+1) / (4233+33+0+1)
Accuracy

