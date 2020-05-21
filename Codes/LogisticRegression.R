
# Author: Richard Saldanha
# Student ID: 18183034
# Problem Statement:Implementation of Logistic Regression on housesalesprediction 
#dataset to find out whether the houses purchased will have have water front view or not



#Install the necessary Packages

install.packages("Rcpp")
install.packages("dplyr")
install.packages('caTools')
install.packages('ROCR')



#Load the Packages

library(dplyr)
library(caTools) # To use the split function
library(ROCR)

# Import the csv files into the environment
data <- read.csv(file="housesalesprediction.csv",header = TRUE, sep=",")



# Drop the uncessary columns of the dataframe
mydata <- select (data,-c(id,date,sqft_lot,view,condition,grade,sqft_above,sqft_basement,zipcode,lat,long,sqft_living15,sqft_lot15))

# Check whether there are any null values in the updated data
is.na(mydata)
sum(is.na(mydata))


#split our data into testing and training for analysis into 80/20 ratio 

set.seed(123)
split <- sample.split(mydata,SplitRatio = 0.8)
split
trainingdata <- subset(mydata,split=="TRUE") # 0.8 part of the dataset is True
testingdata <- subset(mydata,split=="FALSE") # 0.2 part of the dataset is False


# Implement the Logistic Regression model
  model <- glm(waterfront~.,trainingdata, family = "binomial")

summary(model)

# For Model Optimization: Residual  Deviance should not increase and your AIC should decrease if the condition is met
# we can remove the insignificant variable

model <- glm(waterfront~.-bathrooms,trainingdata, family = "binomial")
summary(model)


# Predict the values for the test dataset and compare with the test dataset

result <- predict(model, testingdata, type="response")
result

View(testingdata)
# Creating the Confusion Matrix  and finding out the Accuracy


confusionmatrix <- (table(Actualvalue=testingdata$waterfront, Predictedvalue=result>0.5))
confusionmatrix

# Now Lets find out the Accuracy

Accuracy <- (5357+8) / (5357+5+33+8)
Accuracy

# ROC curve

res <- predict(model, trainingdata, type="response")
res

#ROC Prediction and performance

ROCRPred = prediction(res, trainingdata$waterfront)
ROCRPerf <- performance(ROCRPred,"tpr","fpr") # tpr - true positive rate, fpr - false positive rate
plot(ROCRPerf, colorize=TRUE, print.cutoffs.at=seq(0.1, by=0.1))  

# We observe that 0.1 has the highest true positive rate while 0.5 threshold has the lowest true positive rate

# So let's change the threshold and observe the Accuracy


newres <- predict(model, testingdata, type="response")
newconfusionmatrix <-(table(Actualvalue=testingdata$waterfront, Predictedvalue=newres>0.1))
newconfusionmatrix

# Now Lets Compare the two values of the confusion matrix observed at 0.5 and 0.1 threshold


confusionmatrix
newconfusionmatrix


# The new Accuracy obtained 

newAccuracy <- (5322+18) / (5322+40+23+18)
newAccuracy
