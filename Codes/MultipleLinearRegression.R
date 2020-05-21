# Author: Richard Saldanha
# Student ID: 18183034
# Problem Statement:Implementation Multiple Linear Regression on Avocado Prices

data <- read.csv(file="avocadoprices.csv",header = TRUE, sep=",")


# Install the necessary packages
install.packages("car")
install.packages("dplyr")
install.packages("corrplot")
install.packages("caTools")
#Import the necessary packages

library(dplyr)
library(caTools)
library(car)
library(corrplot)


# Drop the uncessary columns of the dataframe
mydata <- select (data,-c(Date,TotalVolume,TotalBags,SmallBags,LargeBags,XLargeBags,year,region))

View(mydata)

# Check whether there are any null values in the updated data
is.na(mydata)
sum(is.na(mydata))

# Encoding categorical data
mydata$type = factor(mydata$type,
                       levels = c('conventional', 'organic'),
                       labels = c(1, 2))



# Splitting the dataset into the Training set and Test set

set.seed(123)
split = sample.split(mydata$AveragePrice, SplitRatio = 0.8)
trainigdata = subset(mydata, split == TRUE)
testingdata = subset(mydata, split == FALSE)
View(trainigdata)
View(testingdata)


# Fitting Multiple Linear Regression to the Training set
mlregressor = lm(formula = AveragePrice ~ .,
               data = trainigdata)


summary(mlregressor)



#Now we predict the test results

y_pred = predict(mlregressor, newdata = testingdata)

y_pred

View(testingdata)


# We now can plot the regression diagnostic plot for our model and observe the results

plot(mlregressor)


# To make all four diagnostic plot appear on the same screen we make use of
par(mfrow=c(2,2))

plot(mlregressor)


# To check the Variance Inflation factor


mymodel <- lm(AveragePrice~.,data = trainigdata)
vif(mymodel)



# Lets now compare the predicted values and actual values, we can use the plot function

plot(testingdata$AveragePrice, type = "l", lty = 1.8, col = "green")
lines(y_pred, type="l", col="blue")

