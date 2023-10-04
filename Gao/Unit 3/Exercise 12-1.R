# Exercise 12-1

# Create a Simple Regression Model

# Part I: Get, Clean and Examine the Data

# Load the packages, tidyverse and tidymodels

library(tidyverse)

# Read the melbourne_housing.csv file from your data folder

getwd()
setwd("..")
setwd("data")

housing <- read_csv("melbourne_housing.csv")

names(housing) <- names(housing) %>% str_to_title()

# Examine your data for missing values

apply(X = housing, MARGIN = 2, FUN = function(col) 
  sum(is.na(col)))

# Drop all rows that contain missing values

housing <- na.omit(housing)
housing

# Examine the r-values between the Price column and the
# other numeric columns

cor(x = housing$Price, y = housing$Buildingarea)
cor(x = housing$Price, y = housing$Rooms)
cor(x = housing$Price, y = housing$Car)
cor(x = housing$Price, y = housing$Landsize)

# Display a scatter pot that shows the relationshiop between
# the Price and BuildingArea variables

ggplot(housing, aes(x=Price, y=Buildingarea)) + geom_point()

# Part II: Preparing the Data

# Split the data into testing and training data sets with
# 75% of the data in the training data set

housing_split <- initial_split(housing, prop = 0.75)
train <- training(housing_split)
test <- testing(housing_split)

# Create functions for calculating the upper and lower
# fences for outliers

get_upper_fence <- function(x) {
  quantile(x, 0.75) + (1.5 * IQR(x))
}

get_lower_fence <- function(x) {
  quantile(x, 0.25) - (1.5 * IQR(x))
}

# In the training data, use the fences to drop rows that
# contain outliers from the Price and BuildingArea columns

train <- train %>% 
  filter_at(vars(Price, Buildingarea), 
            all_vars(. > get_lower_fence(.) &
                       . < get_upper_fence(.)))

# Drop any other rows that contain additional outliers
# such as rows where the BuildingArea is very low but the
# Price is high.

# Part III: Create a Model

# Create a linear regression model wehre the Price variable
# is a function of the BuildingArea variable

# Use the odel to make predictions for the price in the
# testing data set and add those predictions to the
# testing data set

# Display a scatter plot that shows the actual and predicted
# values in the testing data set

# Part IV: Plot Equations and Formulas

# Display the intercept and coefficient for the equation
# that's used by the model

# Use the geom_function() function to plot the equation
# that's used by the model over a scatter plot of the
# test data set

# Use the geom_smooth() function plot the formula that's
# used by the model over a scatter plot of the test data set



