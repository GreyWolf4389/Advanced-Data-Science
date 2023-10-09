library(tidyverse)

getwd()
setwd("..")

cars <- read_csv("pakistan.csv")

apply(X = cars, MARGIN = 2, FUN = function(col) 
  sum(is.na(col)))

cars <- na.omit(cars)

cor(x = cars$mileage, y = cars$price)

ggplot(cars, aes(x=mileage, y=price)) + geom_point()
