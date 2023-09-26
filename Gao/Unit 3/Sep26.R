library(tidyverse)
library(ggplot2)
library(datasets)
library(RJSONIO)

diamonds

#Rename columns to use title case

names(diamonds) <- names(diamonds) %>% str_to_title()
diamonds

#check for NA values

apply(X=diamonds,MARGIN=2,FUN=function(col),sum(is.na(col)))

#Drop rows that have invalid values

diamonds <- diamonds %>% filter(X!=0 & Y!=0 & Z!=0)
diamonds

#Make box plot

ggplot(diamonds, aes(x=X,y=Price,fill=Cut)) + geom_boxplot()

ggplot(diamonds, aes(x=X,y=Price,fill=Clarity)) + geom_boxplot()

ggplot(diamonds, aes(x=X,y=Price,fill=Color)) + geom_boxplot()