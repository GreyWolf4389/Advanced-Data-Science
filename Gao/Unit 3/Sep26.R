library(tidyverse)
library(ggplot2)
library(datasets)
library(RJSONIO)

diamonds

#Rename columns to use title case

names(diamonds) <- names(diamonds) %>% str_to_title()
diamonds

#check for NA values

apply(X=diamonds,MARGIN=2,FUN=function(col) sum(is.na(col)))

#Drop rows that have invalid values

diamonds <- diamonds %>% filter(X!=0 & Y!=0 & Z!=0)
diamonds

#Make box plot

ggplot(diamonds, aes(x=X,y=Price,fill=Cut)) + geom_boxplot()

ggplot(diamonds, aes(x=X,y=Price,fill=Clarity)) + geom_boxplot()

ggplot(diamonds, aes(x=X,y=Price,fill=Color)) + geom_boxplot()

#KDE plots with outlier fences

get_upper_fence <- function(x) {
  quantile(x, 0.75) + (1.5 * IQR(x))
}

get_lower_fence <- function(x) {
  quantile(x, 0.25) - (1.5 * IQR(x))
}

diamonds_long <- diamonds %>%
  select_if(is.numeric) %>%
  pivot_longer(cols = names(select_if(diamonds, is.numeric)),
               names_to = "Variable",
               values_to = "Value") %>%
  group_by(Variable) %>%
  mutate(UpperFence = get_upper_fence(Value),
         LowerFence = get_lower_fence(Value))

ggplot(data = diamonds_long, aes(x = Value)) + 
  geom_density(aes(color = Variable)) +
  geom_vline(aes(xintercept = UpperFence), color = "red") + 
  geom_vline(aes(xintercept = LowerFence), color = "red") +
  facet_wrap(facets =  vars(Variable), scale = "free") +
  guides(color = "none")
