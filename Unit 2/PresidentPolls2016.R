#it's joever

setwd('..')
setwd("Documents/murach_r/data")
getwd()
library(tidyverse)
library(datasets)

url <- "https://projects.fivethirtyeight.com/general-model/president_general_polls_2016.csv"

dest_file <- "polls.csv"

download.file(url, dest_file)

polls <- read.csv(dest_file)
polls

str(polls, strip.width = "cut")
polls %>% str(strip.width = "cut")

unique(polls$type)
unique(polls$grade)

polls$type %>% unique() %>% length()

polls %>% apply(MARGIN = 2, FUN = unique) %>% str(string.width = "cut")

polls %>% apply(MARGIN = 2, FUN = unique) %>% lapply(FUN = length) %>% str(string.width = "cut")

polls$state %>% table()

(table(polls$state) / sum(table(polls$state)) * 100) %>% round(3)

table(polls[c("state","grade")])

polls %>% arrange()
