library(ggforce)
library(RJSONIO)
library(tidyverse)

setwd("Documents/murach_r/data")
getwd()

url = "https://www.murach.com/python_analysis/shots.json"
dest_file <- "shots.json"

download.file(url, dest_file)

json_data <- fromJSON("shots.json")

column_names <- json_data[["resultSets"]][[1]][["headers"]]

rows <- json_data[["resultSets"]][[1]][["rowSet"]]

shots <- data.frame()
for (row in rows) { 
  shots <- rbind(shots, row)
}
names(shots) <- column_names

shots <- as_tibble(shots)
