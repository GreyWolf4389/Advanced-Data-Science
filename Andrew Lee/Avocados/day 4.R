install.packages("tidyverse")
library(gridExtra)
install.packages("gridExtra")
install.packages("reshape2")
library(tidyverse)
library()
getwd()
Avocado <- read_csv("Avocado.csv")

Avocado <- as_tibble(Avocado)

colnames(Avocado)[2] = "Weight_0"
colnames(Avocado)[5] = "Weight_1"
colnames(Avocado)[3] = "Volume_0"
colnames(Avocado)[6] = "Volume_1"
colnames(Avocado)[4] = "Density_0"
colnames(Avocado)[7] = "Density_1"
colnames(Avocado)[1] = "Avocado_number"
colnames(Avocado)[8] = "Weight_2"
colnames(Avocado)[9] = "Volume_2"
colnames(Avocado)[10] = "Density_2"
colnames(Avocado)[11] = "Weight_3"
colnames(Avocado)[12] = "Volume_3"
colnames(Avocado)[13] = "Density_3"
colnames(Avocado)[14] = "Weight_4"
colnames(Avocado)[15] = "Volume_4"
colnames(Avocado)[16] = "Density_4"



avocado_long <- Avocado %>%
  pivot_longer(cols = starts_with("Density_"), 
               names_to = c(".value", "Day"),
               names_sep = "_") 


y_breaks <- seq(1, max(avocado_long$Density), by = 0.5)
y_labels <- y_breaks

ggplot(avocado_long, aes(x = as.numeric(Day) + 1, y = Density, group = Avocado_number, color = as.factor(Avocado_number))) +
  geom_point() +  # Scatter plot
  geom_line() +   # Connecting lines
  labs(x = "Day", y = "Density", title = "Density Trend for 30 Avocados with Connecting Lines") +
  theme_minimal() +
  scale_x_continuous(breaks = 1:5, labels = paste("Day", 0:4)) +
  coord_cartesian(ylim = NULL)


