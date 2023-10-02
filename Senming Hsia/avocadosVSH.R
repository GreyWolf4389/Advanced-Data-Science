install.packages("tidyverse")
Avocado <- read_csv("Avocado.csv")
install.packages("plotly")

library("tidyverse")
library("plotly")


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
colnames(Avocado)[17] = "Weight_5"
colnames(Avocado)[18] = "Volume_5"
colnames(Avocado)[19] = "Density_5"



#Graph for density vs days, no regression

avocado_long <- Avocado %>%
  pivot_longer(cols = starts_with("Density_"), 
               names_to = c(".value", "Day"),
               names_sep = "_") 



y_breaks <- seq(1, max(avocado_long$Density), by = 0.5)
y_labels <- y_breaks

lmao <- ggplot(avocado_long, aes(x = as.numeric(Day), y = Density, group = Avocado_number, color = as.factor(Avocado_number))) +
  geom_point() + 
  geom_line() +   
  labs(x = "Day", y = "Density", title = "Density Trend for 30 Avocados with Connecting Lines") +
  theme_minimal() +
  scale_x_continuous(breaks = c(0, 1:5), labels = paste("Day", c(0, 1:5))) +
  coord_cartesian(ylim = NULL)

print(lmao)




#Linear regression for density vs days


avocado_long <- avocado_long %>%
  mutate(Day = as.numeric(Day))


predicted_data <- avocado_long %>%
  group_by(Avocado_number) %>%
  do({
    model <- lm(Density ~ Day, data = .)
    next_days <- data.frame(Day = seq(max(.$Day), max(.$Day) + 7, by = 1))
    predicted_density <- predict(model, newdata = next_days)
    data.frame(next_days, predicted_density)
  }) %>%
  ungroup()

ggplot(avocado_long, aes(x = Day, y = Density, group = Avocado_number, color = as.factor(Avocado_number))) +
  geom_point() +  
  geom_line() +   
  geom_point(data = predicted_data, aes(x = Day, y = predicted_density), color = "red", size = 3) +  # Predicted data points
  labs(x = "Day", y = "Density", title = "Density Trend for Avocados with Linear Regression") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1, max(avocado_long$Day) + 8, 1), labels = paste("Day", seq(1, max(avocado_long$Day) + 8, 1))) +
  coord_cartesian(ylim = NULL)










#Graphing weight vs days, no linear regression


avocado_weight <- Avocado %>%
  pivot_longer(cols = starts_with("Weight_"), 
               names_to = c(".value", "Day"),
               names_sep = "_") 


y_breaks <- seq(1, max(avocado_weight$Weight), by = 0.5)
y_labels <- y_breaks

lol <- ggplot(avocado_weight, aes(x = as.numeric(Day), y = Weight, group = Avocado_number, color = as.factor(Avocado_number))) +
  geom_point() + 
  geom_line() +   
  labs(x = "Day", y = "Weight", title = "Weight Trend for 30 Avocados with Connecting Lines") +
  theme_minimal() +
  scale_x_continuous(breaks = c(0, 1:5), labels = paste("Day", c(0, 1:5))) +
  coord_cartesian(ylim = NULL)

print(lol)





#Graph for Weight vs days, with regression

avocado_weight <- avocado_weight %>%
  mutate(Day = as.numeric(Day))


predicted_data <- avocado_weight %>%
  group_by(Avocado_number) %>%
  do({
    model <- lm(Weight ~ Day, data = .)
    next_days <- data.frame(Day = seq(max(.$Day), max(.$Day) + 10, by = 1))
    predicted_weight <- predict(model, newdata = next_days)
    data.frame(next_days, predicted_weight)
  }) %>%
  ungroup()

ggplot(avocado_weight, aes(x = Day, y = Weight, group = Avocado_number, color = as.factor(Avocado_number))) +
  geom_point() +  
  geom_line() +   
  geom_point(data = predicted_data, aes(x = Day, y = predicted_weight), color = "red", size = 3) +  # Predicted data points
  labs(x = "Day", y = "Weight", title = "Weight Trend for Avocados with Linear Regression") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1, max(avocado_weight$Day) + 10, 1), labels = paste("Day", seq(1, max(avocado_weight$Day) + 10, 1))) +
  coord_cartesian(ylim = NULL)





####################################

#R squared values for data Density vs Days (must run separately from weight)


r_squared_values <- numeric(length(unique(avocado_long$Avocado_number)))

# Loop through each avocado
for (i in unique(avocado_long$Avocado_number)) {
  avocado_data <- avocado_long %>% filter(Avocado_number == i)
  
  model <- lm(Density ~ as.numeric(Day), data = avocado_data)
  
  r_squared_values[i] <- summary(model)$r.squared
}

r_squared_data <- data.frame(Avocado_number = unique(avocado_long$Avocado_number), R_squared = r_squared_values)
print(r_squared_data)


average_r_squared <- mean(r_squared_values, na.rm = TRUE)

cat("Average R-squared value:", average_r_squared, "\n")

#R squared values for data Weight vs Days



r_squared_values <- numeric(length(unique(avocado_weight$Avocado_number)))

# Loop through each avocado
for (i in unique(avocado_weight$Avocado_number)) {
  avocado_data <- avocado_weight %>% filter(Avocado_number == i)
  
  model <- lm(Weight ~ as.numeric(Day), data = avocado_data)
  
  r_squared_values[i] <- summary(model)$r.squared
}

r_squared_data <- data.frame(Avocado_number = unique(avocado_weight$Avocado_number), R_squared = r_squared_values)
print(r_squared_data)


average_r_squared <- mean(r_squared_values, na.rm = TRUE)

cat("Average R-squared value:", average_r_squared, "\n")



####################################


#R  values for data Weight vs Days 

r_values <- numeric(length(unique(avocado_weight$Avocado_number)))

# Loop through each avocado
for (i in unique(avocado_weight$Avocado_number)) {
  avocado_data <- avocado_weight %>% filter(Avocado_number == i)
  
  r_value <- cor(avocado_data$Day, avocado_data$Weight)
  
  r_values[i] <- r_value
}

r_data_W <- data.frame(Avocado_number = unique(avocado_weight$Avocado_number), r_value = r_values)

#Average R value for weight

cumulative_val_weight = 0
for(r_val in r_data_W$r_value) {
  cumulative_val_weight = cumulative_val_weight + r_val
}
avg_r_weight_val = cumulative_val_weight/30
avg_r_weight_val


print(r_data)


#R  values for data density vs Days 

r_values <- numeric(length(unique(avocado_long$Avocado_number)))

# Loop through each avocado
for (i in unique(avocado_long$Avocado_number)) {
  avocado_data <- avocado_long %>% filter(Avocado_number == i)
  
  r_value <- cor(avocado_data$Day, avocado_data$Density)
  
  r_values[i] <- r_value
}

r_data_D <- data.frame(Avocado_number = unique(avocado_long$Avocado_number), r_value = r_values)

#Average R value for density

cumulative_val = 0
for(r_val in r_data_D$r_value) {
  cumulative_val = cumulative_val + r_val
}
avg_r_density_val = cumulative_val/30
avg_r_density_val

print(r_data)




###############

#Average change in weight 


avocado_weight <- avocado_weight %>%
  mutate(Day = as.numeric(Day))

average_change_per_avocado <- avocado_weight %>%
  group_by(Avocado_number) %>%
  summarize(average_change = (last(Weight) - first(Weight)) / (last(Day) - first(Day)))

overall_average_change <- mean(average_change_per_avocado$average_change)

print(overall_average_change)


#Average change in density


avocado_long <- avocado_long %>%
  mutate(Day = as.numeric(Day))


average_change_per_avocado <- avocado_long %>%
  group_by(Avocado_number) %>%
  summarise(average_change = (last(Density) - first(Density)) / (last(Day) - first(Day)))

overall_average_change <- mean(average_change_per_avocado$average_change)

print(overall_average_change)


#############
average_r_change <- mean(overall_average_change$Avocado_number)


#Volume plot

avocado_long_V <- Avocado %>%
  pivot_longer(cols = starts_with("Volume_"), 
               names_to = c(".value", "Day"),
               names_sep = "_") 

y_breaks <- seq(1, max(avocado_long_V$Volume), by = 0.5)
y_labels <- y_breaks

nice <- ggplot(avocado_long_V, aes(x = as.numeric(Day), y = Volume, group = Avocado_number, color = as.factor(Avocado_number))) +
  geom_point() + 
  geom_line() +   
  labs(x = "Day", y = "Volume", title = "Volume Trend for 30 Avocados with Connecting Lines") +
  theme_minimal() +
  scale_x_continuous(breaks = c(0, 1:5), labels = paste("Day", c(0, 1:5))) +
  coord_cartesian(ylim = NULL)

print(nice)

plotly_gg <-ggplotly(lmao)

htmlwidgets::saveWidget(plotly_gg, "plotly_plot.html")