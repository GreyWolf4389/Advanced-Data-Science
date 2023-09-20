setwd("Documents/murach_r/data")
getwd()

install.packages("tidyverse")
mortality_data <- read_excel("child_mortality_rates.xlsx")
mortality_data

summary(select(mortality_data, -Year))

mortality_long <- pivot_longer(mortality_data, cols = c("01-04 Years", "05-09 Years", "10-14 Years", "15-19 Years"), names_to = "AgeGroup",values_to = "DeathRate")
mortality_long
mortality_long <- mutate(mortality_long, DeathRate = DeathRate * 100000)
mortality_long
mortality_long <- mutate(mortality_long, Decade = (Year %/% 10) *10)
mortality_long
mortality_long <- rename(mortality_long, DeathsPer100k = DeathRate)
mortality_long
mortality_long %>% group_by(Decade) %>% summarize(MeanDeaths = mean(DeathsPer100k), Count = n())

ggplot(mortality_long, aes(x = Year, y= DeathsPer100k, color = AgeGroup))
ggplot(mortality_long, aes(x = Year, y= DeathsPer100k, color = AgeGroup)) + geom_line()
ggplot() + geom_line(data = mortality_long,
                     aes(x=Year, y=DeathsPer100k, color = AgeGroup)) + labs(y="Deaths Per 100k") + guides(color = guide_legend)