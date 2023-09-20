setwd('..')
setwd("Documents/murach_r/data")
getwd()

#Rename column old_price

laptop_data <- read_csv("laptops.csv")
laptop_data <- laptop_data %>% rename("ListPrice" = "old_price")

#Least and most expensive

attach(laptop_data)
laptop_data <- laptop_data[order(ListPrice),]
tail(laptop_data, n = 3)

#Cheapest: Aspire 1 A114-31
#Expensive: MacBook Pro (Retina + Touch Bar)

#What are the minimum, maximum and average display sizes for laptops

laptop_data$display_size %>% fivenum()
laptop_data$display_size %>% mean()

#The Minimum size is 12.0 inches, the max is 18.4 inches and the average is 14.58

#In this data set, how many laptops does each brand have?

laptop_data %>% group_by(brand) %>% summarise(LaptopsPerBrand = n())

#What is the average discount price for each brand?

laptop_data %>% group_by(brand) %>% summarise(AverageDiscountPrice = mean(discount_price))
