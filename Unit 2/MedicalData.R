setwd('..')
setwd("Documents/murach_r/data")
getwd()

medical_data <- read_csv("insurance.csv")

#Display the summary statistics for the age and charges columns. What do you notice about how the data is distributed?

summary(select(medical_data, age, charges))

#The dataset covers only adults, no children and seniors. Healthcare is also very expensive.

#How many people were sampled from each region?

medical_data %>% group_by(region) %>% summarise(PartientsPerRegion = n())

#How are average medical charges related to age? Use a line plot to answer this question.

AverageCharge <- medical_data %>% group_by(age) %>% summarize(AverageCharges = mean(charges))
ggplot() + geom_line(data = AverageCharge, aes(x = age,y = AverageCharges))

#As age increases, so do medical costs

#How are average medical charges related to the number of children? Use a line plot to answer this question.

AverageCharge <- medical_data %>% group_by(children) %>% summarize(AverageCharges = mean(charges))
ggplot() + geom_line(data = AverageCharge, aes(x = children,y = AverageCharges))

#The more children you have, the lower the medical costs. It is advisable to have as many children as possible to defray healthcare costs.∑