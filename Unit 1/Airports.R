setwd("Documents/murach_r/data")
getwd()

airport_data <- read_csv("tsa_claims1.csv")
airport_data <- airport_data %>% rename("ClaimNumber" = "Claim Number")
airport_data <- airport_data %>% rename("DateReceived" = "Date Received")
airport_data <- airport_data %>% rename("IncidentDate" = "Incident Date")
airport_data <- airport_data %>% rename("AirportCode" = "Airport Code")
airport_data <- airport_data %>% rename("AirportName" = "Airport Name")
airport_data <- airport_data %>% rename("AirlineName" = "Airline Name")
airport_data <- airport_data %>% rename("ClaimType" = "Claim Type")
airport_data <- airport_data %>% rename("ClaimSite" = "Claim Site")
airport_data <- airport_data %>% rename("ClaimAmount" = "Claim Amount")
airport_data <- airport_data %>% rename("CloseAmount" = "Close Amount")

#What is the most common type of insurance claim?

#Q1:What is the most common type of insurance claim?

airport_data_insurance = airport_data$ClaimType
count <- 0
count1 <- 0
for (claim in airport_data_insurance)
{
  if(claim == "Passenger Property Loss"){
    count <-  count + 1
  }
  else if (claim != "Passenger Property Loss"){
    count1 <- count1 + 1
  }
}

if(count > count1){
  print("Passenger Property Loss is the most common insurance claim")
  count
}
if(count1 > count){
  print("Property Damage is the most common insurance claim")
  count1
}

#Which claim site within the airport are claims most commonly filed for?

summarise(group_by(airport_data, ClaimSite), ClaimSiteTotal = n())

#What type of claim is made most at each claim site?

summarise(group_by(airport_data, ClaimSite, ClaimType), SiteTypeTotal = n())

#What is the median claim amount?
#Fun fact! 1st Qu is Grandpa Qu, 3rd Qu is Lucas! Lucas's dad isn't mean though!

summary(select(airport_data, ClaimAmount))

#What is the overall claim approval rate for the entire U.S.?

summarise(group_by(airport_data, Status), StatusTotal = n())
paste0(round(23167/(21367+6+53273+1+18401) * 100, 2), "%")

