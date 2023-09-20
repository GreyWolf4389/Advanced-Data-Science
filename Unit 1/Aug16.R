NumOfWeek = c(1,2,3,4,5,6,7)
NumOfWeek

DaysOfWeek = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
DaysOfWeek

P1Tardies <- seq(1:5)
P1Tardies

P2Tardies <- 1:5
P2Tardies

Miexed <- c(1,2,3, "A", "B")

DaysOfWeek[1]
DaysOfWeek[-1]
DaysOfWeek[2:4]
DaysOfWeek[c(4,5,6,7)]

length(DaysOfWeek)

NFLPassYards <- c("Herbert" = 4000, "Goff" = 3200, "Mahomes" = 4500, "Brady" = 2725, "Manning" = 3562)
NFLPassYards
names(NFLPassYards)
NFLPassYards[3]
NFLPassYards["Mahomes"]

Months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
NumDays <- c(31,28,31,30,31,30,31,31,30,31,30,31)
Seasons <- c("W","W","Sp","Sp","Sp","Su","Su","Su","F","F","F","W")

Calendar <- data.frame(months = Months, days = NumDays, season = Seasons)
Calendar[["months"]]
Calendar[[1]]
Calendar[2:5,2:3]
Calendar[2,2] <- 29
Calendar