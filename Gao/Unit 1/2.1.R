#Q1
#Q2

Vector <- c(1L,2L,3L,4L,5L)

#Q3

twos <-c(seq(2,10,2))

#Q4

threes <- c(seq(3,15,3))
fours <- c(seq(4, 20, 4))
fives <- c(seq(5, 25, 5))

#Q6

TimesTable <- data.frame("one" = Vector, "two" = twos, "three" = threes, "four" = fours, "five" = fives)

#Q7

TimesTable

#Q8

sixes <- c(seq(6,36,6))

#Q9

TimesTable <-  rbind(TimesTable, "six" = sixes)
TimesTable

#Q10

TimesTable <- cbind(TimesTable, sixes)
TimesTable

#Q11

TimesTable[[6]][[5]]
TimesTable[[6,5]]

#Q12,13

DivideAndRound <- function(x,y)
{
  result <- x/y
  result <- round(result, 2)
  print(result)
}

#Q14

DivideAndRound(8,3)
DivideAndRound(0,0)

#Q15

CaptainNames <- c("Lucas","Lee","MenSing")

#Q16

CrewCount <- c(7L, 3L, 4L)

#Q17 

Ships <- data.frame("Captains" = CaptainNames, "SailorCount" = CrewCount)

#Q18

Ships

#Q19

Ships[1]

#Q20

Ships$Captains

#Q21

GaoShip <- list("Gao", 6L)
Ships <- rbind(Ships, GaoShip)
Ships

#Q22,23,24,25,26,27,28,28


for (i in CrewCount) {
  if(i >= 5)
  {
    crew <- i
    print(paste(i, "is a large crew"))
  }
  else if (i <= 3)
  {
    crew <- i
    print(paste(i, "is a medium crew"))
  }
  else
  {
    crew <- i
    print(paste(i, "is a small crew"))
  }
}

