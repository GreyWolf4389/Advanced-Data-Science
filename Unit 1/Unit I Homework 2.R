#Advanced Data Science
#Unit I - Homework 2

#*Please use your creativity when deciding which qualitative (categorical)
#*and quantitative (numerical) variables that you would like to use for
#*this assignment.

#Answer each of the following questions by supplying the appropriate R code.

#Q1

#*Define four different categorical variables and assign each variable
#*one attribute (value). Choose categorical variables that are related
#*to each other or are able to be used in a sentence together.

Carnivore = "Meat"
MeatFlavor = "good"
Herbivore = "plants"
PlantFlavor = "bad"

#Q2

#Write a sentence using the variables in question 1, use the paste() command.

paste(Carnivore, "tastes", MeatFlavor, "but", Herbivore, "tastes", PlantFlavor)

#Q3

#*Redefine one of the variables in questions 1 and 2, using an
#*appropriate function when working with strings, and print the revised
#*sentence from question 2 with the new word/phrase.

Carnivore = "Dog"
paste(Carnivore, "tastes", MeatFlavor, "but", Herbivore, "tastes", PlantFlavor)

#Q4

#*Choose one variable in questions 1 – 3 and convert the value to
#*uppercase. Print the revised sentence from question 3 with this change
#*in attributes.

Carnivore = "DOG"
paste(Carnivore, "tastes", MeatFlavor, "but", Herbivore, "tastes", PlantFlavor)

#Q5

#Using the appropriate R code, calculate log(25).

log25 <- log10(25)
log25

#Q6

#Using the appropriate R code, round 12.398675 correctly to 3 decimal places.

MathStuff <- round(12.398675, 3)
MathStuff

#Q7

#Using the appropriate R code, calculate ln(411).

NatLog <- log(411)
NatLog

#Q8

#Create four vectors for a data frame. The variables are listed below.
#I have provided the values of the first vector for you.
#For the other three vectors, assign either categorical or numerical
#values to each of the individuals listed. Such as age, hair color,
#number of pets, number of siblings, height, etc.

data.science <- c("Ben", "Gao", "Jadyn", "Grace", "Rahul", "Senming",
                    "Lucas", "Andrew", "Lincoln", "Varun")
calc <- c("BC","Multi","AB","AB","BC","Pre-calc","Multi", "BC","AB","AB")
height <- c(5.6, 5.6, 5.6, 5.6, 5.6, 5.6, 12, 5.6, 5.6, 5.6)
grass <- c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L)

#Q9

#Create a data frame using your vectors.
#Print the data frame.

class <- data.frame(Name = data.science, Calculus = calc, Height = height, Grass = grass)
class

#Q10

#Print the data in rows 5 - 7 and columns 2 - 3 of your data frame.

class
class[5:7,2:3]

#Q11

#Add "Worcester" to the data.science vector and assign appropriate
#values for the other three variables.

class[11,1] <- "Worcester" 
class[11,2] <- "Math 55"
class[11,3] <- 12
class[11,4] <- 100L

#Q12

#Print your new data frame with the added row.

print(class)
