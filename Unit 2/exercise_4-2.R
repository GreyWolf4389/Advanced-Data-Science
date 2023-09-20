library(tidyverse)
library(datasets)

swchars <- as_tibble(starwars)
swchars

#Q6

ggplot(data = swchars, aes(x=species, fill=species)) + geom_bar()

#Q7

ggplot(data = swchars, aes(x=gender, fill=gender)) + geom_bar()

#Q8

ggplot(data = swchars, aes(x=height, y=mass, color=species)) + geom_point()

#Q9



#Q10

ggplot(data = filter(swchars, mass < 1000), aes(x=height, y=mass, color=gender)) + geom_point()

#Q11



#Q12

ggplot(data = filter(swchars, mass < 1000), aes(x=height, y=mass)) + geom_line() + facet_wrap(facets=vars(gender),nrow=1,ncol=3)
