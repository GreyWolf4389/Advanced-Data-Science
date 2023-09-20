library(tidyverse)
library(datasets)

swchars <- as_tibble(starwars)
swchars

str(swchars, strict.width = "cut", max.level = 2)

#Q8
swchars %>% filter(duplicated(swchars))

#Q9
swchars <- select(swchars, -birth_year, -homeworld, -films, -vehicles, -starships)

#Q10 Q11
swchars <- swchars %>%
  rename_with(str_to_title)
swchars <- swchars %>%
  rename(hairColor = Hair_color,
         skinColor = Skin_color,
         eyeColor = Eye_color)

#Q12
swchars2 <- swchars %>% filter(!complete.cases(swchars))

swchars %>% apply(MARGIN = 2, function(col) sum(is.na(col)))

swchars %>% filter(is.na(Height))
swchars <- swchars %>% filter(Name != "Captain Phasma")

swchars %>% filter(!complete.cases(Mass))
swchars <- swchars %>% select(-Mass)

swchars %>% filter(!complete.cases(hairColor))
swchars <- swchars %>% mutate(hairColor =
                                replace_na(hairColor, "none"))

swchars %>% filter(!complete.cases(Sex))
swchars <- swchars %>% filter(complete.cases(Sex))

swchars %>% filter(!complete.cases(swchars))

unique(swchars$Species)

table(swchars$Species)

swchars <- swchars %>% mutate(Species = ifelse(Species == "Human" | Species == "Droid", Species, "Alien"))

swchars %>%
  filter(!(Sex == "female" & Gender == "feminine") &
           !(Sex == "male" & Gender == "masculine")) %>%
  select(Name, Sex, Gender)

swchars <- swchars %>% select(-Sex)
swchars

table(swchars$eyeColor)
  
apply(X = swchars, MARGIN = 2, FUN = unique) %>%
  lapply(FUN = length) %>%
  str()
swchars <- swchars %>% mutate(Gender = factor(Gender), Species = factor(Species))
swchars
summary(swchars)
ggplot(swchars, aes(x = Species, y = Height, color = Species)) + geom_boxplot()
view(swchars)
