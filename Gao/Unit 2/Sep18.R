library(tidyverse)
library(datasets)

swchars <- as_tibble(starwars)
swchars

?starwars

swchars %>% str(strict.width = "cut", max.level = 2)

swchars %>% filter(duplicated(swchars))

swchars <- swchars %>% select(-birth_year, -homeworld, -films, -vehicles, -starships)

swchars <- swchars %>% rename_with(str_to_title)
swchars <- swchars %>% rename(HairColor = Hair_color, SkinColor = Skin_color, EyeColor = Eye_color)
swchars

swchars %>% filter(!complete.cases(swchars))

swchars %>% print(n = 34)

swchars2 <- swchars %>% filter(!complete.cases(swchars))
swchars2
swchars2 %>% print(n = 34)

swchars %>% apply(MARGIN = 2, function(col) sum(is.na(col)))

swchars %>% filter(is.na(Height))

swchars <- swchars %>% filter(Name != "Captain Phasma")
swchars %>% print(n=87)

swchars %>% filter(!complete.cases(Mass))
swchars <- swchars %>% select(-Mass)

swchars %>% filter(!complete.cases(HairColor))
swchars <- swchars %>% mutate(HairColor = replace_na(HairColor, "none"))

swchars %>% filter(!complete.cases(Sex))
swchars <- swchars %>% filter(complete.cases(Sex))

swchars %>% filter(!complete.cases(swchars))

unique(swchars$Species)

table(swchars$Species)

swchars <- swchars %>% mutate(Species = ifelse(Species == "Human" | Species == "Droid", Species, "Alien"))

unique(swchars$Sex)
unique(swchars$Gender)

swchars %>% filter(!(Sex == "female" & Gender == "female") & !(Sex == "male" & Gender == "masculine")) %>% select(Name, Sex, Gender)

swchars <- swchars %>% select(-Sex)

table(swchars$EyeColor)

apply(X = swchars, MARGIN = 2, FUN = unique) %>% lapply(FUN = length) %>% str()

swchars <- swchars %>% mutate(Gender = factor(Gender), Species = factor(Species))
swchars

summary(swchars)

ggplot(swchars, aes(x=Species, y=Height, color=Species)) + geom_boxplot()