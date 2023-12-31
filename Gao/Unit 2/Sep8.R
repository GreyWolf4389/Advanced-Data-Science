library(tidyverse)
library(datasets)
data()

irises <- as_tibble(iris)
irises

Chicks <- as_tibble(ChickWeight)
Chick <- rename(Chicks, Weight = weight)
Chicks

Chicks %>% filter(, Diet == 1)
irises %>% filter(Petal.Length > 5)
irises %>% filter(, Species == "virginica")

ggplot(data = Chick, aes(x=Time, y=Weight, color = Chick)) + geom_line()

ggplot(data = irises, aes(x=Sepal.Length, y=Sepal.Width, color = Species)) + geom_point(size = 1.5)

ggplot(data = irises, aes(x=Sepal.Length, y=Sepal.Width, color = Species, size = Sepal.Length*Sepal.Width)) + geom_point()

ggplot(irises, aes(x=Species, fill=Species)) + geom_bar()

ggplot(filter(irises,Sepal.Length > 5.5), aes(x=Species, fill=Species)) + geom_bar()

ggplot(irises, aes(x=Species, y=Sepal.Width,color=Species)) + geom_boxplot()

ggplot(irises, aes(x=Sepal.Length,fill=Species)) + geom_histogram()
ggplot(irises, aes(x=Sepal.Length,fill=Species)) + geom_histogram(bins = 20)

ggplot(irises, aes(x=Sepal.Length, color=Species)) + geom_density(size=1)
ggplot(irises, aes(x=Sepal.Length, color=Species)) + geom_density(linewidth=1)
ggplot(irises, aes(x=Sepal.Length, color=Species)) + geom_density(position="stack")

ggplot(irises, aes(x=Sepal.Length, color=Species)) + stat_ecdf(size=1)

ggplot(irises, aes(x=Sepal.Length, y=Sepal.Width, color=Species)) + geom_density2d(size=1)
ggplot(irises, aes(x=Sepal.Length, y=Sepal.Width, color=Species)) + geom_density2d(size=1,bins=8)
ggplot(irises, aes(x=Sepal.Length, y=Sepal.Width, color=Species)) + geom_density2d(size=1,bins=15) + geom_point(size=1)
ggplot(irises, aes(x=Sepal.Length, y=Sepal.Width, color=Species)) + geom_line() + geom_point()
ggplot(Chick, aes(x=Time, y=Weight, color=Chick)) + geom_line() + geom_point()

ggplot(Chick, aes(x=Time, y=Weight,color=Chick)) + geom_line(aes(x=period())) + facet_wrap(facets=vars(Diet),nrow=5,ncol=2)

