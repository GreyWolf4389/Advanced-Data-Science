# before running this script for the first time, you should install this package:
# install.packages("datasets")

# load the packages for this chapter
library("tidyverse")    # the core tidyverse packages (tibble, ggplot2, etc.)
library("datasets")     # the sample datasets (iris, ChickWeight, etc.)

# get irises data and format it as a tibble
irises <- as_tibble(iris)
irises

# get chicks data and format it as a tibble
chicks <- as_tibble(ChickWeight)
chicks

# fix capitalization of first column
chicks <- rename(chicks, Weight = weight)

# display a line plot for all chicks
ggplot(chicks, aes(x = Time, y = Weight, color = Chick)) +
  geom_line()

# display a scatter plot of iris sepal length to width and set the size of the points to 3
ggplot(irises, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point(size = 3)

ggplot(irises, aes(x = Petal.Length, y = Petal.Width, color = Species)) +
  geom_point(size = 3)

# display a scatter plot and set the size of the points to a calculation
ggplot(irises, aes(x = Sepal.Length, y = Sepal.Width,
                   color = Species, size = Sepal.Length * Sepal.Width)) +
  geom_point()

# display a bar plot
ggplot(irises, aes(x = Species, fill = Species)) +
  geom_bar()

# display a bar plot for only irises with sepal lengths greater than 5.5
ggplot(filter(irises, Sepal.Length > 5.5), 
       aes(x = Species, fill = Species)) +
  geom_bar()

ggplot(filter(irises, Sepal.Length < 5.5), 
       aes(x = Species)) +
  geom_bar()

# display a box plot for iris sepal width
ggplot(irises, aes(x = Species, y = Sepal.Width, color = Species)) +
  geom_boxplot()

# display a histogram
ggplot(irises, aes(x = Sepal.Length, fill = Species)) +
  geom_histogram()

# display a histogram with 20 bins
ggplot(irises, aes(x = Sepal.Length, fill = Species)) +
  geom_histogram(bins = 20)

ggplot(irises, aes(x = Sepal.Length, fill = Species)) +
  geom_histogram(bins = 5)

# display a KDE plot
ggplot(irises, aes(x = Sepal.Length, color = Species)) +
  geom_density(size = 1)

# display a stacked KDE plot
ggplot(irises, aes(x = Sepal.Length, color = Species, fill = Species)) +
  geom_density (position = "stack")

# display an ECDF plot
ggplot(irises, aes(x = Sepal.Length, color = Species)) +
  stat_ecdf(size = 1)

# display an ECDF plot with all the values combined into one line
ggplot(irises, aes(x = Sepal.Length)) +
  stat_ecdf(size = 1)

# display a 2D density plot
ggplot(irises, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_density_2d(size = 1)

# display a 2D density plot and set the bins parameter
ggplot(irises, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_density_2d(size = 1, bins = 15)

# combine two plots
ggplot(irises, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_density_2d(bins = 15, size = 1) + 
  geom_point(size = 3)

ggplot(filter(irises, Species == 'versicolor'), aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_density_2d(bins = 15, size = 1) + 
  geom_point(size = 3)

# combine a line plot with a scatter plot
ggplot(data = chicks, aes(x = Time, y = Weight, color = Chick)) +
  geom_line() +
  geom_point()

# create a grid based on a categorical column
ggplot(chicks, 
       aes(x = Time, y = Weight, color = Chick)) +
  geom_line() +
  facet_wrap(facets = vars(Diet), nrow = 2, ncol = 2)

ggplot(chicks, 
       aes(x = Time, y = Weight, color = Chick)) +
  geom_line() +
  facet_wrap(facets = vars(Diet), nrow = 4, ncol = 1)

#Question 8
ggplot(data = filter(chicks, Diet == 1), aes(x = Time, y = Weight, color = Chick)) +
  geom_line() +
  geom_point()

ggplot(data = filter(chicks, Diet == 2), aes(x = Time, y = Weight, color = Chick)) +
  geom_line() +
  geom_point()

ggplot(data = filter(chicks, Diet == 3), aes(x = Time, y = Weight, color = Chick)) +
  geom_line() +
  geom_point()

ggplot(data = filter(chicks, Diet == 4), aes(x = Time, y = Weight, color = Chick)) +
  geom_line() +
  geom_point()
