library("tidyverse")

diamonds

#Rename columns to use title case

names(diamonds) <- names(diamonds) %>% str_to_title()

#Check for NA values

apply(X = diamonds, MARGIN = 2, FUN = function(col) 
  sum(is.na(col)))

#Drop rows that have invalid values

diamonds <- diamonds %>% filter(X != 0 & Y != 0 & Z != 0)

# KDE plots with outlier fences

get_upper_fence <- function(x) {
  quantile(x, 0.75) + (1.5 * IQR(x))
}

get_lower_fence <- function(x) {
  quantile(x, 0.25) - (1.5 * IQR(x))
}

diamonds_long <- diamonds %>% 
  select_if(is.numeric) %>% 
  pivot_longer(cols = names(select_if(diamonds, is.numeric)), 
               names_to = "Variable",
               values_to = "Value") %>% 
  group_by(Variable) %>% 
  mutate(UpperFence = get_upper_fence(Value),
         LowerFence = get_lower_fence(Value))

ggplot(data = diamonds_long, aes(x = Value)) +
  geom_density(aes(color = Variable)) + 
  geom_vline(aes(xintercept = UpperFence), color = "red") +
  geom_vline(aes(xintercept = LowerFence), color = "red") +
  facet_wrap(facets = vars(Variable), scale = "free") + 
  guides(color = "none")

# the r-value for two specific variables
cor(x = diamonds$Price, y = diamonds$Carat)

# the r-values for Price vs all numeric variables 
cor(diamonds$Price, select_if(diamonds, is.numeric))

# the r-values for all combinations of numeric variables
# rounded to 2 decimal places for easier reading
select_if(diamonds, is.numeric) %>% cor() %>% round(2)

# Carat and Price have a strong positive correlation
ggplot(diamonds, aes(x = Carat, y = Price)) +
  geom_point()

# Z and Price have a strong positive correlation with
# a slight curve
ggplot(diamonds, aes(x = Z, y = Price)) +
  geom_point() +
  xlim(2.25, 6)

# the above plot with outliers included (not shown in chapter)
ggplot(diamonds, aes(x = Z, y = Price)) +
  geom_point()

# a negligible correlation
ggplot(diamonds, aes(x = Depth, y = Price)) +
  geom_point()

# the same plot, but with the alpha adjusted to account
# for overplotting
# (not shown in chapter)
ggplot(diamonds, aes(x = Depth, y = Price)) +
  geom_point(alpha = .1)

# the same plot, but now with a limited sample size
ggplot(sample_n(diamonds, 500), 
       aes(x = Depth, y = Price)) +
  geom_point()

##################################
# How to create a model that uses a straight line
##################################

# how to split the data

library("tidymodels")

diamonds_split <- initial_split(diamonds, prop = 0.75)
train <- training(diamonds_split)
test <- testing(diamonds_split)

train

test

# custom functions for getting the upper and lower fences
get_upper_fence <- function(x) {
  quantile(x, 0.75) + (1.5 * IQR(x))
}

get_lower_fence <- function(x) {
  quantile(x, 0.25) - (1.5 * IQR(x))
}

# returns only rows with values between the fences
# uses . in all_vars() to refer to the current column
train <- train %>% 
  filter_at(vars(Carat, Depth, Table, X, Y, Z), 
            all_vars(. > get_lower_fence(.) &
                       . < get_upper_fence(.)))

# view the filtered training data set
train

# plot the filtered training set to look for any more outliers
ggplot(data = train) +
  geom_point(aes(x = Z, y = Price))

# drop some remaining outliers that were identified visually
train <- train %>% filter(!Z < 2.25)

# plot the set again to confirm outliers are removed
ggplot(data = train) +
  geom_point(aes(x = Z, y = Price))

# two ways to create an empty linear model object
linear_reg()
linear_reg(engine = "lm")

# using linear_reg() and fit() to create a linear 
# regression model
model <- fit(object = linear_reg(), formula = Price ~ Z, 
             data = train)
model

# using lm() to create the same linear regression model
model <- lm(formula = Price ~ Z, data = train)
model

# view summary statistics of fitted model

summary(model)

# view more information about the model's regression terms
model %>% tidy()

# how to create and plot predictions 
model <- fit(object = linear_reg(), formula = Price ~ Z, 
             data = train)
predict(model, new_data = test)

model_results <- test %>% 
  mutate(predict(model, new_data = test))

ggplot(data = model_results) + 
  geom_point(aes(x = Z, y = Price)) + 
  geom_point(aes(x = Z, y = .pred), color = "red")

##################################
# How to work with formulas
##################################

# a simple custom function
cube_x <- function(x){
  return(x^3)
}

# plotting the function
ggplot() +
  geom_function(fun = cube_x) +
  xlim(0, 10)

# using a lambda expression to generate the same plot
ggplot() +
  geom_function(fun = function(x) x^3) +
  xlim(0, 10)

# how to plot a straight line
ggplot(data = test) + 
  geom_point(aes(x = Z, y = Price)) +
  geom_function(fun = function(x) 4642 * x - 12617, 
                color="red", linewidth = 2)

# how to plot a curved line
ggplot(data = train) + 
  geom_point(aes(x = Z, y = Price)) +
  geom_function(fun = function(x) x,   color="red") +
  geom_function(fun = function(x) x^2, color="orange") + 
  geom_function(fun = function(x) x^3, color="yellow") + 
  geom_function(fun = function(x) x^4, color="green") + 
  geom_function(fun = function(x) x^5, color="blue") + 
  geom_function(fun = function(x) x^6, color="purple", 
                linewidth = 2) +
  geom_function(fun = function(x) x^7, color="violet") +
  ylim(0, 20000)

# a straight line, Price ~ Z (not shown in chapter)
ggplot(data = train, aes(x = Z, y = Price)) + 
  geom_point() +
  geom_smooth(method = lm, formula = y ~ x, 
              color="red", linewidth = 2)

# using Price ~ Z^6
ggplot(data = train, aes(x = Z, y = Price)) + 
  geom_point() +
  geom_smooth(method = lm, formula = y ~ I(x^6), 
              color="purple", size = 2)

# fit the model to Price ~ I(Z^6)
model_exp <- fit(object = linear_reg(), 
                 formula = Price ~ I(Z^6), 
                 data = train)

# view the new model
model_exp %>% tidy()

# view the predictions
predict(model_exp, new_data = test)

# add the predictions to the test data
model_exp_results <- test %>% mutate(predict(model_exp,
                                             new_data = test))

# plot the predictions
ggplot(data = model_exp_results) + 
  geom_point(aes(x = Z, y = Price)) + 
  geom_point(aes(x = Z, y = .pred), color = "red")

# zoom in (not shown in chapter)
ggplot(data = model_exp_results) + 
  geom_point(aes(x = Z, y = Price)) + 
  geom_point(aes(x = Z, y = .pred), color = "red") +
  ylim(0, 20000)



