library("tidyverse")
#diamonds

# Rename columns to use title case

names(diamonds) <- names(diamonds) %>% str_to_title()

#Check for NAs

apply(X = diamonds, MARGIN = 2, FUN = function(col)
  sum(is.na(col)))

#Drop rows that have invalid values

diamonds <- diamonds %>% filter(X != 0 & Y != 0 & Z != 0)

#Plot data
ggplot(diamonds, aes(x = X, y = Price, fill = Clarity)) + 
         geom_boxplot()

#KDE plots with outlier fences

get_upper_fence <- function(x) {
  quantile(x, 0.75) + (1.5*IQR(x))
}

get_lower_fence <- function(x) {
  quantile(x, 0.25) - (1.5*IQR(x))
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

r_vals <- data.frame(X = c(1:100)) %>% as_tibble()

r_vals <- r_vals %>% rowwise() %>%
  mutate(A_PerfectPos = X,
         B_StrongPos = X + sample(-25:25, 1, replace = TRUE),
         C_ModeratePos = X + sample(-50:50, 1, replace = TRUE),
         D_WeakPos = X + sample(-125:125, 1, replace = TRUE),
         E_Negligible = sample(-50:50, 1, replace = TRUE),
         F_WeakNeg = -X + sample(-125:125, 1, replace = TRUE),
         G_ModerateNeg = -X + sample(-50:50, 1, replace = TRUE),
         H_StrongNeg = -X + sample(-25:25, 1, replace = TRUE),
         I_PerfectNeg = -X)


r_vals_long <- r_vals %>%
  pivot_longer(cols = names(select(r_vals, -1)),
               names_to = "Variable",
               values_to = "Value")
ggplot(data = r_vals_long) +
  geom_point(aes(x = X, y = Value)) +
  theme(axis.ticks = element_blank()) +
  facet_wrap(facets = vars(Variable), scales = "free")

cor(x = diamonds$Price, y = diamonds$Carat)

cor(diamonds$Price, select_if(diamonds, is.numeric))
select_if(diamonds, is.numeric) %>% cor() %>% round(3)

ggplot(diamonds, aes(x = Carat, y = Price)) +
  geom_point()

ggplot(diamonds, aes(x = Z, y = Price)) +
  geom_point() +
  xlim(2.25, 6)

ggplot(diamonds, aes(x = Z, y = Price)) +
  geom_point()

ggplot(diamonds, aes(x = Depth, y = Price)) +
  geom_point()

ggplot(diamonds, aes(x = Depth, y = Price)) +
  geom_point(alpha = 0.1)

ggplot(sample_n(diamonds, 500),
       aes(x = Depth, y = Price)) +
  geom_point()

library("tidymodels")

diamonds_split <- initial_split(diamonds, prop = 0.75)
train <- training(diamonds_split)
test <- testing(diamonds_split)
train
test

get_upper_fence <- function(x) {
  quantile(x, 0.75) + (1.5*IQR(x))
}

get_lower_fence <- function(x) {
  quantile(x, 0.25) - (1.5*IQR(x))
}
train <- train %>%
  filter_at(vars(Carat, Depth, Table, X, Y, Z),
            all_vars(. > get_lower_fence(.) &
                       . < get_upper_fence(.)))

train

ggplot(data = train) +
  geom_point(aes(x = Z, y = Price))

train <- train %>% filter(!Z < 2.25)

ggplot(data = train) +
  geom_point(aes(x = Z, y = Price))

#linear model
linear_reg()
linear_reg(engine = "lm")

model <- fit(object = linear_reg(), formula = Price ~ Z,
             data = train)
model

model <- lm(formula = Price ~ Z, data = train)
model

#regression info
summary(model)

model %>% tidy()

#model and plot predictions
model <- fit(object = linear_reg(), formula = Price ~ Z,
             data = train)
predict(model, new_data = test)

model_results <- test %>%
  mutate(predict(model, new_data = test))

ggplot(data = model_results) +
  geom_point(aes(x = Z, y = Price)) +
  geom_point(aes(x = Z, y = .pred), color = "red")

cube_x <- function(x){
  return(x^3)
}

ggplot() +
  geom_function(fun = cube_x) +
  xlim(0, 20)

ggplot(data = test) +
  geom_point(aes(x = Z, y = Price)) +
  geom_function(fun = function(x) 4642*x-12617, color = "red", size = 2)

ggplot(data = train) +
  geom_point(aes(x = Z, y = Price)) +
  geom_function(fun = function(x) x, color = "red") +
  geom_function(fun = function(x) x^2, color = "orange") +
  geom_function(fun = function(x) x^3, color = "yellow") +
  geom_function(fun = function(x) x^4, color = "green") +
  geom_function(fun = function(x) x^5, color = "blue") +
  geom_function(fun = function(x) x^6, color = "purple", linewidth = 2) +
  geom_function(fun = function(x) x^7, color = "violet") + ylim(0, 20000)

#straight line, price ~ z
ggplot(data = train, aes(x = Z, y = Price)) +
  geom_point() +
  geom_smooth(method = lm, formula = y ~ x,
              color = "red", linewidth = 2)

ggplot(data = train, aes(x = Z, y = Price)) +
  geom_point() +
  geom_smooth(method = lm, formula = y ~ I(x^6),
              color = "purple", linewidth = 2)

model_exp <- fit(object = linear_reg(),
                 formula = Price ~ I(Z^6),
                 data = train)
model_exp %>% tidy()
