#r values for the plots we made
#set working directory and have the diamonds dataset open
cor(x = diamonds$Price, y = diamonds$Carat)

#r values for all numeric values
cor(diamonds$Price, select_if(diamonds, is.numeric))

#r values for all combinations of numeric variables
#rounded to 2 decimal places for easier reading
select_if(diamonds, is.numeric) %>% cor() %>% round(3)

#Carat and Price have a strong positive correlation
ggplot(diamonds, aes(x = Carat, y = Price)) + geom_point()

#Z and Price have a strong positive correlation
ggplot(diamonds, aes(x = Z, y = Price)) + geom_point()+
  xlim(2.25, 6)

#Depth and Price
ggplot(diamonds, aes(x = Depth, y = Price)) + geom_point()

#Random sample of 500 diamonds
ggplot(sample_n(diamonds, 5000),
       aes(x = Depth, y = Price)) +
  geom_point()

#Create a model that uses a straight line, install the "tidymodels" package
diamonds_split <- initial_split(diamonds, prop = 0.75)
train <- training(diamonds_split)
test <- testing(diamonds_split)

#custom functions for upper and lower fences
get_upper_fence <- function(x) {
  quantile(x, 0.75) + (1.5 * IQR(x))
}

get_lower_fence <- function(x) {
  quantile(x, 0.25) - (1.5 * IQR(x))
}

#returns only rows with values between the fences
#uses . in all_vars() to refer to the current column, should filter the data
train <- train %>%
  filter_at(vars(Carat, Depth, Table, X, Y, Z),
            all_vars(. > get_lower_fence(.) &
                       . < get_upper_fence(.)))

#Plot the filtered trainings et to look for any more outliers
ggplot(data = train) +
  geom_point(aes(x = Z, y = Price))

#drop some outliers that were identified visually
train <- train%>% filter(Z < 2.25)


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
