install.packages("tidymodels")
library(tidymodels)

distance <- c(4.3, 1.4, 1.4, 0.6, 1.4, 6.2, 6.6, 4.8, 5.3)
minutes <- c(19, 5 ,5 , 4, 5, 15, 20, 15, 12)
 cor(x=distance, y=minutes)
 
r_vals <- data.frame(X=c(1:100)) %>% as_tibble()

r_vals <- r_vals %>% rowwise() %>%
  mutate(A_PerfectPos = X,
         B_StrongPos = X + sample(-25:25, 1, replace = TRUE),
         C_ModeratePos = X + sample(-50:50, 1, replace = TRUE),
         D_WeakPos = X + sample(-125:125, 1, replace = TRUE),
         E_Negligble = sample(-50:50, 1, replace = TRUE),
         F_WeakNeg = -X + sample(-125:125, 1, replace =TRUE),
         G_ModerateNeg = -X + sample(-50:50, 1, replace = TRUE),
         H_StrongNeg = -X + sample(-25:25, 1, replace = TRUE),
         I_PerfectNeg = -X)

r_vals_long <- r_vals %>%
  pivot_longer(cols = names(select(r_vals, -1)),
               names_to = "Variable",
               values_to = "Value")

ggplot(data = r_vals_long) + 
  geom_point(aes(x = X, y = Value)) + 
  facet_wrap(facets = vars(Variable), scales = "free")

cor(r_vals, r_vals$X)

cor(x = diamonds$Price, y = diamonds$Carat)

cor(diamonds$Price, select_if(diamonds, is.numeric))

select_if(diamonds, is.numeric) %>% cor() %>% round(3)

ggplot(diamonds, aes(x = Carat, y = Price)) + geom_point()

ggplot(diamonds, aes(x = Z, y = Price)) + geom_point() + xlim(2.25, 6)

ggplot(diamonds, aes(x = Z, y = Price)) + geom_point()

ggplot(diamonds, aes(x = Depth, y = Price)) + geom_point()

ggplot(diamonds, aes(x = Depth, y = Price)) + geom_point(alpha = .1)

ggplot(diamonds %>% sample_n(5000), aes(x = Depth, y = Price)) + geom_point(alpha = .1)

#Splitting into two different data sets, one for training, the other for testing

diamonds_split <- initial_split(diamonds, prop = 0.75)
train <- training(diamonds_split)
test <- testing(diamonds_split)

train
test

get_upper_fence <- function(x){
  quantile(x, 0.75) + (1.5 * IQR(x))
}

get_lower_fence <- function(x) {
  quantile(x, 0.25) - (1.5 * IQR(x))
}


































