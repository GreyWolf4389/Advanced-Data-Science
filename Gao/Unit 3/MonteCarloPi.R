library(ggplot2)
library(ggforce)
library(progress)

num_points <- 1000000

estimate_pi <- function(num_points) {
  inside_circle <- 0
  x_values <- c()
  y_values <- c()
  
  pb <- progress_bar$new(
    format = "[:bar] :percent ETA: :eta",
    total = num_points,
    clear = FALSE
  )
  
  for (i in 1:num_points) {
    x <- runif(1, 0, 1) 
    y <- runif(1, 0, 1)  
    
    x_values <- c(x_values, x)
    y_values <- c(y_values, y)
    
    if (x^2 + y^2 <= 1) {
      inside_circle <- inside_circle + 1
    }
    
    pb$tick()
    print(i)
  }
  
  pi_estimate <- 4 * inside_circle / num_points
  
  df <- data.frame(x = x_values, y = y_values)
  
  plot <- ggplot(df, aes(x, y)) +
    geom_point(aes(color = ifelse(x^2 + y^2 <= 1, "Inside", "Outside"))) +
    scale_color_manual(values = c("Inside" = "blue", "Outside" = "red")) +
    theme_minimal() +
    ggtitle(paste("Estimated Pi =", pi_estimate))
  
  print(plot)
  
  return(pi_estimate)
}

pi_estimate <- estimate_pi(num_points)

cat("Estimated value of pi:", pi_estimate, "\n")
