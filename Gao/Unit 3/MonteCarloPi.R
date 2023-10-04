x <- c(runif(1000000,0,1))
y <- c(runif(1000000,0,1))

x
y



ggplot(x=x, y=y) + geom_point()
