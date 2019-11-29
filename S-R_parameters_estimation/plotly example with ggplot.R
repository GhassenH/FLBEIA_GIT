library(ggplot2)
library(plotly)
a <- rnorm(100)
b <- 1:100
data <- data.frame(x=a, y=b)
plot(a, b)
data 

p <- ggplot(data, aes(x=x, y=y)) +
  geom_histogram(stat = "identity")
p
ggplotly(p)


p <- ggplot(data, aes(x=a)) +
  geom_density()
p
ggplotly(p)
