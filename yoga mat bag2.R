library(ggplot2)

t <- seq(0, 2*pi, length.out = 72)
x <- sin(t)
y <- cos(t)
df <- data.frame(t, x, y)

# Make a scatter plot of points in a circle
q <- ggplot(df, aes(x, y))
q + geom_point()

# Defining the number of points
ring <- 6
rounds <- 1:12
points <- sum(map_dbl(rounds, function(x) ring * x))

# Defining  Angle
#angle <- pi * (3 - sqrt(5))
angle <- pi * 1.95
#Defining another angle
angle2 <- pi

t <- (1:points) * angle
x <- sin(t)
y <-cos(t)
df <- data.frame(t, x, y)

# Make a scatter plot of points in a spiral
p <- ggplot(df, aes(x*t, y*t))
p + geom_point(color = "purple", shape = 15, size = 4) +
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(fill = "white"),
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank())


