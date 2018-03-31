library(ggplot2)


#set number of beginning stitches and rounds
beginning_stitches <- 12
rounds <- 5
spokes <- seq(0, 2*pi, length.out = beginning_stitches + 1)
#set coordinates for spokes
x <- sin(spokes)
y <- cos(spokes)
xy <- data.frame(spokes, x, y)

xs <- c()
ys <- c()
for(i in 1:nrow(xy)) {
  new_x <- seq(0, xy$x[i], length.out = rounds + 1)
  xs <- c(xs, new_x)
  new_y <- seq(0, xy$y[i], length.out = rounds + 1)
  ys <- c(ys, new_y)
}
xs <- c(xs, x)
ys <- c(ys, y)
xy2 <- data.frame(xs, ys)  

q <- ggplot(xy2, aes(xs, ys))
q +  geom_point(size = 12, alpha = 1, color = "blue", shape = 16) +
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "orange"),
        axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(),
        legend.position = "none")







#try this again with all stitches

#set number of spokes
#set number of spokes
beginning_stitches <- 12

rounds <- 5
spokes <- seq(0, 2*pi, length.out = (beginning_stitches + 1) *
                (rounds + 1))
#set coordinates for spokes
x <- sin(spokes)
y <- cos(spokes)
def <- data.frame(spokes, x, y)

xs <- c()
ys <- c()
for(i in 1:nrow(def)) {
  quest <- ((i - 1) %% (rounds + 1)) 
  l_out <- (rounds + 1) - quest
  new_x <- seq(0, def$x[i], length.out = l_out)
  xs <- c(xs, new_x)
  new_y <- seq(0, def$y[i], length.out = l_out)
  ys <- c(ys, new_y)
}
xs <- c(xs, x)
ys <- c(ys, y)
def1 <- data.frame(xs, ys)  

q <- ggplot(def1, aes(xs, ys))
q +  geom_point(size = 12, alpha = 0.7, color = "orange", shape = 18) +
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "#1E90FF"),
        axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(),
        legend.position = "none")

quests <- c()
for(i in 1:61) {
  qw <- (i - 1) %% (rounds + 1)
  quests <- c(quests, qw)
}
numz <- (rounds + 1) - quests 
35 %% (rounds + 1)



test <- xy$x ^ 2 + xy$y ^2
test2 <- xy2$xs ^2 + xy2$ys

library(plotrix)

plot(1:5,seq(1,10,length=5),type="n",xlab="",ylab="",main="Test draw.circle")
draw.circle(2,4,c(1,0.66,0.33),border="purple",
            col=c("#ff00ff","#ff77ff","#ffccff"),lty=1,lwd=1)
draw.circle(2.5,8,0.6,border="red",lty=3,lwd=3)
draw.circle(4,3,0.7,border="green",col="yellow",lty=1,
            density=5,angle=30,lwd=10)
draw.circle(3.5,8,0.8,border="blue",lty=2,lwd=2)



#set number of beginning stitches and rounds
beginning_stitches <- 12
rounds <- 5
radiuz <- 2
newspokes <- seq(0, 2*pi*radiuz, length.out = beginning_stitches + 1)
#set coordinates for spokes
x <- sin(newspokes)
y <- cos(newspokes)
newxy <- data.frame(newspokes, x, y)

ggplot(newxy, aes(x = x, y = y)) + geom_point()


#####heart
par(bg = 'white')

dat <- data.frame(t=seq(0, 2*pi, by=0.1))
xhrt <- function(t) 16*sin(t)^3
yhrt <- function(t) 13*cos(t) - 5*cos(2*t) - 
  2*cos(3*t) - cos(4*t)
dat$y = yhrt(dat$t)
dat$x = xhrt(dat$t)
with(dat, plot(x,y, type="l"))

ggplot(dat, aes(x = x, y = y)) + geom_point(size = 12, color = "#1E90FF")+
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "orange"),
        axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(),
        legend.position = "none")

