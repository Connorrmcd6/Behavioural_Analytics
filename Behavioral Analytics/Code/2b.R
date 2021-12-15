library(ggplot2)
library(flexmix)

d = read.csv('q2.csv')

x1 = d$x
y = d$y

m1 <- flexmix(y ~ poly(x1,3), data = d, k = 3)
m1

c = clusters(m1)

par =parameters(m1)
par

matplot(x1, fitted(m1), pch = 16, type = "p")
points(x1, y)


ggplot(d, aes(x=x,  y=y)) + 
  geom_point(color= c)


summary(m1)

