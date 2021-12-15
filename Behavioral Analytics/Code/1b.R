library(ggplot2)
library(mixtools)
library(tidyverse)
library(mvtnorm)


# 1b
d = read.csv('q1.csv')

x1<-as.matrix(d[,1])
x2<-as.matrix(d[,2])
data = data.frame(x1, x2)

m1<- mean(d$x1)
m2<- mean(d$x2)

s1<- sd(d$x1)
s2<- sd(d$x2)

p <- ggplot(data, aes(x = x1)) + 
  geom_histogram(aes(x1, ..density..), binwidth = 0.4, colour = "black", fill = "white") +
  geom_vline(xintercept = m1, col = "red", size = 1) + theme(aspect.ratio=1)
p

p <- ggplot(data, aes(x = x2)) + 
  geom_histogram(aes(x2, ..density..), binwidth = 0.4, colour = "black", fill = "white") +
  geom_vline(xintercept = m2, col = "blue", size = 1) + theme(aspect.ratio=1)
p

###################

d = read.csv('q1.csv')
x <- c(d$x1,d$x2)
class <- c(rep('x1', 400), rep('xw', 400))
data <- data.frame(cbind(x=as.numeric(x), class=as.factor(class)))


p <- ggplot(data, aes(x = x)) + 
  geom_histogram(aes(x, ..density..), binwidth = 1, colour = "black", fill = "white") +
  geom_vline(xintercept = m1, col = "red", size = 2) + 
  geom_vline(xintercept = m2, col = "blue", size = 2)
p