library(ggplot2)
library(GGally)
library(mixtools)
library(tidyverse)
library(mvtnorm)
library(plotly)
library(MASS)

# 1a
d = read.csv('q1.csv')

d %>% 
  ggplot(aes(x = x1)) +
  geom_density() + theme(aspect.ratio=1)

d %>% 
  ggplot(aes(x = x2)) +
  geom_density()+ theme(aspect.ratio=1)

ggplot(d, aes(x=x1, y=x2)) + geom_point()

ggplot(d, aes(x=x2, y=x1)) + geom_point()+ theme(aspect.ratio=1)

##############################################################################

p2 <- ggplot(d, aes(x = x1, y = x2)) +
  geom_point(alpha = .5) +
  geom_density_2d()

p2


(p <- plot_ly(d, x = ~x1, y = ~x2))
add_histogram2dcontour(p)



dens <- kde2d(d$x1, d$x2)

plot_ly(x = dens$x,
        y = dens$y,
        z = dens$z) %>% add_surface()



