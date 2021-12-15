library(ggplot2)

d = read.csv('q2.csv')

ggplot(d, aes(x=x, y=y)) + geom_point()

clusters <- hclust(dist(d, method = 'euclidean'), method = 'average')
plot(clusters)

clusterCut <- cutree(clusters, 3)
d$class <-clusterCut

ggplot(d, aes(x=x, y=y)) + geom_point(col = d$class)

c = c()
for(i in 1:length(d$class)){
  if(d$class[i] == 1){
    c[i] = 'red'
  }else if(d$class[i] == 2){
    c[i] = 'blue'
  }else{
    c[i] = 'green'
  }
}

d$c = c



xy<-read.csv('q2.csv')
xy$c = c
x<-as.matrix(xy[,2])
y<-as.matrix(xy[,1])
c<-as.matrix(xy[,3])




# DOING THE GUASSIAN MIXTURE MODELLING
plot_mix_comps <- function(x, mu, sigma, lam) {
  lam * dnorm(x, mu, sigma)
}

set.seed(2021)

mixmdl <- normalmixEM(x, k = 3)

data.frame(x = mixmdl$x)
combined = mixmdl$lambda[1]*dnorm(x, mixmdl$mu[1], mixmdl$sigma[1]) + mixmdl$lambda[2]*dnorm(x, mixmdl$mu[2], mixmdl$sigma[2])+
  mixmdl$lambda[3]*dnorm(x, mixmdl$mu[3], mixmdl$sigma[3])
combined_df = data.frame(x, combined)


gaus_plot2 = ggplot() +
  geom_point(data=combined_df, aes(x = x, y = combined), color = "black", size = 1) +
  stat_function(geom = "line", fun = plot_mix_comps,args = list(mixmdl$mu[1], mixmdl$sigma[1], lam = mixmdl$lambda[1]),colour = "green", lwd = 1) +
  stat_function(geom = "line", fun = plot_mix_comps,args = list(mixmdl$mu[2], mixmdl$sigma[2], lam = mixmdl$lambda[2]),colour = "red", lwd = 1) +
  stat_function(geom = "line", fun = plot_mix_comps,args = list(mixmdl$mu[3], mixmdl$sigma[3], lam = mixmdl$lambda[3]),colour = "blue", lwd = 1) +
  ylab("Density")

gaus_plot2



