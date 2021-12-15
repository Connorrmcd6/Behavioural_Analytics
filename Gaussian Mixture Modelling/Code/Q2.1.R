setwd("~/Desktop/Masters/STK802/Assignment 2")
library(ggplot2)

start_time = Sys.time()


xy<-read.csv("data asgn2.csv",sep=",")
x<-as.matrix(xy[,2])
y<-as.matrix(xy[,1])


# DOING THE GUASSIAN MIXTURE MODELLING
plot_mix_comps <- function(x, mu, sigma, lam) {
  lam * dnorm(x, mu, sigma)
}

set.seed(2021)
wait <- xy$Time
mixmdl <- normalmixEM(wait, k = 3)

data.frame(x = mixmdl$x)
combined = mixmdl$lambda[1]*dnorm(x, mixmdl$mu[1], mixmdl$sigma[1]) + mixmdl$lambda[2]*dnorm(x, mixmdl$mu[2], mixmdl$sigma[2])+
  mixmdl$lambda[3]*dnorm(x, mixmdl$mu[3], mixmdl$sigma[3])
combined_df = data.frame(x, combined)

gaus_plot = ggplot() + geom_density(data = xy, aes(Time)) +
  geom_point(data=combined_df, aes(x = x, y = combined), color = "red") +
  ylab("Density")

gaus_plot


gaus_plot2 = ggplot() +
  geom_point(data=combined_df, aes(x = x, y = combined), color = "red") +
  stat_function(geom = "line", fun = plot_mix_comps,args = list(mixmdl$mu[1], mixmdl$sigma[1], lam = mixmdl$lambda[1]),colour = "grey30", lwd = 1) +
  stat_function(geom = "line", fun = plot_mix_comps,args = list(mixmdl$mu[2], mixmdl$sigma[2], lam = mixmdl$lambda[2]),colour = "grey40", lwd = 1) +
  stat_function(geom = "line", fun = plot_mix_comps,args = list(mixmdl$mu[3], mixmdl$sigma[3], lam = mixmdl$lambda[3]),colour = "grey50", lwd = 1) +
  ylab("Density")

gaus_plot2



#combine GMM graph and histogram
GMM_hist<-ggplot() + geom_histogram(data = xy,aes(x=Time), bins = 15,fill="darkgrey" , col= 'black')
GMM_hist <- GMM_hist + geom_line(data=combined_df, aes(x = x, y = combined*300), color = "red",,size=1) + scale_y_continuous("Count", sec.axis = sec_axis(~ . /300, name = "Density"))+
  scale_x_continuous(name = "Time")+ theme(aspect.ratio=9/16)
GMM_hist

class_df = data.frame(mixmdl$posterior)
names(class_df)[1] <- '1'
names(class_df)[2] <- '2'
names(class_df)[3] <- '3'

class_GMM  = colnames(class_df)[apply(class_df,1,which.max)]
class_GMM_int = as.integer(class_GMM)
#print(class_GMM_int)

final_GMM_df = data.frame(class_GMM_int, y)
names(final_GMM_df)[1] <- 'GMM Classification'
names(final_GMM_df)[2] <- 'Type Classification'

final_GMM_df$matches = apply(final_GMM_df, 1, function(x) as.integer(length(unique(x)) == 1))
acc_GMM_Model = sum(final_GMM_df$matches == 1)/250



end_time = Sys.time()
run_time = end_time - start_time
run_time
