
library(ggplot2)

library(mixtools)


setwd("~/Desktop/Masters/STK802/Assignment 2")
xy<-read.csv("data asgn2.csv",sep=",")
type_feature<-as.matrix(xy[,1])
time_feature<-as.matrix(xy[,2])
nrow(time_feature)
summary(time_feature)

plot<-ggplot(data = xy,aes(Time))+
  geom_histogram(bins = 15,fill="lightgreen")
plot

n<-nrow(time_feature)
n
k=70 ;


# knn denisty estimation - function
# requires number of  nearest neighbours
# and the sample size
dx<-as.matrix(dist(time_feature,p=2))
head(dx)
knnde <- function(k,n) {
fx<-0
for(i in 1:n) {
  zz <- dx[,i]
  dv<-sort(t(zz))
  fx[i]<-1/(dv[k]^2)
}
#xfx
xfx <- cbind(1:n,time_feature,fx) 
nm <- cbind("onum","time","fx")
colnames(xfx) <- nm
return(xfx)
}

xfx<-knnde(k,n)
xfxplot<-as.data.frame(xfx)
plotknn<-ggplot(data = xfxplot,aes(x=xfxplot$time,y=xfxplot$fx))+
  geom_line(col="red",size=1)+labs(x="Time",y="Density", title="k-n density estimate")
plotknn

### mode seeking - for a single observation
### needs observation number returns observation
### number of the associated mode 

ms <- function(onum) {
  flag<-0 
  nn<-onum
  kk<-0
  obs<-1
  while (flag==0) {    
    kk<-kk+1   
    zz <- cbind(dx[,nn],xfx)
    colnames(zz)<- c("d","nr","x","fx")
    #head(zz)
    dv<-zz[order(zz[,1]),]
    dvc <-  t(dv[1:k,4])
    #head(dvc)
    ss<-which.max(dvc)
    #ss
    nn<-dv[ss,2]
    #nn
    if(nn==obs){flag<-1}
    if(kk==n){flag<-1}
    obs<-nn
    #obs
  }  
  pointval <- dv[ss,2]
  pval <- cbind(xfxplot[onum,],pointval) 
  return(pval)
  }

#eg. for observation 1
mode1 <- ms(1)
mode1

z<-matrix(nrow = n,ncol = 4)
for(i in 1:n) {
  mode1 <- ms(i)
  z[i,1]<- mode1[1,1]
  z[i,2]<- mode1[1,2]
  z[i,3]<- mode1[1,3]
  z[i,4]<- mode1[1,4]
}

head(z)
pmodes<-unique(z[,4])
pmodes


#combine graphs
overlay<-ggplot() + geom_histogram(data = xy,aes(x=xy$Time), bins = 15,fill="lightgreen")
overlay <- overlay + geom_line(data = xfxplot, aes(x=xfxplot$time, y = xfxplot$fx*28.5 ), col="red",size=1) + scale_y_continuous("Count", sec.axis = sec_axis(~ . /28.5, name = "Density"))+
  scale_x_continuous(name = "Time")
overlay

k = 30
xfx <- knnde(k,n)
xfxplot<-as.data.frame(xfx)



z<-matrix(nrow = n,ncol = 4)
for(i in 1:n) {
  mode1 <- ms(i)
  z[i,1]<- mode1[1,1]
  z[i,2]<- mode1[1,2]
  z[i,3]<- mode1[1,3]
  z[i,4]<- mode1[1,4]
}

head(z)
pmodes<-unique(z[,4])
num_modes <- length(pmodes)
pmodes
num_modes

make_df <- as.data.frame(z)
for(i in 1:num_modes) {
  make_df["V4"][make_df["V4"] == pmodes[i]] <- i
}

colnames(make_df) <- c("Obs", "Time","Dens", "Modes")

#create a scatter plot
scat <- ggplot() + geom_point(data = make_df, aes(x=Time, y = Dens, color = Modes)) + scale_x_continuous(name = "Time")+
  scale_y_continuous(name = "Density")+ scale_color_gradientn(colours = rainbow(num_modes))
scat 

values_of_modes <- c()
for (s in 1:num_modes){
  index = pmodes[s]
  temp = xy$Time[index]
  values_of_modes <- c(values_of_modes, temp)
}

dataframe_all_vals <-as.data.frame(z)
sorted_df<-dataframe_all_vals[order(dataframe_all_vals$V4),]

colnames(sorted_df) <- c("Obs", "Time","Dens", "pmode")

X<-split(sorted_df, sorted_df$pmode)

avg<-mean(X$`95`$Time)
variance = var(X$`95`$Time)

#map the modes to clusters
dataframe_all_vals$Classify <- mapvalues(dataframe_all_vals$V4, 
                               from=c(pmodes[2],pmodes[1],pmodes[3]), 
                               to=c("1","2","3"))

final_knn_df = data.frame(dataframe_all_vals$Classify, type_feature)
names(final_knn_df)[1] <- 'Knn Classification'
names(final_knn_df)[2] <- 'Type Classification'

final_knn_df$matches = apply(final_knn_df, 1, function(x) as.integer(length(unique(x)) == 1))
acc_knn_Model = sum(final_knn_df$matches == 1)/250


# DOING THE GUASSIAN MIXTURE MODELLING

data_dens <- ggplot(xy, aes(x = time_feature)) +
  geom_density()
data_dens



plot_mix_comps <- function(x, mu, sigma, lam) {
  lam * dnorm(x, mu, sigma)
}

set.seed(1)
wait <- xy$Time
mixmdl <- normalmixEM(wait, k = 3)

data.frame(x = mixmdl$x)
combined = mixmdl$lambda[1]*dnorm(time_feature, mixmdl$mu[1], mixmdl$sigma[1]) + mixmdl$lambda[2]*dnorm(time_feature, mixmdl$mu[2], mixmdl$sigma[2])+
  mixmdl$lambda[3]*dnorm(time_feature, mixmdl$mu[3], mixmdl$sigma[3])
combined_df = data.frame(time_feature, combined)

gaus_plot = ggplot() + geom_density(data = xy, aes(Time)) +
geom_point(data=combined_df, aes(x = time_feature, y = combined), color = "purple") +
stat_function(geom = "line", fun = plot_mix_comps,args = list(mixmdl$mu[1], mixmdl$sigma[1], lam = mixmdl$lambda[1]),colour = "red", lwd = 1.5) +
stat_function(geom = "line", fun = plot_mix_comps,args = list(mixmdl$mu[2], mixmdl$sigma[2], lam = mixmdl$lambda[2]),colour = "blue", lwd = 1.5) +
stat_function(geom = "line", fun = plot_mix_comps,args = list(mixmdl$mu[3], mixmdl$sigma[3], lam = mixmdl$lambda[3]),colour = "green", lwd = 1.5) +
ylab("Density")

gaus_plot

#combine GMM graph and histogram
GMM_hist<-ggplot() + geom_histogram(data = xy,aes(x=xy$Time), bins = 15,fill="lightgreen")
GMM_hist <- GMM_hist + geom_line(data=combined_df, aes(x = time_feature, y = combined*300), color = "purple",,size=1) + scale_y_continuous("Count", sec.axis = sec_axis(~ . /300, name = "Density"))+
  scale_x_continuous(name = "Time")
GMM_hist

class_df = data.frame(mixmdl$posterior)
names(class_df)[1] <- '1'
names(class_df)[2] <- '2'
names(class_df)[3] <- '3'

class_GMM  = colnames(class_df)[apply(class_df,1,which.max)]
class_GMM_int = as.integer(class_GMM)
print(class_GMM_int)

final_GMM_df = data.frame(class_GMM_int, type_feature)
names(final_GMM_df)[1] <- 'GMM Classification'
names(final_GMM_df)[2] <- 'Type Classification'

final_GMM_df$matches = apply(final_GMM_df, 1, function(x) as.integer(length(unique(x)) == 1))
acc_GMM_Model = sum(final_GMM_df$matches == 1)/250

