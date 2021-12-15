
library(ggplot2)
library(mixtools)
setwd("~/Desktop/Masters/STK802/Assignment 2")
xy<-read.csv("data asgn2.csv",sep=",")
x<-as.matrix(xy[,2])
y<-as.matrix(xy[,1])


n<-nrow(x)
k=40 ;


# knn denisty estimation - function
# requires number of  nearest neighbours
# and the sample size
dx<-as.matrix(dist(x,p=2))
head(dx)
knnde <- function(k,n) {
  fx<-0
  for(i in 1:n) {
    zz <- dx[,i]
    dv<-sort(t(zz))
    fx[i]<-1/(dv[k]^2)
  }
  #xfx
  xfx <- cbind(1:n,x,fx) 
  nm <- cbind("onum","x","fx")
  colnames(xfx) <- nm
  return(xfx)
}

xfx<-knnde(k,n)
xfxplot<-as.data.frame(xfx)


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

z<-matrix(,nrow = n,ncol = 4)
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

corr <- as.data.frame(z)

types = list()
for(i in 1:length(corr$V4)){
  if(corr$V4[i] == 178){
    types[i] = 2
  }else{
    types[i] = 1
  }
}

corr$V5 = as.integer(types)

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



class_df = data.frame(mixmdl$posterior)
names(class_df)[1] <- '1'
names(class_df)[2] <- '2'
names(class_df)[3] <- '3'

class_GMM  = colnames(class_df)[apply(class_df,1,which.max)]
class_GMM_int = as.integer(class_GMM)
#print(class_GMM_int)

corr$V6 = class_GMM_int



m = 10000 #number of  trials
P = 0.8*250 #subsample rate


avg_corr_list = list()
for(z in 1:m){
  avg_corr_numerator = 0
  sample_index = sample(corr$V1, size = P )
  for(i in sample_index){
    
    if(corr$V5[i] == corr$V6[i]){
      avg_corr_numerator = avg_corr_numerator + 1
    }else{
      avg_corr_numerator = avg_corr_numerator
    }
  }
  avg = avg_corr_numerator/length(sample_index)
  avg_corr_list[z] = avg
}


mean(unlist(avg_corr_list))





