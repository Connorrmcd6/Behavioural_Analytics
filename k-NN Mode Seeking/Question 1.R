
library(dplyr)
library(patchwork) # To display 2 charts together
library(ggplot2)

data<-read.csv("q_clus.csv",sep=",")

x<-as.matrix(data[,1])
n<-nrow(x)
n
k=70

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



ggplot(data = data,aes(x=x1)) +
  geom_histogram(bins = 15,fill="lightgreen" , col= 'black')+ 
  geom_line(data = as.data.frame(knnde(k,n)),aes(x=x,y=12.8*fx), col="red",size=1) +
  
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "Count",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*0.0781, name="Density")
  )




