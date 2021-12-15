library(ggplot2)
library("viridis")  
library(GGally)
library(mixtools)
library(tidyverse)
library(mvtnorm)
library(plotly)
library(MASS)

#1c
xy<-read.csv("q1.csv",sep=",")
x<-as.matrix(xy)

n<-nrow(x)
n
k= 40


dx<-as.matrix(dist(xy,p=2))

knnde <- function(k,n) {
  fx<-0
  for(i in 1:n) {
    zz <- dx[,i]
    dv<-sort(t(zz))
    fx[i]<-1/(dv[k]^2)
  }
  #xfx
  xfx <- cbind(1:n,xy,fx) 
  nm <- cbind("onum","x1","x2","fx")
  colnames(xfx) <- nm
  return(xfx)
}

xfx<-knnde(k,n)
xfxplot<-as.data.frame(xfx)


ms <- function(onum) {
  flag<-0 
  nn<-onum
  kk<-0
  obs<-1
  while (flag==0) {    
    kk<-kk+1   
    zz <- cbind(dx[,nn],xfx)
    colnames(zz)<- c("d","nr","x1","x2","fx")
    #head(zz)
    dv<-zz[order(zz[,1]),]
    dvc <-  t(dv[1:k,5])
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

z<-matrix(,nrow = n,ncol = 5)
for(i in 1:n) {
  mode1 <- ms(i)
  z[i,1]<- mode1[1,1]
  z[i,2]<- mode1[1,2]
  z[i,3]<- mode1[1,3]
  z[i,4]<- mode1[1,4]
  z[i,5]<- mode1[1,5]
}

head(z)
pmodes<-unique(z[,5])
sort(pmodes)


xfx<-knnde(k,n)
xfxplot<-as.data.frame(z)

c = c()
for(i in 1:length(xfxplot$V1)){
  if(xfxplot$V5[i] == 122){
    c[i] = 'red'
  }else if(xfxplot$V5[i] == 283){
    c[i] = 'blue'
  }else if(xfxplot$V5[i] == 317){
    c[i] = 'darkgreen'
  }else{
    c[i] = 'orange'
  }
}


xfxplot$c = c

p4 <- plot_ly(xfxplot, x = ~ V2, y = ~ V3, z = ~ V4,
              marker = list(color = ~ c)) %>% 
  add_markers()

p4

ggplot(xfxplot, aes(x=V2, y=V3)) + geom_point(col = xfxplot$c)


