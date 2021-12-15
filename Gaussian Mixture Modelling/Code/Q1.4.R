setwd("~/Desktop/Masters/STK802/Assignment 2")
library(ggplot2)
start_time = Sys.time()


xy<-read.csv("data asgn2.csv",sep=",")
x<-as.matrix(xy[,2])
m = 500 #number of clustering trials
P = 0.8*250 #subsample rate


I = matrix(0,nrow=length(x),ncol=length(x))
S = matrix(0,nrow=length(x),ncol=length(x))

for(z in 1:m){
  sample_data = as.matrix(sample(xy$Time, size = P ))
  #Get the I matrix
  for(i in sample_data){
    for(j in sample_data){
      if(i != j){
        r = match(c(j),x)
        c = match(c(i),x)
        I[r, c] = I[r, c] + 1
      }
    }
  }
  
  #Get the S matrix
  n<-nrow(sample_data)
  k = sample(10:80, 1)
  
  dx<-as.matrix(dist(sample_data,p=2))
  knnde <- function(k,n) {
    fx<-0
    for(i in 1:n) {
      zz <- dx[,i]
      dv<-sort(t(zz))
      fx[i]<-1/(dv[k]^2)
    }
    #xfx
    xfx <- cbind(1:n,sample_data,fx) 
    nm <- cbind("onum","x","fx")
    colnames(xfx) <- nm
    return(xfx)
  }
  
  xfx<-knnde(k,n)
  xfxplot<-as.data.frame(xfx)
  
  ms <- function(onum) {
    flag<-0 
    nn<-onum
    kk<-0
    obs<-onum
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
  
  z<-matrix(,nrow = n,ncol = 4)
  for(i in 1:n) {
    mode1 <- ms(i)
    z[i,1]<- mode1[1,1]
    z[i,2]<- mode1[1,2]
    z[i,3]<- mode1[1,3]
    z[i,4]<- mode1[1,4]
  }
  
  y = z[, 4]
  vals = z[,1]

  
  for(i in 1:length(y)){
    for(j in 1:length(y)){
      if(y[i] == y[j] & vals[i] != vals[j] ){
        S[i, j] = S[i, j] + 1
      }
    }
  }
  
}

C = S/I

C = as.data.frame(C)
C[is.na(C)] = 0


dendrogram = hclust(dist(C, method = "euclidean"), method = "average")
plot(dendrogram, 
     main = paste('dendrogram'),
     xlab = 'C_ij')

end_time = Sys.time()
run_time = end_time - start_time
run_time


