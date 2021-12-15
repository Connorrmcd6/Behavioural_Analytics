library(cowplot)
library(ggplot2)

xy<-read.csv("data asgn2.csv",sep=",")
x<-as.matrix(xy[,2])
y<-as.matrix(xy[,1])
nrow(x)
summary(x)



n<-nrow(x)
n
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
plotknn<-ggplot(data = xfxplot,aes(x=xfxplot$x,y=xfxplot$fx))+
  geom_line(col="red",size=1)+labs(x="X",y="Density", title="k-n density estimate")
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
mode1 <- ms(4)
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



zz<-matrix(,nrow = n,ncol = 5)
for(i in 1:n) {
  mode1 <- ms(i)
  zz[i,1]<- mode1[1,1]
  zz[i,2]<- mode1[1,2]
  zz[i,3]<- mode1[1,3]
  zz[i,4]<- mode1[1,4]
  zz[i,5]<- y[i]
}

xfxplot<-as.data.frame(z)

xfxplot2<-as.data.frame(zz)



# Set new column values to appropriate colours
plotknn2<- ggplot(data = xfxplot,aes(x=V2,y=V3))+
  geom_point(size=1, col = xfxplot$V4)+labs(x="X",y="Density", title="40-nn density estimate") + theme(aspect.ratio=1)
plotknn2


aggregate(xfxplot$V2, list(xfxplot$V4), mean)

aggregate(xfxplot$V2, list(xfxplot$V4), var)

prop.table( table(xfxplot$V4))



acc = 0
for(i in 1:length(xfxplot2$V4)){
  if(xfxplot2$V4[i] == 178 & xfxplot2$V5[i] == 2){
    acc = acc + 1
  }else if(xfxplot2$V4[i] == 135 & xfxplot2$V5[i] == 1){
    acc = acc + 1
  }else{
    acc = acc + 0
  }

}

acc = 100*acc/length(xfxplot2$V4)
acc

