
library(ggplot2)

data = 'data asgn2.csv'

xy<-read.csv(data,sep=",")
x<-as.matrix(xy[,2])
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



ggplot(data = xy,aes(x=Time)) +
  geom_histogram(bins = 15,fill="darkgrey" , col= 'black')+ 
  geom_line(data = as.data.frame(knnde(k,n)),aes(x=x,y=12*fx), col="red",size=1) +
  
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "Count",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*0.0781, name="Density")
  )

