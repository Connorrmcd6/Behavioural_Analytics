library(ggplot2)
library(flexmix)
library(xtable)

set.seed(2021)
c = read.csv('tdata.csv')

x1 = c$Beneficiaries
x2 = c$Female_Ratio
x3 = c$Age
x4 = c$Pens_ratio
x5 = c$Depen_ratio
y = c$Claims

m1<- flexmix(c$Claims ~ x1 + x2 +x3 +x4 + x5, data = c, k = 3)
m1

par = parameters(m1)


#beneficiaries
c$f1 = par[1,1] + par[2,1]*x1 
c$f2 = par[1,2] + par[2,2]*x1 
c$f3 = par[1,3] + par[2,3]*x1 

ggplot(c, aes(x=x1,  y=y)) + 
  geom_line(aes(y = f1), color = "red") + 
  geom_line(aes(y = f2), color="orange") +
  geom_line(aes(y = f3), color="black")+ 
  geom_point(shape=18, color="blue")+
  labs(x="Standardized Beneficiary",y="Standardized Claim Cost")+ theme(aspect.ratio=1)


#female_ratio
c$f4 = par[1,1] + par[3,1]*x2
c$f5 = par[1,2] + par[3,2]*x2 
c$f6 = par[1,3] + par[3,3]*x2 

ggplot(c, aes(x=x2,  y=y)) + 
  geom_line(aes(y = f4), color = "red") + 
  geom_line(aes(y = f5), color="orange") +
  geom_line(aes(y = f6), color="black")+ 
  geom_point(shape=18, color="blue")+
  labs(x="Standardized Female_Ratio",y="Standardized Claim Cost")+ theme(aspect.ratio=1)

#age
c$f7 = par[1,1] + par[4,1]*x3 
c$f8 = par[1,2] + par[4,2]*x3 
c$f9 = par[1,3] + par[4,3]*x3 

ggplot(c, aes(x=x3,  y=y)) + 
  geom_line(aes(y = f7), color = "red") + 
  geom_line(aes(y = f8), color="orange") +
  geom_line(aes(y = f9), color="black")+ 
  geom_point(shape=18, color="blue")+
  labs(x="Standardized Age",y="Standardized Claim Cost")+ theme(aspect.ratio=1)


#Pensioners
c$f10 = par[1,1] + par[5,1]*x4 
c$f11 = par[1,2] + par[5,2]*x4 
c$f12 = par[1,3] + par[5,3]*x4 

ggplot(c, aes(x=x4,  y=y)) + 
  geom_line(aes(y = f10), color = "red") + 
  geom_line(aes(y = f11), color="orange") +
  geom_line(aes(y = f12), color="black")+ 
  geom_point(shape=18, color="blue")+
  labs(x="Standardized Pensioners Ratio",y="Standardized Claim Cost")+ theme(aspect.ratio=1)

#dependants
c$f13 = par[1,1] + par[6,1]*x5 
c$f14 = par[1,2] + par[6,2]*x5x
c$f15 = par[1,3] + par[6,3]*x5

ggplot(c, aes(x=x5,  y=y)) + 
  geom_line(aes(y = f13), color = "red") + 
  geom_line(aes(y = f14), color="orange") +
  geom_line(aes(y = f15), color="black")+ 
  geom_point(shape=18, color="blue")+
  labs(x="Standardized Dependents Ratio",y="Standardized Claim Cost")+ theme(aspect.ratio=1)


xtable(par)


