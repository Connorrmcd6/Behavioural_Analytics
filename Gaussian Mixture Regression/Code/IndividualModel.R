library(ggplot2)
library(flexmix)
c = read.csv('tdata.csv')

x1 = c$Beneficiaries
x1 = c$Female_Ratio
#x1 = c$Age
#x1 = c$Pens_ratio
#x1 = c$Depen_ratio

y = c$Claims

m1 <- flexmix(c$Claims ~ x1, data = c, k = 3)
m1

par = parameters(m1)

c$f1 = par[1,1] + par[2,1]*x1 
c$f2 = par[1,2] + par[2,2]*x1 
c$f3 = par[1,3] + par[2,3]*x1 

ggplot(c, aes(x=x1,  y=y)) + 
  geom_line(aes(y = f1), color = "red") + 
  geom_line(aes(y = f2), color="orange") +
  geom_line(aes(y = f3), color="black")+ 
  geom_point(shape=18, color="blue")+
  labs(x="Standardized Female_Ratio",y="Standardized Claim Cost")

