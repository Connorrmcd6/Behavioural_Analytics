library(ggplot2)
library(reshape)
library(RColorBrewer)
library(tidyverse)
library(faraway)
library(xtable)

d = read.csv('tdata.csv')

claimsOut = sum(abs(d$Claims) > 3)
femOut = sum(abs(d$Female_Ratio) > 3)
ageOut = sum(abs(d$Age) > 3)
penOut = sum(abs(d$Pens_ratio) > 3)
benOut = sum(abs(d$Beneficiaries) > 3)
depOut = sum(abs(d$Depen_ratio) > 3)

out <- data.frame(
  category=c("Claims", "Beneficiaries", "Female_Ratio", "Age", "Pens_ratio", "Depen_ratio"),
  count=c(claimsOut, benOut, femOut, ageOut,penOut, depOut)
)

# Compute percentages
out$fraction = out$count / sum(out$count)

# Compute the cumulative percentages (top of each rectangle)
out$ymax = cumsum(out$fraction)

# Compute the bottom of each rectangle
out$ymin = c(0, head(out$ymax, n=-1))

# Compute label position
out$labelPosition <- (out$ymax + out$ymin) / 2

# Compute a good label
out$label <- paste0(out$category,'\n', round(100*out$fraction,2), "%")

# Make the plot
ggplot(out, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
  geom_rect() +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=3) +
  scale_fill_brewer(palette=4) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "none")


meltData <- melt(d)

p <- ggplot(meltData, aes(factor(variable), value)) 
p + geom_boxplot() + facet_wrap(~variable, scale="free") + labs(x='Variable', y='Z-Score')


clm = c()
ben = c()
fem = c()
age = c()
pen = c()
dep = c()

for(i in 1:length(d$Claims)){
  if(abs(d$Claims[i])>3){
    clm = append(clm, 'outlier')
  }else{
    clm = append(clm, 'non-outlier')
  }
  
  if(abs(d$Beneficiaries[i])>3){
    ben = append(ben, 'outlier')
  }else{
    ben = append(ben, 'non-outlier')
  }
  
  if(abs(d$Female_Ratio[i])>3){
    fem = append(fem, 'outlier')
  }else{
    fem = append(fem, 'non-outlier')
  }
  
  if(abs(d$Age[i])>3){
    age = append(age, 'outlier')
  }else{
    age = append(age, 'non-outlier')
  }
  
  if(abs(d$Pens_ratio[i])>3){
    pen = append(pen, 'outlier')
  }else{
    pen = append(pen, 'non-outlier')
  }
  
  if(abs(d$Depen_ratio[i])>3){
    dep = append(dep, 'outlier')
  }else{
    dep = append(dep, 'non-outlier')
  }
}


df <- data.frame(Claims = clm,
                 Beneficiaries = ben,
                 Female_Ratio = fem,
                 Age = age,
                 Pens_ratio = pen,
                 Depen_ratio = dep)
tidy_df <- df %>% 
  gather(key = "variable", value = "Zscore", 1:6)

# Stacked + percent
ggplot(data = tidy_df, aes(fill=Zscore, y=100, x=variable)) + 
  geom_bar(position="fill", stat="identity")




#data<-d[!(abs(d$Beneficiaries) > 3 | abs(d$Female_Ratio) > 3 | abs(d$Age) > 3 | abs(d$Depen_ratio) > 3 | abs(d$Pens_ratio) > 3 ),]
data = d


# Histogram with density plot
ggplot(data, aes(x=Claims)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth = 0.1)+
  geom_density(alpha=.2, fill="blue")

#Claims vs Age
ggplot(data, aes(x=Age, y=Claims)) + 
  geom_point(shape=18, color="blue")+
  geom_smooth(method=lm,  linetype="dashed",
              color="darkred", fill="blue", level=0.95, fullrange=TRUE)+
  labs(x="Standardized Age",y="Standardized Claim Cost")+ theme(aspect.ratio=1)
avg_age = c()
for(i in data$Age){
  if(i > 0){
    avg_age = append(avg_age, 'Above')
  }else{
    avg_age = append(avg_age, 'Below')
  }
}

data$avg_age = avg_age

# Color by groups
ggplot(data, aes(x=Claims, color=avg_age, fill=avg_age)) + 
  geom_histogram(aes(y=..density..), alpha=0.5, 
                 position="identity", binwidth = 0.1)+
  geom_density(alpha=.2)+ theme(aspect.ratio=1)

#Claims vs Female_ratio
ggplot(data, aes(x=Female_Ratio, y=Claims)) + 
  geom_point(shape=18, color="blue")+
  geom_smooth(method=lm,  linetype="dashed",
              color="darkred", fill="blue", level=0.95, fullrange=TRUE)+
  labs(x="Standardized Female Ratio",y="Standardized Claim Cost")+ theme(aspect.ratio=1)


primary_gender = c()
for(i in data$Female_Ratio){
  if(i > 0){
    primary_gender = append(primary_gender, 'Female')
  }else{
    primary_gender = append(primary_gender, 'Male')
  }
}

data$primary_gender = primary_gender

# Color by groups
ggplot(data, aes(x=Claims, color=primary_gender, fill=primary_gender)) + 
  geom_histogram(aes(y=..density..), alpha=0.5, 
                 position="identity", binwidth = 0.1)+
  geom_density(alpha=.2)+ theme(aspect.ratio=1)



#Claims vs Depen_ratio
ggplot(data, aes(x=Depen_ratio, y=Claims)) + 
  geom_point(shape=18, color="blue")+
  geom_smooth(method=lm,  linetype="dashed",
              color="darkred", fill="blue", level=0.95, fullrange=TRUE)+
  labs(x="Standardized Dependants Ratio",y="Standardized Claim Cost")+ theme(aspect.ratio=1)


#Claims vs Pens_ratio
ggplot(data, aes(x=Pens_ratio, y=Claims)) + 
  geom_point(shape=18, color="blue")+
  geom_smooth(method=lm,  linetype="dashed",
              color="darkred", fill="blue", level=0.95, fullrange=TRUE)+
  labs(x="Standardized Pensioners Ratio",y="Standardized Claim Cost")+ theme(aspect.ratio=1)


#Claims vs Beneficiaries
ggplot(data, aes(x=Beneficiaries, y=Claims)) + 
  geom_point(shape=18, color="blue")+
  geom_smooth(method=lm,  linetype="dashed",
              color="darkred", fill="blue", level=0.95, fullrange=TRUE)+
  labs(x="Standardized Beneficiaries",y="Standardized Claim Cost")+ theme(aspect.ratio=1)

#Claims vs Beneficiaries
ggplot(data, aes(x=Age, y=Pens_ratio)) + 
  geom_point(shape=18, color="blue")+
  geom_smooth(method=lm,  linetype="dashed",
              color="darkred", fill="blue", level=0.95, fullrange=TRUE)+
  labs(x="Standardized Age",y="Standardized Pensioners Ratio")+ theme(aspect.ratio=1)


c = data 
x1 = c$Beneficiaries
x2 = c$Female_Ratio
x3 = c$Age
x4 = c$Pens_ratio
x5 = c$Depen_ratio
y = c$Claims


mod = lm(y~x1+x2+x3+x4+x5)

summary(mod)

vif(mod)
