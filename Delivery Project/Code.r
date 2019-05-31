library(ggplot2)
fee.r=read.csv(file.choose(),header = T)
head(fee.r)
str(fee.r)

# RQ1: Find the relationship between the Price of the order and Tips
# generate a new dataframe with categorical variables
data.r=data.frame(fee.r$cost,fee.r$tips,fee.r$pec,fee.r$ppec,as.factor(fee.r$gender),as.factor(fee.r$age),
                  as.factor(fee.r$race),as.factor(fee.r$time),as.factor(fee.r$house),
                  as.factor(fee.r$date),as.factor(fee.r$Rcost),as.factor(fee.r$Rtips),as.factor(fee.r$Rpec))
colnames(data.r)=c("Price","Tips","Pecentage","PPec","Gender","Age","Race","Time","House","Date","Rcost","Rtips","Rpec")

ggplot(data.r,aes(Price,Tips))+geom_point(aes(Price,Tips,color=Rcost))+labs(title="Price of the order Vs Tips")+scale_x_continuous(breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60))

ggplot(data.r,aes(Price,Tips))+geom_point(aes(Price,Tips,color=Rcost))+
  labs(title="Price of the order Vs Tips")+scale_x_continuous(breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60))+
  stat_smooth(color="steelblue",method='glm',se=FALSE)

# RQ2: Find the relationship between the Price of the order and  Ratio of Tips to Price
# plot Percentage~Price, fitted pec(glm method)~cost, fitted pec(loess method)~cost with CiBond
ggplot(data.r,aes(Price,Pecentage))+geom_point(aes(Price,Pecentage,color=Rcost))+
  labs(title="Price VS. The Ratio of Tips in Cost")+
  scale_x_continuous(breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60))

ggplot(data.r,aes(Price,Pecentage))+geom_point(aes(Price,Pecentage,color=Rcost))+
  stat_smooth(color="steelblue",method="glm",se=FALSE)+
  labs(title="Cost VS. The Ratio of Tips in Cost")+
  scale_x_continuous(breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60))

data.r$expp=exp(data.r$Pecentage)
data.r$recp=data.r$Price^-2
data.r$expc=exp(-0.5*data.r$Price)
perfit=lm(expp~recp+expc,data.r)
summary(perfit)
shapiro.test(residuals(perfit))
data.r$fit=log(fitted(perfit))
data.r$Price
plot(data.r$Price,data.r$fit)

ggplot(data.r,aes(Price,Pecentage))+geom_point(aes(Price,Pecentage,color=Rcost))+geom_line(aes(Price,fit))+
  stat_smooth(color="steelblue",method="glm",se=FALSE)+
  labs(title="Cost VS. The Ratio of Tips in Cost")+
  scale_x_continuous(breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60))

ggplot(data.r,aes(Price,Pecentage))+geom_point(aes(Price,Pecentage,color=Rcost))+geom_line(aes(Price,fit))+
  stat_smooth(color="steelblue",method="glm",se=FALSE)+stat_smooth(color="grey", se=FALSE)+
  labs(title="Cost VS. The Ratio of Tips in Cost")+
  scale_x_continuous(breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60))

# Although the increasing trend of percentage is going to be flat, tips is increasing along the increasing Price of Order 
ggplot(data.r,aes(Price,Tips))+geom_point(aes(Price,Tips,color=Rcost))+stat_smooth(color="black", se=FALSE)+
  labs(title="Price of the order Vs Tips")+scale_x_continuous(breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60))+
  stat_smooth(color="steelblue",method='glm',se=FALSE)

# RQ3: People vs  Percentage?
perfit.full=lm(expp~recp+expc+Age+Race+Gender+House, data.r)
summary(perfit.full)
anova(perfit.full)
perfit.red=lm(expp~recp+expc+Age+House,data.r)
anova(perfit.red,perfit.ful)
anova(perfit.red)
TC=xtabs(~Age+House,data = data.r)
addmargins(TC)
chisq.test(TC)

# plot Percentage~Age by House
ggplot(data.r,aes(Pecentage,Age))+geom_point(aes(color=House))+labs(title = "Percentage VS Age")+
  scale_x_continuous( limits = c(0,0.5))

par(mfrow=c(2,2))
plot(perfit.red)
par(mfrow=c(1,1))

# RQ4:Is the Time(Day) Significant on the amount of delivered orders?
# calculate the total price every working shift
time=c(0,0,0,0,0,1,1,1,1,1)
date=c(1,2,4,6,7,1,2,4,6,7)
orderday1=sum(data.r[data.r$Time==0 & data.r$Date==1,]$Price)
orderday2=sum(data.r[data.r$Time==0 & data.r$Date==2,]$Price)
orderday3=sum(data.r[data.r$Time==0 & data.r$Date==4,]$Price)
orderday4=sum(data.r[data.r$Time==0 & data.r$Date==6,]$Price)
orderday5=sum(data.r[data.r$Time==0 & data.r$Date==7,]$Price)
orderday6=sum(data.r[data.r$Time==1 & data.r$Date==1,]$Price)
orderday7=sum(data.r[data.r$Time==1 & data.r$Date==2,]$Price)
orderday8=sum(data.r[data.r$Time==1 & data.r$Date==4,]$Price)
orderday9=sum(data.r[data.r$Time==1 & data.r$Date==6,]$Price)
orderday10=sum(data.r[data.r$Time==1 & data.r$Date==7,]$Price)
orderday=c(orderday1,orderday2,orderday3,orderday4,orderday5,orderday6,orderday7,orderday8,orderday9,orderday10)
orderday

# calculate the number of  delivered orders every working shift
ntime1=nrow(data.r[data.r$Time==0 & data.r$Date==1,])
ntime2=nrow(data.r[data.r$Time==0 & data.r$Date==2,])
ntime3=nrow(data.r[data.r$Time==0 & data.r$Date==4,])
ntime4=nrow(data.r[data.r$Time==0 & data.r$Date==6,])
ntime5=nrow(data.r[data.r$Time==0 & data.r$Date==7,])
ntime6=nrow(data.r[data.r$Time==1 & data.r$Date==1,])
ntime7=nrow(data.r[data.r$Time==1 & data.r$Date==2,])
ntime8=nrow(data.r[data.r$Time==1 & data.r$Date==4,])
ntime9=nrow(data.r[data.r$Time==1 & data.r$Date==6,])
ntime10=nrow(data.r[data.r$Time==1 & data.r$Date==7,])
ntime=c(ntime1,ntime2,ntime3,ntime4,ntime5,ntime6,ntime7,ntime8,ntime9,ntime10)
ntime

salary1=sum(data.r[data.r$Time==0 & data.r$Date==1,]$Tips)
salary2=sum(data.r[data.r$Time==0 & data.r$Date==2,]$Tips)
salary3=sum(data.r[data.r$Time==0 & data.r$Date==4,]$Tips)
salary4=sum(data.r[data.r$Time==0 & data.r$Date==6,]$Tips)
salary5=sum(data.r[data.r$Time==0 & data.r$Date==7,]$Tips)
salary6=sum(data.r[data.r$Time==1 & data.r$Date==1,]$Tips)
salary7=sum(data.r[data.r$Time==1 & data.r$Date==2,]$Tips)
salary8=sum(data.r[data.r$Time==1 & data.r$Date==4,]$Tips)
salary9=sum(data.r[data.r$Time==1 & data.r$Date==6,]$Tips)
salary10=sum(data.r[data.r$Time==1 & data.r$Date==7,]$Tips)
salaryT=c(salary1,salary2,salary3,salary4,salary5,salary6,salary7,salary8,salary9,salary10)+ntime
salaryT

# Using Chi-squared Test test statistical variable "Time"
data.salaryT=cbind(salaryT, orderday,ntime,time,date)
data.salaryT
matrix(ntime,ncol = 2)
chisq.test(matrix(ntime,ncol = 2))
time=as.factor(time)
date=as.factor(date)

#  ntime variable follows Poisson distribution.
nfit=glm(ntime~time+date,family = 'poisson')
summary(nfit)

# Test for statistical variables
salaryfit=lm(salaryT~ntime+orderday+time+date)
summary(salaryfit)
anova(salaryfit)

