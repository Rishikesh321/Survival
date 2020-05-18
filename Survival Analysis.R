setwd("G:\\DATA SCIENCE\\DAta for me")

library(survival)
library(GGally)
library(ggplot2)

mydata = read.csv("survival_unemployment.csv")

attach(mydata)

#define variables

time = mydata$spell

event = mydata$event

X = cbind(ui,logwage,age)

group = mydata$ui

#Description

summary(time)
summary(event)
summary(X)
summary(group)

# kaplan - Meier non parametric analysis

kmsurvival = survfit(Surv(time,event) ~ 1)
summary(kmsurvival)
ggsurv(kmsurvival)

## kaplan - Meier non parametric analysis by employment insurance

kmsurvival_group = survfit(Surv(time,event)~group)
summary(kmsurvival_group)
ggsurv(kmsurvival_group)

# Nelson-Aalen non parametric analysis 

nasurvival = survfit(coxph(Surv(time,event)~1),method = "aalen")
summary(nasurvival)
ggsurv(nasurvival)


## Cox proportional hazard model it is a semi-parametric models

coxsurvival = coxph(Surv(time,event)~X , method = "breslow")
summary(coxsurvival)

#Exponential,weibull and loglogistics parametric models

exponential = survreg(Surv(time,event)~X,dist = "exponential")
summary(exponential)

weibull = survreg(Surv(time,event)~X,dist = "weibull")
summary(weibull)

loglogistic = survreg(Surv(time)~X , dist = "loglogistic")
summary(loglogistic)
