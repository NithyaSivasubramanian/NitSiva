getwd() # function we use to know the present working directory
setwd("C://Users//Assignments//Multi linear regression") # Created a new working directory
library(readxl)
library(readr)
library(readr)
library("MASS")
# ggplot for adding regresion line for data
library(dplyr)

library(ggplot2)
install.packages("moments")
library(moments)
install.packages("plyr")
install.packages("e1071")
library(plyr)
library(e1071)

toyota_data <- read.csv("C://Users//Assignments//Multi linear regression//ToyotaCorolla.csv")
View(toyota_data)

#Corolla<-data[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]

library(data.table)
dt <- fread("ToyotaCorolla.csv",select=c(3,4,7,9,13,14,16,17,18))
View(dt)

attach(dt)
summary(dt)

plot(Age_08_04, Price)
plot(KM, Price)
plot(HP, Price)
plot(cc, Price)
plot(Doors, Price)
plot(Gears, Price)
plot(Quarterly_Tax, Price)
plot(Weight, Price)


pairs(dt)
cor(dt)

install.packages("corpcor")
library(corpcor)
cor2pcor(cor(dt))

model.corolla <- lm(Price~.,data=dt)
summary(model.corolla)
plot(model.corolla)

model.corocc <- lm(Price ~ cc,data=dt)
summary(model.corocc)

model.corodoor <- lm(Price ~ Doors,data=dt)
summary(model.corodoor)

model.coro <- lm(Price ~ cc + Doors,data=dt)
summary(model.coro)

install.packages("psych")
library(psych)
pairs.panels(Cars)

# find out the influence 

influence.measures(model.coro)

install.packages("car")
library(car)
influenceIndexPlot(model.coro)

influencePlot(model.coro)

# Delete influentails records and build the model
model1 <- lm(Price ~ ., data = dt[-c(81),])
summary(model1)

## Variance Inflation factor to check collinearity b/n variables 
vif(model1)


## Added Variable plot to check correlation b/n variables and o/p variable
avPlots(model1,id.n=2,id.cex=0.7)


finalmodel <- lm(Price ~ Age_08_04 + KM + HP + cc + Gears + Quarterly_Tax + Weight, data = dt[-c(81),])
summary(finalmodel)


# Evaluate model LINE assumptions 
plot(finalmodel)
#Residual plots,QQplot,std-Residuals Vs Fitted,Cook's Distance 
qqPlot(finalmodel)
# QQ plot of studentized residuals helps in identifying outlier 

library("MASS")
stepAIC(finalmodel)
