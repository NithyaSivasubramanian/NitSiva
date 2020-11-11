getwd() # function we use to know the present working directory
setwd("C://Users//JayNit//Desktop//Nithya//Data Science//ExcelR//Assignments//Multi linear regression") # Created a new working directory
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

comp_data <- read.csv("C://Users//JayNit//Desktop//Nithya//Data Science//ExcelR//Assignments//Multi linear regression//Computer_Data.csv")
View(comp_data)

#startups_data$State <- revalue(startups_data$State,c("New York"="0", "California"="1", "Florida"="2"))
#startups_data$State <- as.numeric(startups_data$State)

comp_data1 <- comp_data
comp_data1$cd <- as.numeric(revalue(comp_data1$cd,c("yes"=1, "no"=0)))
comp_data1$multi <- as.numeric(revalue(comp_data1$multi,c("yes"=1, "no"=0)))
comp_data1$premium <- as.numeric(revalue(comp_data1$premium,c("yes"=1, "no"=0)))
View(comp_data1)
class(comp_data1)

#Corolla<-data[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]

#library(data.table)
#dt <- fread("ToyotaCorolla.csv",select=c(3,4,7,9,13,14,16,17,18))
#View(dt)

attach(comp_data1)
summary(comp_data1)

plot(speed, price)
plot(hd, price)
plot(ram, price)
plot(cd, price)
plot(multi, price)
plot(premium, price)
plot(ads, price)
plot(trend, price)

pairs(comp_data1)
cor(comp_data1)

install.packages("corpcor")
library(corpcor)
cor2pcor(cor(comp_data1))

model.comp <- lm(price~.,data=comp_data1)
summary(model.comp)
plot(model.comp)

#model.corocc <- lm(Price ~ cc,data=dt)
#summary(model.corocc)

#model.corodoor <- lm(Price ~ Doors,data=dt)
#summary(model.corodoor)

#model.coro <- lm(Price ~ cc + Doors,data=dt)
#summary(model.coro)

install.packages("psych")
library(psych)
pairs.panels(comp_data1)

# find out the influence 

influence.measures(model.comp)

install.packages("car")
library(car)
influenceIndexPlot(model.comp)

influencePlot(model.comp)
Model.Computer_dataLog <- lm(price ~ log(speed)+log(hd)+log(ram)+log(screen)+log(cd)+log(multi)+log(premium)+log(ads)+log(trend),data=comp_data1[-c(1441,1701),])
Model.Computer_dataLog <- lm(price ~ log(speed+hd+ram+screen+cd+multi+premium+ads+trend),data=comp_data1[-c(1441,1701),])
summary(Model.Computer_dataLog) 
confint(Model.Computer_dataLog,level=0.95)

# Exponential Transformation :
Model.Computer_exp<-lm(log(price)~speed+hd+ram+screen+cd+multi+premium+ads+trend,data=comp_data1[-c(1441,1701),])
summary(Model.Computer_exp) 

Model.Computer_Quad <- lm(price~speed+I(speed^2)+hd+I(hd^2)+ram+I(ram^2)+screen+I(screen^2)+
                            +cd+I(cd^2)+multi+I(multi^2)+premium+I(premium^2)
                          +ads+I(ads^2)+trend+I(trend^2),data=comp_data1[-c(1441,1701),])
summary(Model.Computer_Quad)

# Delete influentails records and build the model
model1 <- lm(price ~ ., data = comp_data1[-c(1441,1701),])
summary(model1)


install.packages("psych")
library(psych)
install.packages("psych")
library(psych)
pairs.panels(Cars)
install.packages("car")
library(car)

## Variance Inflation factor to check collinearity b/n variables 
vif(Model.Computer_Quad)


## Added Variable plot to check correlation b/n variables and o/p variable
avPlots(Model.Computer_Quad,id.n=2,id.cex=0.7)



#finalmodel <- lm(Price ~ Age_08_04 + KM + HP + cc + Gears + Quarterly_Tax + Weight, data = comp_data1[-c(81),])

FinalModel<-lm(price~speed+I(speed^2)+I(speed^3)+
                 hd+I(hd^2)+I(hd^3)+
                 ram+I(ram^2)+I(ram^3)+
                 screen+I(screen^2)+I(screen^3)+
                 cd+I(cd^2)+I(cd^3)+
                 multi+I(multi^2)+I(multi^3)+
                 premium+I(premium^2)+I(premium^3)+
                 ads+I(ads^2)+I(ads^3)+
                 trend+I(trend^2)+I(trend^3),data=comp_data1[-c(1441,1701),])
summary(FinalModel)

Profit_Predict <- predict(FinalModel)
View(Profit_Predict)

finplot <- comp_data1[-c(1441,1701),]
View(finplot)

plot1 <- cbind(finplot$price, Profit_Predict)
pairs(plot1)

attach(comp_data1)
Final <- cbind(speed,hd,ram,screen,cd,multi,premium,ads,trend,price,Profit_Predict)

pairs(Final)
# Evaluate model LINE assumptions 
plot(FinalModel)
#Residual plots,QQplot,std-Residuals Vs Fitted,Cook's Distance 
qqPlot(FinalModel)
# QQ plot of studentized residuals helps in identifying outlier 

library("MASS")
stepAIC(FinalModel)
