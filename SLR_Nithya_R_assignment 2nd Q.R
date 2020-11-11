getwd() # function we use to know the present working directory
setwd("C://Users//JayNit//Desktop//Nithya//Data Science//ExcelR//Assignments//Simple Linear Regression") # Created a new working directory
library(readxl)
library(readr)
library(readr)
library("MASS")
# ggplot for adding regresion line for data
library(dplyr)

library(ggplot2)
install.packages("moments")
library(moments)

DelryTime_data <- read.csv("C://Users//JayNit//Desktop//Nithya//Data Science//ExcelR//Assignments//Simple Linear Regression//delivery_time.csv")
# Load delivery_time.csv dataset
library(readr)
#wc_at <- read_csv("E://Excelr Data//R Codes//Simple Linear Regression//wc-at.csv")
View(DelryTime_data)

# Exploratory data analysis
summary(DelryTime_data)
attach(DelryTime_data)
#Scatter plot
plot(DelryTime_data$Delivery.Time, DelryTime_data$Sorting.Time)  # plot(X,Y)

?plot

attach(DelryTime_data)


#Correlation Coefficient (r)
cor(Delivery.Time, Sorting.Time)             # cor(X,Y)

# Simple Linear Regression model
reg <- lm(Sorting.Time ~ Delivery.Time) # lm(Y ~ X)

summary(reg)

pred <- predict(reg)

reg$residuals
sum(reg$residuals)

mean(reg$residuals)
sqrt(sum(reg$residuals^2)/nrow(DelryTime_data))  #RMSE

sqrt(mean(reg$residuals^2)) # another way to find RMSE value

confint(reg,level=0.95)
predict(reg,interval="predict")

# ggplot for adding regresion line for data
library(ggplot2)

?ggplot2

ggplot(data = DelryTime_data, aes(x = Delivery.Time, y = Sorting.Time)) + 
  geom_point(color='green') +
  geom_line(color='blue',data = DelryTime_data, aes(x=Delivery.Time, y=pred))

?ggplot2

########################
# A simple ggplot code for directly showing the line

# ggplot(wc_at,aes(Waist,AT))+stat_summary(fun.data=mean_cl_normal) + geom_smooth(method='lm')

####################

# Logrithamic Model

# x = log(Delivery.Time); y = Sorting.Time

plot(log(Delivery.Time), Sorting.Time)
cor(log(Delivery.Time), Sorting.Time)

reg_log <- lm(Sorting.Time ~ log(Delivery.Time))   # lm(Y ~ X)

summary(reg_log)
predict(reg_log)

reg_log$residuals
sqrt(sum(reg_log$residuals^2)/nrow(DelryTime_data))  #RMSE

confint(reg_log,level=0.95)
predict(reg_log,interval="confidence")

######################

# Exponential Model

plot(Delivery.Time, log(Sorting.Time))

cor(Delivery.Time, log(Sorting.Time))

reg_exp <- lm(log(Sorting.Time) ~ Delivery.Time)  #lm(log(Y) ~ X)

summary(reg_exp)

reg_exp$residuals

sqrt(mean(reg_exp$residuals^2))

logat <- predict(reg_exp)
at <- exp(logat)

error = DelryTime_data$Sorting.Time - at
error

sqrt(sum(error^2)/nrow(DelryTime_data))  #RMSE

confint(reg_exp,level=0.95)
predict(reg_exp,interval="confidence")

##############################
# Polynomial model with 2 degree (quadratic model)

plot(Delivery.Time, Sorting.Time)
plot(Delivery.Time*Delivery.Time, Sorting.Time)

cor(Delivery.Time*Delivery.Time, Sorting.Time)

plot(Delivery.Time*Delivery.Time, log(Sorting.Time))

cor(Delivery.Time, log(Sorting.Time))
cor(Delivery.Time*Delivery.Time, log(Sorting.Time))

# lm(Y ~ X + I(X*X) +...+ I(X*X*X...))

reg2degree <- lm(log(Sorting.Time) ~ Delivery.Time + I(Delivery.Time*Delivery.Time))

summary(reg2degree)

logpol <- predict(reg2degree)
expy <- exp(logpol)

err = DelryTime_data$Sorting.Time - expy

sqrt(sum(err^2)/nrow(DelryTime_data))  #RMSE

confint(reg2degree,level=0.95)
predict(reg2degree,interval="confidence")

# visualization
ggplot(data = DelryTime_data, aes(x = Delivery.Time + I(Delivery.Time^2), y = log(Sorting.Time))) + 
  geom_point(color='blue') +
  geom_line(color='red',data = DelryTime_data, aes(x=Delivery.Time+I(Delivery.Time^2), y=logpol))


##############################
#  Polynomial model with 3 degree

reg3degree<-lm(log(Sorting.Time)~Delivery.Time + I(Delivery.Time*Delivery.Time) + I(Delivery.Time*Delivery.Time*Delivery.Time))

summary(reg3degree)
logpol3 <- predict(reg3degree)
expy3 <- exp(logpol3)


# visualization
ggplot(data = DelryTime_data, aes(x = Delivery.Time + I(Delivery.Time^2) + I(Delivery.Time^3), y = Sorting.Time)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = DelryTime_data, aes(x=Delivery.Time+I(Delivery.Time^2)+I(Delivery.Time^3), y=expy3))

################################
