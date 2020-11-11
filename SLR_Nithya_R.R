getwd() # function we use to know the present working directory
setwd("C://Users//Assignments/Simple Linear Regression") # Created a new working directory
library(readxl)
library(readr)

library("MASS")
wc.at <- read.csv("C://Users//Assignments//Datasets//wc-at.csv")
# Load wc_at.csv dataset
library(readr)
calweight_at <- read_csv("C://Users//Assignments//Simple Linear Regression//calories_consumed.csv")
View(calweight_at)

# Exploratory data analysis
summary(calweight_at)

#Scatter plot
plot(calweight_at$`Weight gained (grams)`, calweight_at$`Calories Consumed`)  # plot(X,Y)

?plot

attach(calweight_at)


#Correlation Coefficient (r)
cor(calweight_at$`Weight gained (grams)`, calweight_at$`Calories Consumed`)             # cor(X,Y)

# Simple Linear Regression model
reg <- lm(calweight_at$`Calories Consumed` ~ calweight_at$`Weight gained (grams)`) # lm(Y ~ X)

summary(reg)

pred <- predict(reg)

reg$residuals
sum(reg$residuals)

mean(reg$residuals)
sqrt(sum(reg$residuals^2)/nrow(wc_at))  #RMSE

sqrt(mean(reg$residuals^2)) # another way to find RMSE value

confint(reg,level=0.95)
predict(reg,interval="predict")

# ggplot for adding regresion line for data
library(dplyr)
install.packages("tidyverse")
library(ggplot2)
install.packages("moments")
library(moments)

?ggplot2

ggplot(data = calweight_at, aes(x = calweight_at$`Weight gained (grams)`, y = calweight_at$`Calories Consumed`)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = calweight_at, aes(x=calweight_at$`Weight gained (grams)`, y=pred))

?ggplot2

########################
# A simple ggplot code for directly showing the line

# ggplot(wc_at,aes(Waist,AT))+stat_summary(fun.data=mean_cl_normal) + geom_smooth(method='lm')

####################0

# Logrithamic Model

# x = log(calweight_at$weight); y = AT

plot(log(calweight_at$`Weight gained (grams)`), calweight_at$`Calories Consumed`)
cor(log(calweight_at$`Weight gained (grams)`), calweight_at$`Calories Consumed`)

#(calweight_at$`Calories Consumed` ~ calweight_at$`Weight gained (grams)`)
reg_log <- lm(calweight_at$`Calories Consumed` ~ (log(calweight_at$`Weight gained (grams)`)))   # lm(Y ~ X)

summary(reg_log)

predict(reg_log)

reg_log$residuals
sqrt(sum(reg_log$residuals^2)/nrow(calweight_at))  #RMSE

confint(reg_log,level=0.95)
predict(reg_log,interval="confidence")

######################

# Exponential Model

# x = Waist and y = log(AT)

plot(calweight_at$`Weight gained (grams)`, log(calweight_at$`Calories Consumed`))

cor(calweight_at$`Weight gained (grams)`, log(calweight_at$`Calories Consumed`))

reg_exp <- lm(log(calweight_at$`Calories Consumed`) ~ calweight_at$`Weight gained (grams)`)  #lm(log(Y) ~ X)

summary(reg_exp)

reg_exp$residuals

sqrt(mean(reg_exp$residuals^2))

logat <- predict(reg_exp)
cconsu <- exp(logat)

error = calweight_at$`Calories Consumed` - at
error

sqrt(sum(error^2)/nrow(wc_at))  #RMSE

confint(reg_exp,level=0.95)
predict(reg_exp,interval="confidence")

##############################
# Polynomial model with 2 degree (quadratic model)

plot(calweight_at$`Weight gained (grams)`, calweight_at$`Calories Consumed`)
plot(calweight_at$`Weight gained (grams)`*calweight_at$`Weight gained (grams)`, calweight_at$`Calories Consumed`)

cor(calweight_at$`Weight gained (grams)`*calweight_at$`Weight gained (grams)`, calweight_at$`Calories Consumed`)

plot(calweight_at$`Weight gained (grams)`*calweight_at$`Weight gained (grams)`, log(calweight_at$`Calories Consumed`))

cor(calweight_at$`Weight gained (grams)`, log(calweight_at$`Calories Consumed`))
cor(calweight_at$`Weight gained (grams)`*calweight_at$`Weight gained (grams)`, log(calweight_at$`Calories Consumed`))

# lm(Y ~ X + I(X*X) +...+ I(X*X*X...))

reg2degree <- lm(log(calweight_at$`Calories Consumed`) ~ calweight_at$`Weight gained (grams)` + I(calweight_at$`Weight gained (grams)`*calweight_at$`Weight gained (grams)`))

summary(reg2degree)

logpol <- predict(reg2degree)
expy <- exp(logpol)

err = calweight_at$`Calories Consumed` - expy

sqrt(sum(err^2)/nrow(calweight_at))  #RMSE

confint(reg2degree,level=0.95)
predict(reg2degree,interval="confidence")

# visualization
ggplot(data = calweight_at, aes(x = calweight_at$`Weight gained (grams)` + I(calweight_at$`Weight gained (grams)`^2), y = log(calweight_at$`Calories Consumed`))) + 
  geom_point(color='blue') +
  geom_line(color='red',data = calweight_at, aes(x=calweight_at$`Weight gained (grams)`+I(calweight_at$`Weight gained (grams)`^2), y=logpol))


##############################
#  Polynomial model with 3 degree

reg3degree<-lm(log(calweight_at$`Calories Consumed`)~calweight_at$`Weight gained (grams)` + I(calweight_at$`Weight gained (grams)`*calweight_at$`Weight gained (grams)`) + I(calweight_at$`Weight gained (grams)`*calweight_at$`Weight gained (grams)`*calweight_at$`Weight gained (grams)`))

summary(reg3degree)
logpol3 <- predict(reg3degree)
expy3 <- exp(logpol3)


# visualization
ggplot(data = calweight_at, aes(x = calweight_at$`Weight gained (grams)` + I(calweight_at$`Weight gained (grams)`^2) + I(calweight_at$`Weight gained (grams)`^3), y = calweight_at$`Calories Consumed`)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = calweight_at, aes(x=calweight_at$`Weight gained (grams)`+I(calweight_at$`Weight gained (grams)`^2)+I(calweight_at$`Weight gained (grams)`^3), y=expy3))

################################
