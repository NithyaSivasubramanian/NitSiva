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

salary_data <- read.csv("C://Users//JayNit//Desktop//Nithya//Data Science//ExcelR//Assignments//Simple Linear Regression//Salary_Data.csv")
# Load salary_data.csv dataset
library(readr)

View(salary_data)

# Exploratory data analysis
summary(salary_data)
attach(salary_data)
#Scatter plot
plot(salary_data$YearsExperience,salary_data$Salary)  # plot(X,Y)

?plot

attach(salary_data)


#Correlation Coefficient (r)
cor(YearsExperience, Salary)             # cor(X,Y)

# Simple Linear Regression model
reg <- lm(YearsExperience ~ Salary) # lm(Y ~ X)

summary(reg)

pred <- predict(reg)

reg$residuals
sum(reg$residuals)

mean(reg$residuals)
sqrt(sum(reg$residuals^2)/nrow(salary_data))  #RMSE

sqrt(mean(reg$residuals^2)) # another way to find RMSE value

confint(reg,level=0.95)
predict(reg,interval="predict")

# ggplot for adding regresion line for data
library(ggplot2)

?ggplot2

ggplot(data = salary_data, aes(x = YearsExperience , y = Salary)) + 
  geom_point(color='red') +
  geom_line(color='blue',data = salary_data, aes(x=YearsExperience, y=pred))


########################
# A simple ggplot code for directly showing the line

# ggplot(wc_at,aes(Waist,AT))+stat_summary(fun.data=mean_cl_normal) + geom_smooth(method='lm')

####################

# Logrithamic Model

# x = log(Delivery.Time); y = Sorting.Time

plot(log(YearsExperience), Salary)
cor(log(YearsExperience), Salary)

reg_log <- lm(Salary ~ log(YearsExperience))   # lm(Y ~ X)

summary(reg_log)
predict(reg_log)

reg_log$residuals
sqrt(sum(reg_log$residuals^2)/nrow(salary_data))  #RMSE

confint(reg_log,level=0.95)
predict(reg_log,interval="confidence")

######################

# Exponential Model

plot(YearsExperience, log(Salary))

cor(YearsExperience, log(Salary))

reg_exp <- lm(log(Salary) ~ YearsExperience)  #lm(log(Y) ~ X)

summary(reg_exp)

reg_exp$residuals

sqrt(mean(reg_exp$residuals^2))

logat <- predict(reg_exp)
at <- exp(logat)

error = salary_data$Salary - at
error

sqrt(sum(error^2)/nrow(salary_data))  #RMSE

confint(reg_exp,level=0.95)
predict(reg_exp,interval="confidence")

##############################
# Polynomial model with 2 degree (quadratic model)

plot(YearsExperience, Salary)
plot(YearsExperience*YearsExperience, Salary)

cor(YearsExperience*YearsExperience, Salary)

plot(YearsExperience*YearsExperience, log(Salary))

cor(YearsExperience, log(Salary))
cor(YearsExperience*YearsExperience, log(Salary))

# lm(Y ~ X + I(X*X) +...+ I(X*X*X...))

reg2degree <- lm(log(Salary) ~ YearsExperience + I(YearsExperience*YearsExperience))

summary(reg2degree)

logpol <- predict(reg2degree)
expy <- exp(logpol)

err = salary_data$Salary - expy

sqrt(sum(err^2)/nrow(salary_data))  #RMSE

confint(reg2degree,level=0.95)
predict(reg2degree,interval="confidence")

# visualization
ggplot(data = salary_data, aes(x = YearsExperience + I(YearsExperience^2), y = log(Salary))) + 
  geom_point(color='blue') +
  geom_line(color='red',data = salary_data, aes(x=YearsExperience+I(YearsExperience^2), y=logpol))


##############################
#  Polynomial model with 3 degree

reg3degree<-lm(log(Salary)~YearsExperience + I(YearsExperience*YearsExperience) + I(YearsExperience*YearsExperience*YearsExperience))

summary(reg3degree)
logpol3 <- predict(reg3degree)
expy3 <- exp(logpol3)


# visualization
ggplot(data = salary_data, aes(x = YearsExperience + I(YearsExperience^2) + I(YearsExperience^3), y = Salary)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = salary_data, aes(x=YearsExperience+I(YearsExperience^2)+I(YearsExperience^3), y=expy3))

################################
