library(readxl
      )
q7_at <- read.csv("C://Users//Data Science//ExcelR//Assignments//Basic Statistics Level 1//q7.csv")
#1st business moment

attach(q7_at)
View(q7_at)
mean(Points)
mean(Score)
mean(Weigh)

median(Points)
median(Score)
median(Weigh)


install.packages("NCmisc")
library(NCmisc)

summary(q7_at)

#2nd Business moment
var(q7_at$Points) # Var
sd(q7_at$Points) # standard deviation
range(Points) # range

var(q7_at$Score) # Var
sd(q7_at$Score) # standard deviation
range(Score) # range

var(q7_at$Weigh) # Var
sd(q7_at$Weigh) # standard deviation
range(Weigh) # range


library(readxl)
q9_a <- read.csv("C://Data Science//ExcelR//Assignments//Basic Statistics Level 1//q9_a.csv")
q9_b <- read.csv("C://Data Science//ExcelR//Assignments//Basic Statistics Level 1//q9_b.csv")

#3rd Skewness & 4th Kurtosis
install.packages("moments")
library(moments)
hist(SP,col = "tomato3",main = "HISTOGRAM FOR SPEED") # as tail is on right side so its +ve skewness
skewness(q9_a)
skewness(SP)

kurtosis(q9_a)

skewness(q9_b)


kurtosis(q9_b)
boxplot(q9_a$speed,q9_a$dist)
hist(q9_a$speed)
hist(q9_a$dist)

hist(q9_b$SP)
hist(q9_b$WT)

data_cars <- read.csv("C:////Data Science//ExcelR//Assignments//Basic Statistics Level 1//Cars.csv")
attach(data_cars)
summary(data_cars)
MPG <- data_cars$MPG
(MPG>38)        
a=subset(MPG, MPG>38)
b=subset(MPG, MPG<40)
c=subset(MPG, MPG<50 & MPG>20)
install.packages("moments")
library(moments)
shapiro.test(MPG) # Normality test
attach(data_cars)
skewness(data_cars)
skewness(MPG)

kurtosis(data_cars)
kurtosis(MPG)
hist(MPG)
barplot(data_cars$MPG, col = "whitesmoke", main = "BARPLOT FOR HP")

#boxplot
boxplot(Cars_data,col = "blue")
boxplot.stats(SP)$out

wc_at <- read.csv("C://Data Science//ExcelR//Assignments//Basic Statistics Level 1//wc-at.csv")


attach(wc_at)
skewness(wc_at)
skewness(AT)
skewness(Waist)
kurtosis(AT)
kurtosis(Waist)

plot(Waist,AT)
# Correlation coefficient value for Waist and Addipose tissue
cor(AT,Waist) #cor(y,x)
summary(wc_at)


?ppnorm
