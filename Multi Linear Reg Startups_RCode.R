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
library(plyr)

startups_data <- read.csv("C://Users//Assignments//Multi linear regression//50_Startups.csv")
View(startups_data)

startups_data$State <- revalue(startups_data$State,c("New York"="0", "California"="1", "Florida"="2"))
startups_data$State <- as.numeric(startups_data$State)
attach(startups_data)
colnames(startups_data) <- c("RDSpend", "Administration", "MarkSpend","State","Profit")
#startups_data <- cbind(RDSpend=R.D.Spend,Administration,MarkSpend=Marketing.Spend,State,Profit)
#Startups <- cbind(RDSpend=R.D.Spend,Administration,MarkSpend=Marketing.Spend,State,Profit)
View(Startups)

#data_startups <- as.data.frame(Startups)

attach(startups_data)

summary(startups_data)


# Load delivery_time.csv dataset
# Exploratory Data Analysis(60% of time)
# 1. Measures of Central Tendency
# 2. Measures of Dispersion
# 3. Third Moment Business decision
# 4. Fourth Moment Business decision
# 5. Probability distributions of variables
# 6. Graphical representations
#  > Histogram,Box plot,Dot plot,Stem & Leaf plot, 
#     Bar plot

summary(startups_data) # 5 point summary
install.packages("fastdummies")
library(fastdummies)


# 7. Find the correlation b/n Output (MPG) & (HP,VOL,SP)-Scatter plot
pairs(startups_data)
#plot(Cars)
# 8. Correlation Coefficient matrix - Strength & Direction of Correlation
cor(startups_data)

### Partial Correlation matrix - Pure Correlation  b/n the varibles
install.packages("corpcor")
library(corpcor)
cor2pcor(cor(startups_data))


# The Linear Model of interest
model.startups <- lm(Profit~RDSpend+Administration+MarkSpend+State,data=startups_data)
summary(model.startups)
plot(model.startups)

Model.Startups1 <- lm(Profit~RDSpend+log(Administration))
summary(Model.Startups1)

# Prediction based on only Volume 
model.startupsspend<-lm(startups_data$Profit~startups_data$RDSpend)
summary(model.startupsspend)
plot(model.startupsspend)
# Volume became significant

model.startupsMspend<-lm(startups_data$Profit~startups_data$MarkSpend)
summary(model.startupsMspend)

model.startupsMRDspend<-lm(startups_data$Profit~startups_data$MarkSpend+startups_data$RDSpend)
summary(model.startupsMRDspend)

#model.startupslogspend<-lm(Profit ~ log(MarkSpend))
#Model.Startups1 <- lm(Profit~RDSpend+log(MarkSpend))
#summary(model.startupslogspend)

# Prediction based on only Weight
model.carW<-lm(MPG~WT)
summary(model.carW) # Weight became significant

# Prediction based on Volume and Weight
model.carVW<-lm(MPG~VOL+WT)
summary(model.carVW) # Both became Insignificant

# So there exists a collinearity problem b/n volume and weight
### Scatter plot matrix along with Correlation Coefficients
install.packages("psych")
library(psych)
pairs.panels(Cars)

# It is Better to delete influential observations rather than deleting entire column which is 
# costliest process
# Deletion Diagnostics for identifying influential observations
influence.measures(model.startups)
install.packages("car")
library(car)

## plotting Influential measures 
influenceIndexPlot(model.startups) # index plots for infuence measures
influencePlot(model.startups,id.n=3) # A user friendly representation of the above

# Regression after deleting the 77th observation, which is influential observation
# Logarthimic Transformation 
#model.startups_Log<-lm(Profit~RDSpend+log(Administration)+MarkSpend+log(State),data=startups_data[-c(49,50),])
#summary(model.startups_Log)

model.startupsnew<-lm(Profit~RDSpend+Administration+MarkSpend+State,data=startups_data[-c(49,50),])
summary(model.startupsnew)

# Regression after deleting the 49 Observation
model.startupsnew1<-lm(Profit~RDSpend+Administration+MarkSpend+State,data=startups_data[-c(49),])
summary(model.startupsnew1)


## Variance Inflation factor to check collinearity b/n variables 
vif(model.startupsnew)
## vif>10 then there exists collinearity among all the variables 

## Added Variable plot to check correlation b/n variables and o/p variable
avPlots(model.startupsnew,id.n=2,id.cex=0.7)

## VIF and AV plot has given us an indication to delete "wt" variable

## Final model
finalmodel<-lm(Profit~RDSpend+Administration+MarkSpend+State,data=startups_data[-c(49,50),])
summary(finalmodel)

# Evaluate model LINE assumptions 
plot(finalmodel)
#Residual plots,QQplot,std-Residuals Vs Fitted,Cook's Distance 
qqPlot(finalmodel,id.n = 5)
# QQ plot of studentized residuals helps in identifying outlier 

library("MASS")
stepAIC(finalmodel)
