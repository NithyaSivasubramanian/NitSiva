getwd() # function we use to know the present working directory
setwd("C://Users//JayNit//Desktop//Nithya//Data Science//ExcelR//Assignments//Logistic Regression") # Created a new working directory
library(readxl)
install.packages("boot")
install.packages("ISLR")
install.packages("dplyr")
install.packages("ggvis")
library(xlsx)
library(ISLR)
library(dplyr)
library(ggvis)
library(boot)
credits <- read.csv("C://Users//JayNit//Desktop//Nithya//Data Science//ExcelR//Assignments//Logistic Regression//creditcard.csv")
#claimants <- read.csv(file.choose()) # Choose the claimants Data set
View(credits)
sum(is.na(credits))
credits <- na.omit(credits) # Omitting NA values from the Data 
# na.omit => will omit the rows which has atleast 1 NA value
dim(credits)

colnames(credits)
credits <- credits[,-1] # Removing the first column which is is an Index

install.packages("plyr")
library(plyr)
credits$card <- revalue(credits$card,c("yes"="0", "no"="1"))
credits$card <- as.numeric(credits$card)
credits$owner <- revalue(credits$owner,c("yes"="0", "no"="1"))
credits$selfemp <- revalue(credits$selfemp,c("yes"="0", "no"="1"))
credits$owner <- as.numeric(credits$owner)
credits$selfemp <- as.numeric(credits$selfemp)
attach(credits)

View(credits)
attach(credits)
# Preparing a linear regression 
mod_lm <- lm(credits$card~.,data=credits) # this dot will remove all the independent variables
pred1 <- predict(mod_lm,credits)
pred1
plot(credits$card,pred1)
# We can no way use the linear regression technique to classify the data
plot(pred1)


# We can also include NA values but where ever it finds NA value
# probability values obtained using the glm will also be NA 
# So they can be either filled using imputation technique or
# exlclude those values 


# GLM function use sigmoid curve to produce desirable results 
# The output of sigmoid function lies in between 0-1
model <- glm(card~.,data=credits,family = "binomial")
summary(model)
# To calculate the odds ratio manually we going r going to take exp of coef(model)
exp(coef(model))

# Confusion matrix table 
prob <- predict(model,credits,type="response")
summary(model)
plot(prob)
# We are going to use NULL and Residual Deviance to compare the between different models

# Confusion matrix and considering the threshold value as 0.5 
confusion<-table(prob>0.5,credits$card)
confusion
# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy # 70.62


# Creating empty vectors to store predicted classes based on threshold value
pred_values <- NULL
yes_no <- NULL

pred_values <- ifelse(prob>=0.5,1,0)
yes_no <- ifelse(prob>=0.5,"yes","no")

# Creating new column to store the above values
credits[,"prob"] <- prob
credits[,"pred_values"] <- pred_values
credits[,"yes_no"] <- yes_no

View(credits[,c(1,13:15)])

table(credits$card,credits$pred_values)
# Calculate the below metrics
# precision | recall | True Positive Rate | False Positive Rate | Specificity | Sensitivity
# from the above table - 59


# ROC Curve => used to evaluate the betterness of the logistic model
# more area under ROC curve better is the model 
# We will use ROC curve for any classification technique not only for logistic
install.packages("ROCR")
library(ROCR)
rocrpred<-prediction(prob,credits$card)
rocrperf<-performance(rocrpred,'tpr','fpr')
plot(rocrperf)
#library(ggplot2)
#ggplot(rocrperf)
str(rocrperf)

plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
# More area under the ROC Curve better is the logistic regression model obtained

## Getting cutt off or threshold value along with true positive and false positive rates in a data frame 
str(rocrperf)
rocr_cutoff <- data.frame(cut_off = rocrperf@alpha.values[[1]],fpr=rocrperf@x.values,tpr=rocrperf@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
View(rocr_cutoff)
plot(rocr_cutoff,colorize=T,text.adj=c(-0.2,1.7))

library(dplyr)
rocr_cutoff$cut_off <- round(rocr_cutoff$cut_off,6)
# Sorting data frame with respect to tpr in decreasing order 
rocr_cutoff <- arrange(rocr_cutoff,desc(TPR))
View(rocr_cutoff)

plot(rocr_cutoff,colorize=T,text.adj=c(-0.2,1.7))
