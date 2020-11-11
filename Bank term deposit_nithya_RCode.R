# most submitted code is R Code Bank Logistic Nithya.R
# this commands were executed from 
# https://rstudio-pubs-static.s3.amazonaws.com/333425_bc3ce119f58c46289dfbca3e51c73a55.html
getwd() # function we use to know the present working directory
setwd("C://Users//Assignments//Logistic Regression") # Created a new working directory
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
bank <- read.csv("C://Users//Assignments//Logistic Regression//bank-full.csv")
#myObj <- read.table(file.choose(), sep=";", header=TRUE)
#claimants <- read.csv(file.choose()) # Choose the claimants Data set
install.packages("magrittr")
library(magrittr )
input <- readLines("bank-full.csv") %>% gsub( '"|^# +', "", . ) %>% strsplit( "\\ +|;" )
View(input)
input1 <- input[-1] %>% do.call( what = rbind ) %>% as.data.frame()
#names(input1) <- c( input1[[1]][1:10], "fill.col", input1[[1]][11])
View(input1)
names(input1) <- c("age","job","marital","education","default","balance","housing","loan","contact","day","month","duration","campaign","pdays","previous","poutcome","y")
bankdata1 <- input1
View(bankdata1)

# split the data into train and test 70 % and 30 %

train <- bankdata1 %>% sample_frac(0.70)
test <- bankdata1 %>% anti_join(train)
train
test


apply(bankdata1,2,function(x)sum(is.na(x)))

sum(is.na(bankdata1))
bankdata1 <- na.omit(bankdata1) # Omitting NA values from the Data 
# na.omit => will omit the rows which has atleast 1 NA value
dim(bankdata1)

colnames(bankdata1)
#bankdata <- bankdata[,-1] # Removing the first column which is is an Index

install.packages("plyr")
library(plyr)
#credits$card <- revalue(credits$card,c("yes"="0", "no"="1"))
#credits$card <- as.numeric(credits$card)
#credits$owner <- revalue(credits$owner,c("yes"="0", "no"="1"))
#credits$selfemp <- revalue(credits$selfemp,c("yes"="0", "no"="1"))
#credits$owner <- as.numeric(credits$owner)
#credits$selfemp <- as.numeric(credits$selfemp)
#attach(credits)

#View(credits)
#attach(credits)
#Creating dummy variables by combining similar categories for variable job(char type)
t=table(bankdata$job)
sort(t)

finaljob=round(prop.table(table(bankdata$job,bankdata$y),1)*100,1)
finaljob

smar=addmargins(finaljob,2) #add margin across Y
sort(smar[,1])
View(smar)
install.pa("Select")
library(Select)
bankdata=bankdata %>% 
  mutate(job_1=as.numeric(job %in% c("self-employed","unknown","technician")), 
         job_2=as.numeric(job %in% c("services","housemaid","entrepreneur")),
         job_3=as.numeric(job %in% c("management","admin")),
         job_4=as.numeric(job=="student"),
         job_5=as.numeric(job=="retired"),
         job_6=as.numeric(job=="unemployed")) %>% 
  select(-job)
View(bankdata)
# Preparing a linear regression 

#Making dummies for variable marital

t=table(bankdata$marital)
sort(t)
bankdata=bankdata %>% 
  mutate(divorced=as.numeric(marital %in% c("divorced")),
         single=as.numeric(marital %in% c("single"))
  ) %>% 
  select(-marital)

t=table(bankdata$education)


sort(t)

bankdata=bankdata %>% 
  mutate(edu_primary=as.numeric(education %in% c("primary")),
         edu_sec=as.numeric(education %in% c("secondary")),
         edu_tert=as.numeric(education %in% c("tertiary"))
  ) %>% 
  select(-education)

#Making dummies for varible default
table(bankdata$default)
bankdata$default=as.numeric(bankdata$default=="yes")

#Making dummies for variable housing
table(bankdata$housing)
bankdata$housing=as.numeric(bankdata$housing=="yes")

#Making dummies for variable loan
table(bankdata$loan)
bankdata$loan=as.numeric(bankdata$loan=="yes")

#Making dummies for variable contact
t=table(bankdata$contact)
sort(t)
bankdata=bankdata %>% 
  mutate(co_cellular=as.numeric(contact %in% c("cellular")),
         co_tel=as.numeric(contact %in% c("telephone"))
  ) %>% 
  select(-contact)

#Making dummies for variable month
table(bankdata$month)
finalmnth=round(prop.table(table(bankdata$month,bankdata$y),1)*100,1)
sss=addmargins(finalmnth,2) #adding margin across Y
sort(sss[,1])

bankdata=bankdata %>% 
  mutate(month_1=as.numeric(month %in% c("aug","jun","nov","jan","jul")), 
         month_2=as.numeric(month %in% c("dec","sep")),
         month_3=as.numeric(month=="mar"),
         month_4=as.numeric(month=="oct"),
         month_5=as.numeric(month=="apr"),
         month_6=as.numeric(month=="feb")) %>% 
  select(-month)

#Making dummies for variable outcome
t=table(bankdata$poutcome)
sort(t)
bankdata=bankdata %>% 
  mutate(poc_success=as.numeric(poutcome %in% c("success")),
         poc_failure=as.numeric(poutcome %in% c("failure")),
         poc_other=as.numeric(poutcome %in% c("other"))
  )%>% 
  select(-poutcome)

bankdata$y=as.numeric(bankdata$y=="yes")
table(bankdata$y)


#train1=bankdata %>% 
#  filter(data=='train') %>% 
#  select(-data) #31647,34

#test1=bankdata %>% 
#  filter(data=='test') %>% 
#  select(-data,-y)

library(car)
memory.size()
memory.limit(50000)
library(speedglm)
mod_lm=speedlm(y~.,data=bankdata)
summary(mod_lm)

#============================================================================
mod_lm <- lm(bankdata$y~.,data=bankdata) # this dot will remove all the independent variables
pred1 <- predict(mod_lm,bankdata)
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




#=======================================
# doing the assignment with bank data by splitting the data into train and test
getwd() # function we use to know the present working directory
setwd("C://Users//Assignments//Logistic Regression") # Created a new working directory
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
bank <- read.csv("C://Users//Assignments//Logistic Regression//bank-full.csv")
#myObj <- read.table(file.choose(), sep=";", header=TRUE)
#claimants <- read.csv(file.choose()) # Choose the claimants Data set
install.packages("magrittr")
library(magrittr)

input <- readLines("bank-full.csv") %>% gsub( '"|^# +', "", . ) %>% strsplit( "\\ +|;" )
View(input)
input1 <- input[-1] %>% do.call( what = rbind ) %>% as.data.frame()
#names(input1) <- c( input1[[1]][1:10], "fill.col", input1[[1]][11])
View(input1)
names(input1) <- c("age","job","marital","education","default","balance","housing","loan","contact","day","month","duration","campaign","pdays","previous","poutcome","y")
bankdata1 <- input1
View(bankdata1)

# split the data into train and test 70 % and 30 %

train <- bankdata1 %>% sample_frac(0.70)
test <- bankdata1 %>% anti_join(train)
train
test

apply(train,2,function(x)sum(is.na(x)))
apply(test,2,function(x)sum(is.na(x)))


#combining both again 
test$y=NA
train$data='train'
test$data='test'
bankdata=rbind(train,test)
apply(bankdata,2,function(x)sum(is.na(x)))

#Creating dummy variables by combining similar categories for variable job(char type)
t=table(bankdata$job)
sort(t)

finaljob=round(prop.table(table(bankdata$job,bankdata$y),1)*100,1)
finaljob

smar=addmargins(finaljob,2) #add margin across Y
sort(smar[,1])
View(smar)
install.packages("Select")
library(Select)
bankdata=bankdata %>% 
  mutate(job_1=as.numeric(job %in% c("self-employed","unknown","technician")), 
         job_2=as.numeric(job %in% c("services","housemaid","entrepreneur")),
         job_3=as.numeric(job %in% c("management","admin")),
         job_4=as.numeric(job=="student"),
         job_5=as.numeric(job=="retired"),
         job_6=as.numeric(job=="unemployed")) %>% 
  select(-job)
View(bankdata)
# Preparing a linear regression 

#Making dummies for variable marital

t=table(bankdata$marital)
sort(t)
bankdata=bankdata %>% 
  mutate(divorced=as.numeric(marital %in% c("divorced")),
         single=as.numeric(marital %in% c("single"))
  ) %>% 
  select(-marital)

t=table(bankdata$education)


sort(t)

bankdata=bankdata %>% 
  mutate(edu_primary=as.numeric(education %in% c("primary")),
         edu_sec=as.numeric(education %in% c("secondary")),
         edu_tert=as.numeric(education %in% c("tertiary"))
  ) %>% 
  select(-education)


#Making dummies for varible default
table(bankdata$default)
bankdata$default=as.numeric(bankdata$default=="yes")

#Making dummies for variable housing
table(bankdata$housing)
bankdata$housing=as.numeric(bankdata$housing=="yes")

#Making dummies for variable loan
table(bankdata$loan)
bankdata$loan=as.numeric(bankdata$loan=="yes")

#Making dummies for variable contact
t=table(bankdata$contact)
sort(t)
bankdata=bankdata %>% 
  mutate(co_cellular=as.numeric(contact %in% c("cellular")),
         co_tel=as.numeric(contact %in% c("telephone"))
  ) %>% 
  select(-contact)

#Making dummies for variable month
table(bankdata$month)
finalmnth=round(prop.table(table(bankdata$month,bankdata$y),1)*100,1)
sss=addmargins(finalmnth,2) #adding margin across Y
sort(sss[,1])

bankdata=bankdata %>% 
  mutate(month_1=as.numeric(month %in% c("aug","jun","nov","jan","jul")), 
         month_2=as.numeric(month %in% c("dec","sep")),
         month_3=as.numeric(month=="mar"),
         month_4=as.numeric(month=="oct"),
         month_5=as.numeric(month=="apr"),
         month_6=as.numeric(month=="feb")) %>% 
  select(-month)

#Making dummies for variable outcome
t=table(bankdata$poutcome)
sort(t)
bankdata=bankdata %>% 
  mutate(poc_success=as.numeric(poutcome %in% c("success")),
         poc_failure=as.numeric(poutcome %in% c("failure")),
         poc_other=as.numeric(poutcome %in% c("other"))
  )%>% 
  select(-poutcome)

bankdata$y=as.numeric(bankdata$y=="yes")
table(bankdata$y)


#train1=bankdata %>% 
#  filter(data=='train') %>% 
#  select(-data) #31647,34

#test1=bankdata %>% 
#  filter(data=='test') %>% 
#  select(-data,-y)

library(car)
memory.size()
memory.limit(50000)
library(speedglm)
View(bankdata)
mod_lm=speedlm(y~.,data=bankdata)
summary(mod_lm)


t1<-table(bankdata$y)
t2<-table(train$y)
bankdata$y=as.numeric(bankdata$y=="yes")
t3<-table(bankdata$y)
library(dplyr)
library(Select)
train=bankdata %>% 
  filter(data=='train') %>% select(-data) #31647,34

test=bankdata %>% 
  filter(data=='test') %>% 
  select(-data,-y)

set.seed(5)
s=sample(1:nrow(train),0.70*nrow(train))
train_70=train[s,] 
test_30=train[-s,]
library(Rfast)
for_vif=lm(y~.,data=train)
summary(for_vif)

fit_train=train %>% 
  select(-edu_sec)
#1 omited
colnames(fit_train)

fit_train=train %>% 
  select(-edu_sec)
#1 omited
colnames(fit_train)

fit=glm(y~.,family = "binomial",data=fit_train) #32 predictor var
summary(fit) #we get aic:14348
