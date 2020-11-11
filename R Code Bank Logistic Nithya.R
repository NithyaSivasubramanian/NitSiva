library(readxl)
library(xlsx)
library(ISLR)
library(dplyr)
library(ggvis)
library(boot)
library(plyr)
library(moments)
setwd("C://Users//JayNit//Desktop//Nithya//Data Science//ExcelR//Assignments//Logistic Regression") # Created a new working directory

bank <- read.csv("C://Users//JayNit//Desktop//Nithya//Data Science//ExcelR//Assignments//Logistic Regression//bank-full.csv")
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

#rapply(bankdata1,function(x)length(unique(x)))
unique(bankdata1$job)

bankdata1$job <- revalue(bankdata1$job,c("management"="0", "technician"="1", "entrepreneur"="2",
                                         "blue-collar"="3","unknown"="4","retired"="5",
                                         "admin."="6","services"="7","self-employed"="8",
                                         "unemployed"="9","housemaid"="10","student"="11"))
bankdata1$job <- as.numeric(bankdata1$job)
unique(bankdata1$marital)
bankdata1$marital <- revalue(bankdata1$marital,c("married"="0", "single"="1", "divorced"="2"))
bankdata1$marital <- as.numeric(bankdata1$marital)

unique(bankdata1$education)

bankdata1$education <- revalue(bankdata1$education,c("tertiary"="0", "secondary"="1", "unknown"="2",
                                         "primary"="3"))
bankdata1$education <- as.numeric(bankdata1$education)

unique(bankdata1$default)
bankdata1$default <- revalue(bankdata1$default,c("no"="0", "yes"="1"))
bankdata1$default <- as.numeric(bankdata1$default)

unique(bankdata1$housing)
bankdata1$housing <- revalue(bankdata1$housing,c("no"="0", "yes"="1"))
bankdata1$housing <- as.numeric(bankdata1$housing)

unique(bankdata1$loan)
bankdata1$loan <- revalue(bankdata1$loan,c("no"="0", "yes"="1"))
bankdata1$loan <- as.numeric(bankdata1$loan)

unique(bankdata1$contact)
bankdata1$contact <- revalue(bankdata1$contact,c("unknown"="0", "cellular"="1", "telephone"="2"))
bankdata1$contact <- as.numeric(bankdata1$contact)

unique(bankdata1$poutcome)
bankdata1$poutcome <- revalue(bankdata1$poutcome,c("unknown"="0", "failure"="1", "other"="2","success"="3"))
bankdata1$poutcome <- as.numeric(bankdata1$poutcome)

unique(bankdata1$y)
bankdata1$y <- revalue(bankdata1$y,c("no"="0", "yes"="1"))
bankdata1$y <- as.numeric(bankdata1$y)

unique(bankdata1$month)

bankdata1$month <- revalue(bankdata1$month,c("jan"="1", "feb"="2", "mar"="3",
                                         "apr"="4","may"="5","jun"="6",
                                         "jul"="7","aug"="8","sep"="9",
                                         "oct"="10","nov"="11","dec"="12"))
bankdata1$month <- as.numeric(bankdata1$month)

unique(bankdata1$age)
bankdata1$age <- as.numeric(bankdata1$age)
unique(bankdata1$balance)
bankdata1$balance <- as.numeric(bankdata1$balance)
unique(bankdata1$day)
bankdata1$day <- as.numeric(bankdata1$day)
unique(bankdata1$duration)
bankdata1$duration <- as.numeric(bankdata1$duration)
unique(bankdata1$campaign)
bankdata1$campaign <- as.numeric(bankdata1$campaign)
unique(bankdata1$pdays)
bankdata1$pdays <- as.numeric(bankdata1$pdays)
unique(bankdata1$previous)
bankdata1$previous <- as.numeric(bankdata1$previous)

attach(bankdata1)
summary(bankdata1)
sum(is.na(bankdata1))
bankdata1 <- na.omit(bankdata1) # Omitting NA values from the Data 
# na.omit => will omit the rows which has atleast 1 NA value
dim(bankdata1)

colnames(bankdata1)

# Preparing a linear regression 
mod_lm <- lm(y~.,data=bankdata1) # this dot will remove all the independent variables
pred1 <- predict(mod_lm,bankdata1)
pred1
plot(bankdata1$y,pred1)
plot(pred1)


# We can also include NA values but where ever it finds NA value
# probability values obtained using the glm will also be NA 
# So they can be either filled using imputation technique or
# exlclude those values 


# GLM function use sigmoid curve to produce desirable results 
# The output of sigmoid function lies in between 0-1
model <- glm(y~.,data=bankdata1,family = "binomial")
summary(model)
# To calculate the odds ratio manually we going r going to take exp of coef(model)
exp(coef(model))

# Confusion matrix table 
prob <- predict(model,bankdata1,type="response")
summary(model)
# We are going to use NULL and Residual Deviance to compare the between different models

# Confusion matrix and considering the threshold value as 0.5 
confusion<-table(prob>0.5,bankdata1$y)
confusion
# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy


# Creating empty vectors to store predicted classes based on threshold value
pred_values <- NULL
yes_no <- NULL

pred_values <- ifelse(prob>=0.5,1,0)
yes_no <- ifelse(prob>=0.5,"yes","no")

# Creating new column to store the above values
bankdata1[,"prob"] <- prob
bankdata1[,"pred_values"] <- pred_values
bankdata1[,"yes_no"] <- yes_no

View(bankdata1[,c(17,19:21)])

table(bankdata1$y,bankdata1$pred_values)
# Calculate the below metrics
# precision | recall | True Positive Rate | False Positive Rate | Specificity | Sensitivity
# from the above table - 59


# ROC Curve => used to evaluate the betterness of the logistic model
# more area under ROC curve better is the model 
# We will use ROC curve for any classification technique not only for logistic
library(ROCR)
rocrpred<-prediction(prob,bankdata1$y)
rocrperf<-performance(rocrpred,'tpr','fpr')

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









#==================================















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

library(car)
memory.size()
memory.limit(50000)
library(speedglm)
View(bankdata)
mod_lm=speedlm(y~.,data=bankdata)

# trying 

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