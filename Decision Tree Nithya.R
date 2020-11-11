#Data Load
library(readr)
company_data <- read.csv("C://Users//Assignments//Decision Tree//Company_Data.csv")
View(company_data)

#Install the required packages
install.packages("caret")
install.packages("C50")
install.packages("tree")
install.packages("partykit")
install.packages("party")
install.packages("e1071")
#Library invoke
library(caret)
library(C50)
library(e1071)
library(tree)
library(gmodels)
library(party)
library(rmarkdown)
library(partykit)
#To make the results consistent across the runs

hist(company_data$Sales)
View(company_data)
summary(company_data)

company_data$ShelveLoc = as.factor(company_data$ShelveLoc)
company_data$US = as.factor(company_data$US)
company_data$Urban = as.factor(company_data$Urban)
str(company_data$Sales)
summary(company_data)

inTrainingcomp<-createDataPartition(company_data$Sales,p=.70,list = F)
Ctraining<-company_data[inTrainingcomp,]
Ctesting<-company_data[-inTrainingcomp,]
class(Ctraining)

#Model Building
model<-C5.0(as.factor(Sales)~.,data = Ctraining) 
#Generate the model summary
summary(model)

#Predict for test data set
pred<-predict.C5.0(model,Ctesting) #type ="prob"

#Accuracy of the algorithm
a<-table(Ctesting$Sales,pred)
sum(diag(a))/sum(a)
#Visualize the decision tree
plot(model)



##############
# accuracy of the above method came as 0.008403361 which is not great
#######################
#using rpubs logic
CompanyData <- read.csv("C://Users//Assignments//Decision Tree//Company_Data.csv")

View(CompanyData)
library(plyr)

View(CompanyData)

High = ifelse(CompanyData$Sales<10, "No", "Yes")
CD = data.frame(CompanyData, High)


hist(CompanyData$Sales)
CD_train <- CD[1:200,]

# View(CD_train)
CD_test <- CD[201:400,]

# View(CD_test)
attach(CompanyData)
modelhigh<-C5.0(as.factor(Sales)~.,data = CD_train) 
summary(modelhigh)
#Predict for test data set
predhigh<-predict.C5.0(modelhigh,CD_test) #type ="prob"

#Accuracy of the algorithm
a<-table(CD_test$Sales,predhigh)
sum(diag(a))/sum(a)
#Visualize the decision tree
plot(modelhigh)

#####  now trying  method. this worked beautifully. 

COMPD <- read.csv("C://Users//Assignments//Decision Tree//Company_Data.csv")

COMPD$ShelveLoc <- factor(COMPD$ShelveLoc)
COMPD$US <- factor(COMPD$US)
COMPD$Urban <- factor(COMPD$Urban)

High = ifelse(COMPD$Sales<10, "No", "Yes")
High <- factor(High)
summary(COMPD)

#COMPD$COMPD_High <- factor(COMPD$COMPD_High)

COMPD_CD = data.frame(COMPD, High)
summary(COMPD_CD)
#CD <- CompanyData[,2:12]
# View(CD)

CD_train1 <- COMPD_CD[1:200,]
summary(CD_train1)
dim(CD_train1)
# View(CD_train)
CD_test1 <- COMPD_CD[201:400,]
summary(CD_test1)
dim(CD_test1)
# View(CD_test)

#Using Party Function 
op_tree = ctree(High ~ CompPrice + Income + Advertising + Population + Price + ShelveLoc
                + Age + Education + Urban + US, data = CD_train1)
#op_tree = ctree(COMPD_High ~ CompPrice + Income + Population + Price + ShelveLoc
#                + Age + Education + US, data = CD_train1)
summary(op_tree)

plot(op_tree)

pred_tree <- as.data.frame(predict(op_tree,newdata=CD_test1))
 
#pred_tree["final"] <- NULL
pred_test_df <- predict(op_tree,newdata=CD_test1)
pred_test_df
dim(pred_test_df)

#mean(pred_test_df==COMPD_High) 
mean(pred_test_df==COMPD_CD$High)


CrossTable(CD_test1$High,pred_test_df)

confusionMatrix(CD_test1$High,pred_test_df)

##### Using tree function 
cd_tree_org <- tree(High~.-Sales,data=COMPD_CD)
summary(cd_tree_org)

plot(cd_tree_org)
text(cd_tree_org,pretty = 0)


cd_tree <- tree(High~.-Sales,data=CD_train1)
summary(cd_tree)

plot(cd_tree)
text(cd_tree,pretty = 0)


### Evaluate the Model

# Predicting the test data using the model
pred_tree <- as.data.frame(predict(cd_tree,newdata=CD_test1))
pred_tree["final"] <- NULL
pred_test_df1 <- predict(cd_tree,newdata=CD_test1)


pred_tree$final <- colnames(pred_test_df1)[apply(pred_test_df1,1,which.max)]

pred_tree$final <- as.factor(pred_tree$final)
summary(pred_tree$final)

summary(CD_test1$High)

mean(pred_tree$final==COMPD_CD$High)

CrossTable(CD_test1$High,pred_tree$final)

confusionMatrix(CD_test1$High,pred_tree$final)
