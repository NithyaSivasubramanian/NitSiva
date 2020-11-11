#Data Load
library(readr)


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

#####  now trying  method. this worked beautifully. 

FraudCheck <- read.csv("C://Users//JayNit//Desktop//Nithya//Data Science//ExcelR//Assignments//Decision Tree//Fraud_check.csv")
View(FraudCheck)
FraudCheck$Undergrad <- factor(FraudCheck$Undergrad)
FraudCheck$Marital.Status <- factor(FraudCheck$Marital.Status)


FraudCheck$Urban <- factor(FraudCheck$Urban)
hist(FraudCheck$Taxable.Income)
Risky_Good = ifelse(FraudCheck$Taxable.Income<= 30000, "Risky", "Good")
Risky_Good <- factor(Risky_Good)
summary(FraudCheck)

#COMPD$COMPD_High <- factor(COMPD$COMPD_High)

FraudCk = data.frame(FraudCheck, Risky_Good)
summary(FraudCk)
#CD <- CompanyData[,2:12]
# View(CD)

# View(CD)
FC_train <- FraudCk[1:300,]
# View(CD_train)
FC_test <- FraudCk[301:600,]

#summary(CD_train1)
#dim(CD_train1)
# View(CD_train)

#summary(CD_test1)
#dim(CD_test1)
# View(CD_test)

###Using Party Function 

png(file = "decision_tree.png")
opall_tree = ctree(Risky_Good ~ Undergrad + Marital.Status + City.Population + 
                     Work.Experience + Urban, data = FraudCk)
summary(opall_tree)

plot(opall_tree)

#Using Party Function 
png(file = "decision_tree.png")
op_tree = ctree(Risky_Good ~ Undergrad + Marital.Status + City.Population + 
                  Work.Experience + Urban, data = FC_train)
summary(op_tree)
plot(op_tree)

pred_tree <- as.data.frame(predict(op_tree,newdata=FC_test))
pred_tree["final"] <- NULL
pred_test_df <- predict(op_tree,newdata=FC_test)


mean(pred_test_df==FC_test$Risky_Good) # Accuracy = 82 %

CrossTable(FC_test$Risky_Good,pred_test_df)

confusionMatrix(FC_test$Risky_Good,pred_test_df)
