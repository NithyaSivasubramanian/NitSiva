# Using Random Forest
install.packages("randomForest")
library(randomForest)
data(iris)
View(iris)
# Splitting data into training and testing. As the species are in order 
# splitting the data based on species 
iris_setosa<-iris[iris$Species=="setosa",] # 50
iris_versicolor <- iris[iris$Species=="versicolor",] # 50
iris_virginica <- iris[iris$Species=="virginica",] # 50
iris_train <- rbind(iris_setosa[1:25,],iris_versicolor[1:25,],iris_virginica[1:25,])
iris_test <- rbind(iris_setosa[26:50,],iris_versicolor[26:50,],iris_virginica[26:50,])

# Building a random forest model on training data 
fit.forest <- randomForest(Species~.,data=iris_train, na.action=na.roughfix,importance=TRUE)
fit.forest$ntree
plot(fit.forest) # Explore the plot - which containts (n+1) plots where n - number of classes in output 
# Training accuracy 
mean(iris_train$Species==predict(fit.forest,iris_train)) # 100% accuracy 

# Prediction of train data
pred_train <- predict(fit.forest,iris_train)
library(caret)


# Confusion Matrix
confusionMatrix(iris_train$Species, pred_train)


# Predicting test data 
pred_test <- predict(fit.forest,newdata=iris_test)
mean(pred_test==iris_test$Species) # Accuracy = 94.6 % 


# Confusion Matrix 
library(caret)
confusionMatrix(iris_test$Species, pred_test)

# Visualization 
plot(fit.forest,lwd=2)
legend("topright", colnames(fit.forest$err.rate),col=1:4,cex=0.8,fill=1:4)


############ WBCD #################
# Read the dataset
wbcd <- read.csv(file.choose())
View(wbcd)
#First colum in dataset is id which is not required so we will be taking out
wbcd <- wbcd[-1]

#table of diagonis B <- 357 and M <- 212
table(wbcd$diagnosis)

# Replace B with Benign and M with Malignant. Diagnosis is factor with 2 levels that is B and M. We also replacing these two entery with Benign and Malignat
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B","M"), labels = c("Benign","Malignant"))

# table or proportation of enteries in the datasets. What % of entry is Bengin and % of entry is Malignant
round(prop.table(table(wbcd$diagnosis))*100,1)
summary(wbcd[c("radius_mean","texture_mean","perimeter_mean")])
#Create a function to normalize the data
norm <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}
#test normalization
norm(c(1,2,3,4,5))
norm(c(10,20,30,40,50))
#Apply the normalization function to wbcd dataset
wbcd_n <- as.data.frame(lapply(wbcd[2:31], norm))
View(wbcd_n)
wbcd_n["diagnosis"] <- wbcd$diagnosis
# Building a random forest model on training data 
wbcd_forest <- randomForest(diagnosis~.,data=wbcd_n,importance=TRUE)
plot(wbcd_forest)
legend("topright",colnames(wbcd_forest$err.rate),col=1:3,cex=0.8,fill=1:3)

acc_wbcd <- mean(wbcd_n$diagnosis==predict(wbcd_forest))
acc_wbcd
varImpPlot(wbcd_forest) # Explore about variable importance plot, how it can be used to for feature 
# Selection


# random forest using company data

CompanyData  <- read.csv("C://Users//JayNit//Desktop//Nithya//Data Science//ExcelR//Assignments//Decision Tree//Company_Data.csv")
set.seed(123)
hist(CompanyData$Sales, main = "Sales of Companydata",xlim = c(0,20),
     breaks=c(seq(10,20,30)), col = c("blue","red", "green","violet"))

CompanyData$ShelveLoc <- factor(CompanyData$ShelveLoc)
CompanyData$US <- factor(CompanyData$US)
CompanyData$Urban <- factor(CompanyData$Urban)
summary(CompanyData)


highsales = ifelse(CompanyData$Sales<9, "No", "Yes")  # if greater than 8 then high sales else Low
highsales <- factor(highsales)
CD = data.frame(CompanyData[2:11], highsales)
str(CD)
summary(CD)
table(CD$highsales)
# Data Partition
set.seed(123)
ind <- sample(2, nrow(CD), replace = TRUE, prob = c(0.7,0.3))
train <- CD[ind==1,]
test  <- CD[ind==2,]
set.seed(213)
rf <- randomForest(highsales~., data=train)
rf  # Description of the random forest with no of trees, mtry = no of variables for splitting

attributes(rf)
plot(rf)
# Prediction and Confusion Matrix - Training data 
pred1 <- predict(rf, train)
head(pred1)

head(train$highsales)
# looks like the first six predicted value and original value matches.

confusionMatrix(pred1, train$highsales) 

# Prediction with test data - Test Data 
pred2 <- predict(rf, test)
confusionMatrix(pred2, test$highsales)
plot(rf)

# Tune Random Forest Model mtry 
tune <- tuneRF(train[,-11], train[,11], stepFactor = 0.5, plot = TRUE, ntreeTry = 300,
               trace = TRUE, improve = 0.05)
rf1 <- randomForest(highsales~., data=train, ntree = 300, mtry = 3, importance = TRUE,
                    proximity = TRUE)
rf1

pred1 <- predict(rf1, train) # 100 % accuracy on training data 
confusionMatrix(pred1, train$highsales)
#mean(pred1==test$highsales) 

# test data prediction using the Tuned RF1 model
pred2 <- predict(rf1, test)
confusionMatrix(pred2, test$highsales)
mean(pred2==test$highsales)

hist(treesize(rf1), main = "No of Nodes for the trees", col = "light blue")
partialPlot(rf1, train, Price, "Yes")

getTree(rf, 1, labelVar = TRUE)
MDSplot(rf1, CD$highsales)
##############################

### RAndom forest using Fraud data

FraudCheck <- read.csv("C://Users//JayNit//Desktop//Nithya//Data Science//ExcelR//Assignments//Decision Tree//Fraud_check.csv")
View(FraudCheck)
FraudCheck$Undergrad <- factor(FraudCheck$Undergrad)
FraudCheck$Marital.Status <- factor(FraudCheck$Marital.Status)
FraudCheck$Urban <- factor(FraudCheck$Urban)
hist(FraudCheck$Taxable.Income)
hist(FraudCheck$Taxable.Income, main = "Risky Vs. Good data",xlim = c(0,100000),
    breaks=c(seq(40,60,80)), col = c("blue","red", "green","light blue"))

Risky_Good = ifelse(FraudCheck$Taxable.Income<= 30000, "Risky", "Good")
Risky_Good <- factor(Risky_Good)
summary(FraudCheck)

#COMPD$COMPD_High <- factor(COMPD$COMPD_High)

FraudCk = data.frame(FraudCheck, Risky_Good)
summary(FraudCk)
FC = FraudCk[,c(1:7)]
str(FC)
table(FC$Risky_Good)  


set.seed(123)
ind <- sample(2, nrow(FC), replace = TRUE, prob = c(0.7,0.3))
train <- FC[ind==1,]
test  <- FC[ind==2,]
set.seed(213)
rf <- randomForest(Risky_Good~., data=train)
rf  # Description of the random forest with no of trees, mtry = no of variables for splitting
# Out of bag estimate of error rate is 0.47 % in Random Forest Model.
attributes(rf)
# Prediction and Confusion Matrix - Training data 
pred1 <- predict(rf, train)
head(pred1)
head(train$Risky_Good)
# looks like the first six predicted value and original value matches.

confusionMatrix(pred1, train$Risky_Good)
mean(pred1==test$Risky_Good)


# Prediction with test data - Test Data 
pred2 <- predict(rf, test)
confusionMatrix(pred2, test$Risky_Good)
mean(pred2==test$Risky_Good)
plot(rf)

# Tune Random Forest Model mtry 
tune <- tuneRF(train[,-6], train[,6], stepFactor = 0.5, plot = TRUE, ntreeTry = 300,
               trace = TRUE, improve = 0.05)
rf1 <- randomForest(Risky_Good~., data=train, ntree = 200, mtry = 2, importance = TRUE,
                    proximity = TRUE)
rf1
pred1 <- predict(rf1, train)
confusionMatrix(pred1, train$Risky_Good)
mean(pred1==test$Risky_Good)

# test data prediction using the Tuned RF1 model
pred2 <- predict(rf1, test)
confusionMatrix(pred2, test$Risky_Good)
                
# no of nodes of trees

hist(treesize(rf1), main = "No of Nodes for the trees", col = "green")
varImpPlot(rf1)

partialPlot(rf1, train, Taxable.Income, "Good")

tr1 <- getTree(rf1, 2, labelVar = TRUE)

# Multi Dimension scaling plot of proximity Matrix
MDSplot(rf1, FC$Risky_Good)
