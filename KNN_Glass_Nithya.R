# Read the dataset
#glass dataset
library(readxl)
glass <- read.csv("C://Users//JayNit//Desktop//Nithya//Data Science//ExcelR//Assignments//KNN//glass.csv")
#glass <- read_excel(file.choose())
glass <- as.data.frame(glass)
class(glass)
View(glass)
table(glass$Type)
#First colum in dataset is id which is not required so we will be taking out
#glass <- glass[-1]
View(glass)
str(glass)
# target variable should be charactor. not factor 
#table of diagonis B <- 357 and M <- 212
table(glass$Type)
glass$type = as.factor(glass$Type)
str(glass)
# Replace types with the values
#glass$Type <- factor(glass$Type, levels = c(1,2,3,4,5,6,7), 
#              labels = c("building_windows_float_processed","building_windows_non_float_processed",
#              "vehicle_windows_float_processed","vehicle_windows_non_float_processed",
#              "containers","tableware","headlamps"))                 


# table or proportation of enteries in the datasets. What % of entry is Bengin and % of entry is Malignant
round(prop.table(table(glass$Type))*100,1)
# rounding to percentage 
summary(glass)
summary(glass[c("RI","Na","Mg")])
#Create a function to normalize the data
#defining a functin norm
norm <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}
#test normalization
norm(c(1,2,3,4,5))
norm(c(10,20,30,40,50))


#Apply the normalization function to glass dataset
glass_n <- as.data.frame(lapply(glass[1:9], norm))
View(glass_n)
summary(glass_n[c("RI","Na","Mg")])

#create training and test datasets
#glass_train <- glass_n[1:150,]
#glass_test <- glass_n[151:214,]
#create training and test datasets
set.seed(123)
ind <- sample(2, nrow(glass_n), replace = TRUE, prob = c(0.7,0.3))
glass_train <- glass_n[ind==1,]
glass_test <-  glass_n[ind==2,]

#Get labels for training and test datasets

#glass_train_labels <- glass[1:150,1]
#glass_test_labels <- glass[151:214,1]
#Get labels for training and test datasets
set.seed(123)
ind1 <- sample(2, nrow(glass), replace = TRUE, prob = c(0.7,0.3))
glass_train_labels <- glass[ind1==1,10]
glass_test_labels <-  glass[ind1==2,10]

# Build a KNN model on taining dataset
library("class")
library("caret")
# Building the KNN model on training dataset and also need labels which we are including c1. Once we build the preduction model
# we have to test on test dataset
#glass_pred <- knn(train = glass_train, test = glass_test, cl = glass_train_labels, k=21)
glass_test_pred <- knn(train = glass_train, test = glass_test, cl = glass_train_labels, k=3)
table(glass_test_pred,glass_test_labels)
acc_glass <- table(glass_test_pred,glass_test_labels)
class(glass_train)
class(glass_test)

mean(glass_test_pred==glass_test_labels)

## Now evualuation the model performance

# install package gmodels
install.packages("gmodels")
library("gmodels")

# Create cross table of predicted and actual
# Evaluating Model Performance ----

# load the gmodel library

CrossTable(x=glass_test_labels,y=glass_test_pred,prop.chisq = FALSE) 
#CrossTable( x =  glass_test_labels, y = glass_pred)

# to check  the accuracy

pr <- knn(dia_train,dia_test,cl=dia_target,k=20)

##create the confucion matrix
tb <- table(pr,test_target)

##check the accuracy
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(acc_glass)
