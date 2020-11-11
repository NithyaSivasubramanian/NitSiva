# Read the dataset
#zoo dataset
library(readxl)
zoo <- read.csv("C://Users//JayNit//Desktop//Nithya//Data Science//ExcelR//Assignments//KNN//Zoo.csv")
#zoo <- read_excel(file.choose())
zoo <- as.data.frame(zoo)
class(zoo)
View(zoo)
table(zoo$domestic)
#First colum in dataset is id which is not required so we will be taking out
#zoo <- zoo[-1]
View(zoo)
str(zoo)
# target variable should be charactor. not factor 
#table of diagonis B <- 357 and M <- 212
table(zoo$domestic)

zoo$domestic <- factor(zoo$domestic, levels = c(0,1), labels = c("Wild Animal/Game Animal","Domestic Animal"))
str(zoo)
zoo1 <- zoo[,-c(1,16)]
# Replace types with the values
#zoo$Type <- factor(zoo$Type, levels = c(1,2,3,4,5,6,7), 
#              labels = c("building_windows_float_processed","building_windows_non_float_processed",
#              "vehicle_windows_float_processed","vehicle_windows_non_float_processed",
#              "containers","tableware","headlamps"))                 


# table or proportation of enteries in the datasets. What % of entry is Bengin and % of entry is Malignant
round(prop.table(table(zoo$domestic))*100,1)
# rounding to percentage 
summary(zoo)

#Create a function to normalize the data
#defining a functin norm
norm <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}
#test normalization
norm(c(1,2,3,4,5))
norm(c(10,20,30,40,50))


#Apply the normalization function to zoo dataset
zoo_n <- as.data.frame(lapply(zoo1[1:16], norm))
View(zoo_n)

summary(zoo_n[c("RI","Na","Mg")])

#create training and test datasets
#zoo_train <- zoo_n[1:150,]
#zoo_test <- zoo_n[151:214,]
#create training and test datasets

zoo_train <- zoo_n[1:81,]
zoo_test <- zoo_n[82:101,]


#Get labels for training and test datasets

#zoo_train_labels <- zoo[1:150,1]
#zoo_test_labels <- zoo[151:214,1]
#Get labels for training and test datasets
zoo_train_labels <- zoo[1:81,16]
zoo_test_labels <- zoo[82:101,16]

# Build a KNN model on taining dataset
library("class")
library("caret")
# Building the KNN model on training dataset and also need labels which we are including c1. Once we build the preduction model
# we have to test on test dataset
#zoo_pred <- knn(train = zoo_train, test = zoo_test, cl = zoo_train_labels, k=21)

zoo_test_pred <- knn(train = zoo_train, test = zoo_test, cl = zoo_train_labels, k=3)
table(zoo_test_pred,zoo_test_labels)
acc_zoo <- table(zoo_test_pred,zoo_test_labels)
class(zoo_train)
class(zoo_test)

mean(zoo_test_pred==zoo_test_labels)

## Now evualuation the model performance

# install package gmodels
install.packages("gmodels")
library("gmodels")

# Create cross table of predicted and actual
# Evaluating Model Performance ----

# load the gmodel library

CrossTable(x=zoo_test_labels,y=zoo_test_pred,prop.chisq = FALSE) 
#CrossTable( x =  zoo_test_labels, y = zoo_pred)

# to check  the accuracy
##check the accuracy
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(acc_zoo)
