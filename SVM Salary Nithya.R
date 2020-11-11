attach(letters)
library(e1071)
library(ggplot2)
train_sal<-read.csv("C://Users//JayNit//Desktop//Nithya//Data Science//ExcelR//Assignments//Support Vector Machines//SalaryData_Train(1).csv")
test_sal<-read.csv("C://Users//JayNit//Desktop//Nithya//Data Science//ExcelR//Assignments//Support Vector Machines//SalaryData_Test(1).csv")
View(train_sal)
str(train_sal)
train_sal$educationno <- as.factor(train_sal$educationno)
train_sal$Salary <- as.factor(train_sal$Salary)
test_sal$Salary <- as.factor(test_sal$Salary)
class(train_sal)
str(test_sal)
test_sal$educationno <- as.factor(test_sal$educationno)
class(test_sal)
#Visualization 
# Plot and ggplot 
ggplot(data=train_sal,aes(x=train_sal$Salary, y = train_sal$age, fill = train_sal$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")

ggplot(data=train_sal,aes(x=train_sal$Salary, y = train_sal$capitalgain, fill = train_sal$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")

ggplot(data=train_sal,aes(x=train_sal$Salary, y = train_sal$capitalloss, fill = train_sal$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")

ggplot(data=train_sal,aes(x=train_sal$Salary, y = train_sal$hoursperweek, fill = train_sal$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")
#Density Plot 

ggplot(data=train_sal,aes(x = train_sal$age, fill = train_sal$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')

ggtitle("Age - Density Plot")

ggplot(data=train_sal,aes(x = train_sal$workclass, fill = train_sal$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')

ggtitle("Workclass Density Plot")
ggplot(data=train_sal,aes(x = train_sal$education, fill = train_sal$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')

ggtitle("Capitalgain Density Plot")
ggplot(data=train_sal,aes(x = train_sal$capitalloss, fill = train_sal$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')


# Building model 


model1<-ksvm(train_sal$Salary~., 
             data= train_sal, kernel = "vanilladot")
model1
Salary_prediction <- predict(model1, test_sal)

table(Salary_prediction,test_sal$Salary)
agreement <- Salary_prediction == test_sal$Salary
table(agreement)
mean(Salary_prediction == test_sal$Salary)

prop.table(table(agreement))

# Different types of kernels 
# "rbfdot", "polydot", "tanhdot", "vanilladot", "laplacedot", 
# "besseldot", "anovadot", "splinedot", "matrix"

# kernel = rfdot 
model_rfdot<-ksvm(train_sal$Salary~., 
                  data= train_sal,kernel = "rbfdot")
pred_rfdot<-predict(model_rfdot,newdata=test_sal)
mean(pred_rfdot==test_sal$Salary) # 85.19

# kernel = vanilladot
model_vanilla<-ksvm(train_sal$Salary~., 
                    data= train_sal,kernel = "vanilladot")
pred_vanilla<-predict(model_vanilla,newdata=test_sal)
mean(pred_vanilla==test_sal$Salary) # 84.64
