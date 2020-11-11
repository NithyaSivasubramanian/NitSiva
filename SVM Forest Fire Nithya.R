library(e1071)
library(ggplot2)
forestfires<-read.csv("C://Users//data Science//ExcelR//Assignments//Support Vector Machines//forestfires.csv")

View(forestfires)
str(forestfires)
forestfires$month <- as.factor(forestfires$month)
forestfires$day <- as.factor(forestfires$day)
forestfires$size_category <- as.factor(forestfires$size_category)

forfire_train <- forestfires[1:413, ]
forfire_test  <- forestfires[414:517, ]
str(forestfires)

#Visualization 
# Plot and ggplot 
ggplot(data=forfire_train,aes(x=forfire_train$size_category, y = forfire_train$temp, fill = forfire_train$size_category)) +
  geom_boxplot() +
  ggtitle("Box Plot")

ggplot(data=forfire_train,aes(x=forfire_train$size_category, y = forfire_train$wind, fill = forfire_train$size_category)) +
  geom_boxplot() +
  ggtitle("Box Plot")

ggplot(data=forfire_train,aes(x=forfire_train$size_category, y = forfire_train$rain, fill = forfire_train$size_category)) +
  geom_boxplot() +
  ggtitle("Box Plot")

#Density Plot 

ggplot(data=forfire_train,aes(x = forfire_train$temp, fill = forfire_train$size_category)) +
  geom_density(alpha = 0.9, color = 'Violet')

ggtitle("Age - Density Plot")

ggplot(data=forfire_train,aes(x = forfire_train$month, fill = forfire_train$size_category)) +
  geom_density(alpha = 0.9, color = 'Violet')


  

# Building model 


model1<-ksvm(forfire_train$size_category~., 
             data= forfire_train, kernel = "vanilladot")
model1
size_prediction <- predict(model1, forfire_test)

table(size_prediction,forfire_test$size_category)
size_pred <- size_prediction == forfire_test$size_category
table(size_pred)
mean(size_prediction == forfire_test$size_category)

prop.table(table(size_pred))

# Different types of kernels 
# "rbfdot", "polydot", "tanhdot", "vanilladot", "laplacedot", 
# "besseldot", "anovadot", "splinedot", "matrix"

# kernel = rfdot 
model_rfdot<-ksvm(forfire_train$size_category~., 
                  data= forfire_train,kernel = "rbfdot")
pred_rfdot<-predict(model_rfdot,newdata=forfire_test)
mean(pred_rfdot==forfire_test$size_category) # 85.19

prop.table(table(pred_rfdot))
# kernel = vanilladot
model_vanilla<-ksvm(forfire_train$size_category~., 
                    data= forfire_train,kernel = "vanilladot")
pred_vanilla<-predict(model_vanilla,newdata=forfire_test)
mean(pred_vanilla==forfire_test$size_category) # 84.64
prop.table(table(pred_vanilla))
