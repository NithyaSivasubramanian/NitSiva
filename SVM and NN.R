#####Support Vector Machines -------------------
##  Optical Character Recognition ----
#load letterdata as letters
# divide into training and test data
#letters<-read.csv(file.choose(),header = T)
letters<-read.csv("C://Users//JayNit//Desktop//Nithya//Data Science//ExcelR//Assignments//Support Vector Machines//letterdata.csv")
View(letters)
class(letters$letter)
letters$letter <- as.factor(letters$letter)
class(letters$letter)
attach(letters)
library(e1071)
letters_train <- letters[1:16000, ]
letters_test  <- letters[16001:20000, ]
letters_train
?svm

##Training a model on the data ----
# begin by training a simple linear SVM
#install.packages("kernlab")
#library(kernlab)
library(e1071)
letter_classifier <- ksvm(letter~., data = letters_train,kernel = "vanilladot")
letter_classifier <- svm(letter~., data = letters_train,kernel = "linear")
# basic information about the model
letter_classifier

## Evaluating model performance ----
# predictions on testing dataset
letter_predictions <- predict(letter_classifier, letters_test)
View(letters_test)
head(letter_predictions)

table(letter_predictions, letters_test$letter)


agreement <- letter_predictions == letters_test$letter
table(agreement)
prop.table(table(agreement))


## Improving model performance ----
letter_classifier_rbf <- ksvm(letter ~ ., data = letters_train, kernel = "rbfdot")
letter_predictions_rbf <- predict(letter_classifier_rbf, letters_test)

agreement_rbf <- letter_predictions_rbf == letters_test$letter
table(agreement_rbf)
mean(letter_predictions_rbf == letters_test$letter)
prop.table(table(agreement_rbf))
?ksvm

##### Neural Networks -------------------
# Load the Concrete data as concrete

# custom normalization function
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}
View(concrete)

# apply normalization to entire data frame
concrete_norm <- as.data.frame(lapply(concrete, normalize))
View(concrete_norm)

# create training and test data
concrete_train <- concrete_norm[1:773, ]
concrete_test <- concrete_norm[774:1030, ]

## Training a model on the data ----
# train the neuralnet model
install.packages("neuralnet")
library(neuralnet)

# simple ANN with only a single hidden neuron
concrete_model <- neuralnet(formula = strength ~ cement + slag +
                              ash + water + superplastic + 
                              coarseagg + fineagg + age,
                              data = concrete_train)


# visualize the network topology
plot(concrete_model)

## Evaluating model performance 

----
# obtain model results

#results_model <- NULL
concrete_test[1:8]
results_model <- compute(concrete_model, concrete_test[1:8])
results_model
# obtain predicted strength values
str(results_model)
predicted_strength <- results_model$net.result
predicted_strength
# examine the correlation between predicted and actual values
cor(predicted_strength, concrete_test$strength)

## Improving model performance ----
# a more complex neural network topology with 10 hidden neurons
concrete_model2 <- neuralnet(strength ~ cement + slag +
                               ash + water + superplastic + 
                               coarseagg + fineagg + age,
                               data = concrete_train, hidden = c(2,2),algorithm = 'backprop',learningrate = 0.0001,linear.output=F,stepmax=1e+08,act.fct = 'tanh')
?neuralnet


neuralnet# plot the network
plot(concrete_model2)

# evaluate the results as we did before
model_results2 <- compute(concrete_model2, concrete_test[1:8])
predicted_strength2 <- model_results2$net.result
cor(predicted_strength2, concrete_test$strength)



