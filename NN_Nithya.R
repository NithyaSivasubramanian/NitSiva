setwd("D:\\ML\\R\\Nueral Networks")
library(readr)
concrete <- read.csv("C://Users//JayNit//Desktop//Nithya//Data Science//ExcelR//Assignments//Neural Networks//concrete1.csv")
View(concrete)
str(concrete)
attach(concrete)
#normal_concrete<-scale(concrete)
## or 
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
concrete_norm<-as.data.frame(lapply(concrete,FUN=normalize))
summary(concrete_norm$strength)
#summary(normal_concrete)
summary(concrete$strength)
concrete_train<-concrete_norm[1:773,]
concrete_test<-concrete_norm[774:1030,]

# Using multilayered feed forward nueral network
# package nueralnet
install.packages("neuralnet")
install.packages("nnet")
library(neuralnet)
library(nnet)
library(caret)
# Building model
concrete_model <- neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age,data = concrete_train)
str(concrete_model)
plot(concrete_model)

# SSE sum of squared errors . least SSE best model
# Evaluating model performance
# compute function to generate ouput for the model prepared
model_results <- compute(concrete_model,concrete_test[1:8])
predicted_strength <- model_results$net.result
predicted_strength
model_results$neurons
cor(predicted_strength,concrete_test$strength)
plot(predicted_strength,concrete_test$strength)
model_5<-neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age,data= concrete_norm,hidden = 5)
plot(model_5)
model_5_res<-compute(model_5,concrete_test[1:8])
pred_strn_5<-model_5_res$net.result
cor(pred_strn_5,concrete_test$strength)
plot(pred_strn_5,concrete_test$strength)
# SSE has reduced and training steps had been increased as the number of nuerons 
# under hidden layer are increased

######Assignment 1.	Build a Neural Network model for 50_startups data to predict profit
library(neuralnet)
library(nnet) 
library(NeuralNetTools)
library(plyr)
Startups <- read.csv("C://Users//JayNit//Desktop//Nithya//Data Science//ExcelR//Assignments//Neural Networks//50_Startups.csv")

View(Startups)
class(Startups)
str(Startups)
Startups$State <- as.numeric(revalue(Startups$State,
                                     c("New York"="0", "California"="1",
                                       "Florida"="2")))
str(Startups)

Startups <- as.data.frame(Startups)
attach(Startups)

plot(R.D.Spend, Profit)
plot(Administration, Profit)
plot(Marketing.Spend, Profit)
plot(State, Profit)
window()
pairs(Startups)
# Correlation coefficient - Strength & Direction of correlation
cor(Startups)
summary(Startups) # Confirms on the different scale and demands normalizing the data

# Apply Normalization technique to the whole dataset :

normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
Startups_norm<-as.data.frame(lapply(Startups,FUN=normalize))
summary(Startups_norm$Profit) # Normalized form of profit
summary(Startups$profit) # Orginal profit value

# Data Partition 
set.seed(123)
ind <- sample(2, nrow(Startups_norm), replace = TRUE, prob = c(0.7,0.3))
Startups_train <- Startups_norm[ind==1,]
startups_test  <- Startups_norm[ind==2,]

# Creating a neural network model on training data


startups_model <- neuralnet(Profit~R.D.Spend+Administration
                            +Marketing.Spend+State,data = Startups_train)
str(startups_model)

plot(startups_model, rep = "best")
summary(startups_model)

par(mar = numeric(4), family = 'serif')
plotnet(startups_model, alpha = 0.6)

# Evaluating model performance

set.seed(12323)
model_results <- compute(startups_model,startups_test[1:4])
predicted_profit <- model_results$net.result

# Predicted profit Vs Actual profit of test data.
cor(predicted_profit,startups_test$Profit)

# since the prediction is in Normalized form, we need to de-normalize it 
# to get the actual prediction on profit
str_max <- max(Startups$Profit)
str_min <- min(Startups$Profit)

unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}

ActualProfit_pred <- unnormalize(predicted_profit,str_min,str_max)
head(ActualProfit_pred)

# Improve the model performance :
set.seed(12345)
Startups_model2 <- neuralnet(Profit~R.D.Spend+Administration
                             +Marketing.Spend+State,data = Startups_train,
                             hidden = 2)
plot(Startups_model2 ,rep = "best")

summary(Startups_model2)

model_results2<-compute(Startups_model2,startups_test[1:4])
predicted_Profit2<-model_results2$net.result
cor(predicted_Profit2,startups_test$Profit)
plot(predicted_Profit2,startups_test$Profit)

par(mar = numeric(4), family = 'serif')
plotnet(Startups_model2, alpha = 0.6)


####### 2.	PREDICT THE BURNED AREA OF FOREST FIRES WITH NEURAL NETWORKS

forestfire <- read.csv("C://Users//JayNit//Desktop//Nithya//Data Science//ExcelR//Assignments//Neural Networks//forestfires.csv")
View(forestfire)
str(forestfire)
attach(forestfire)
library(plyr)
forestfire$month <- as.numeric(revalue(forestfire$month,
                                     c("jan"="1", "feb"="2","mar"="3","apr"="4","may"="5",
                                       "jun"="6","jul"="7","aug"="8","sep"="9","oct"="10",
                                       "nov"="11","dec"="12")))
forestfire$day <- as.numeric(revalue(forestfire$day,                                      
                                         c("mon"="1","tue"="2","wed"="3","thu"="4","fri"="5",
                                         "sat"="6","sun"="0")))
forestfire$size_category <- as.numeric(revalue(forestfire$size_category,                                      
                                     c("small"="1","large"="2")))
#forestfire$month <- as.factor(forestfire$month)
#forestfire$day <- as.factor(forestfire$day)
#forestfire$size_category <- as.factor(forestfire$size_category)
#normal_concrete<-scale(concrete)
## or 

normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
forest_norm<-as.data.frame(lapply(forestfire,FUN=normalize))
summary(forest_norm$size_category)
#summary(normal_concrete)
summary(forestfire$size_category)
forestfire_train<-forest_norm[1:415,]
forestfire_test<-forest_norm[416:518,]

# Using multilayered feed forward nueral network
# package nueralnet
install.packages("neuralnet")
install.packages("nnet")
library(neuralnet)
library(nnet)
library(caret)
library(igraph)
# Building model
forest_model <- neuralnet(size_category~month+day+FFMC+DMC+DC+ISI+temp+RH+
                            wind+rain+area+dayfri+daymon+daysat+daysun+
                            daythu+daytue+daywed+monthapr+monthjan+monthfeb+
                            monthmar+monthjun+monthjul+monthaug+monthsep+monthoct+
                            monthnov+monthdec,data = forestfire_train)
str(forest_model)
plot(forest_model)

# SSE sum of squared errors . least SSE best model
# Evaluating model performance
# compute function to generate ouput for the model prepared
model_results <- compute(forest_model,forestfire_test[1:30])
predicted_strength <- model_results$net.result
predicted_strength
model_results$neurons
cor(predicted_strength,forestfire_test$strength)
plot(predicted_strength,forestfire_test$strength)

model_5<-neuralnet(size_category~month+day+FFMC+DMC+DC+ISI+temp+RH+
                     wind+rain+area,data= forest_norm,hidden = 5)
plot(model_5)
model_5_res<-compute(model_5,forestfire_test[1:30])
pred_strn_5<-model_5_res$net.result
cor(pred_strn_5,forestfire_test$strength)
plot(pred_strn_5,forestfire_test$strength)
# SSE has reduced and training steps had been increased as the number of nuerons 
# under hidden layer are increased
library(ggvis) #Data visulization
library(psych) #Scatterplot matrix
library(knitr) #html table
library(neuralnet) #artifical neural network
library(nnet)
install.packages("NeuralNetTools")
library(NeuralNetTools)
par(mar = numeric(4), family = 'serif')
plotnet(model_5, alpha = 0.6)

forest_model2 <- neuralnet(size_category~month+day+FFMC+DMC+DC+ISI+temp+RH+
                            wind+rain+area+dayfri+daymon+daysat+daysun+
                            daythu+daytue+daywed+monthapr+monthjan+monthfeb+
                            monthmar+monthjun+monthjul+monthaug+monthsep+monthoct+
                            monthnov+monthdec,data= forest_norm,hidden = 5)
str(forest_model2)
plot(forest_model2)

# SSE sum of squared errors . least SSE best model
# Evaluating model performance
# compute function to generate ouput for the model prepared
model2_results <- compute(forest_model2,forestfire_test[1:30])
predicted_strength2 <- model2_results$net.result
predicted_strength2
model2_results$neurons
cor(predicted_strength2,forestfire_test$strength)
plot(predicted_strength2,forestfire_test$strength)
library(NeuralNetTools)
par(mar = numeric(4), family = 'serif')
plotnet(forest_model2, alpha = 0.6)

####### Assignment 3. Apply neural network on concrete data
library(ggvis) #Data visulization
library(psych) #Scatterplot matrix
library(knitr) #html table
library(neuralnet) #artifical neural network 
concrete <- read.csv("C://Users//JayNit//Desktop//Nithya//Data Science//ExcelR//Assignments//Neural Networks//concrete.csv")

knitr::kable(head(concrete), caption = "Partial Table Preview")
str(concrete)
concrete %>% ggvis(x = ~strength, fill:= "#27bc9c") %>% layer_histograms() %>% layer_paths(y = ~strength, 35.82, stroke := "green")
pairs.panels(concrete[c("cement", "slag", "ash", "strength")])


normalize <- function(x){
  return ((x - min(x))/(max(x) - min(x) ))
}

concrete_norm <- as.data.frame(lapply(concrete, normalize))

kable(round(head(concrete_norm), digits = 3), caption = "Normalized Data Preview")

#training set
concrete_train <- concrete_norm[1:773, ]

#test set
concrete_test <- concrete_norm[774:1030, ]
#Build a neural network with one hidden layer 
concrete_model <- neuralnet(strength ~ cement + slag + ash + water + superplastic + coarseagg + fineagg + age , data = concrete_train, hidden = 1)
plot(concrete_model)
#building the predictor, exclude the target variable column
model_results <- compute(concrete_model, concrete_test[1:8])

#store the net.results column 
predicted_strength <- model_results$net.result
cor(predicted_strength, concrete_test$strength)
#building the new model
concrete_model2 <- neuralnet(strength ~ cement + slag + ash + water + superplastic + coarseagg + fineagg + age, data = concrete_train, hidden = 5 )
#nuilding the new predictor
model_results2 <- compute(concrete_model2, concrete_test[1:8])
plot(concrete_model2)
#storing the results
predicted_strength2 <- model_results2$net.result
cor(predicted_strength2, concrete_test$strength)

library(NeuralNetTools)
par(mar = numeric(4), family = 'serif')
plotnet(concrete_model2, alpha = 0.6)
