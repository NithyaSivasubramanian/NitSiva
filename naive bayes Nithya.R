library(readr)
library(e1071)
# Libraries
library(naivebayes)
library(caret)
library(ggplot2)
sal_train <- read.csv("C://Users//JayNit//Desktop//Nithya//Data Science//ExcelR//Assignments//Naive Bayes//SalaryData_Train.csv")
sal_test <- read.csv("C://Users//JayNit//Desktop//Nithya//Data Science//ExcelR//Assignments//Naive Bayes//SalaryData_Test.csv")
#Import the raw_sms dataset 

#sms_raw <- read_csv("E:/Classes/Trainer Tools/Final/12 KNN & Naive Bayes/Data Sets/sms_raw_NB.csv")
View(sal_train)
View(sal_test)
str(sal_train)
str(sal_test
    )
sal_train$educationno <- as.factor(sal_train$educationno)
class(sal_train)
sal_test$educationno <- as.factor(sal_test$educationno)
class(sal_test)

# examine the type variable more carefully
str(sal_train)
#Visualization 
# Plot and ggplot 
ggplot(data=sal_train,aes(x=sal_train$Salary, y = sal_train$age, fill = sal_train$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")


plot(table(sal_train$workclass,sal_train$Salary), type = "h", col = "blue", lwd = 10)
plot(table(sal_train$education,sal_train$Salary), type = "h", col = "green", lwd = 10)
plot(table(sal_train$educationno,sal_train$Salary), type = "h", col = "yellow", lwd = 10)
plot(table(sal_train$maritalstatus,sal_train$Salary), type = "h", col = "light blue", lwd = 10)
plot(table(sal_train$occupation,sal_train$Salary), type = "h", col = "light blue", lwd = 10)
plot(table(sal_train$relationship,sal_train$Salary), type = "h", col = "light green", lwd = 10)

ggplot(data=sal_train,aes(x=sal_train$Salary, y = sal_train$capitalgain, fill = sal_train$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")

ggplot(data=sal_train,aes(x=sal_train$Salary, y = sal_train$capitalloss, fill = sal_train$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")

ggplot(data=sal_train,aes(x=sal_train$Salary, y = sal_train$hoursperweek, fill = sal_train$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")

# density plot
ggplot(data=sal_train,aes(x = sal_train$age, fill = sal_train$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')
ggplot(data=sal_train,aes(x = sal_train$educationno, fill = sal_train$Salary)) +
  geom_density(alpha = 0.9, color = 'Blue')

ggplot(data=sal_train,aes(x = sal_train$native, fill = sal_train$Salary)) +
  geom_density(alpha = 0.9, color = 'Green')

ggplot(data=sal_train,aes(x = sal_train$capitalloss, fill = sal_train$Salary)) +
  geom_density(alpha = 0.9, color = 'Light Blue')
ggplot(data=sal_train,aes(x = sal_train$hoursperweek, fill = sal_train$Salary)) +
  geom_density(alpha = 0.9, color = 'Orange')

# Naive Bayes Model 
# initially i tried the below one. but it did not work. predict did not give any reseults. it gave
# NaN - not a number as reeults. 

# Model <- naiveBayes(sal_train$Salary ~ ., data = sal_train)
# then i tried this one. as.factor for salary as naive bayes does not work properly for nonfactored
Model <- naiveBayes(as.factor(sal_train$Salary) ~ ., data = sal_train)
Model
Model_pred <- predict(Model,sal_test)
mean(Model_pred==sal_test$Salary)
confusionMatrix(Model_pred,as.factor(sal_test$Salary))
acc <- NULL
acc<-c(acc,mean(Model_pred==sal_test[,3]))
acc



# naive bayes assignment 2. 
# data set for classifying the ham and spam
#Import the raw_sms dataset 

library(readr)
sms_raw <- read.csv("C://Users//JayNit//Desktop//Nithya//Data Science//ExcelR//Assignments//Naive Bayes//sms_raw_NB1.csv")
View(sms_raw)

sms_raw$type <- factor(sms_raw$type)

# examine the type variable more carefully
str(sms_raw$type)
table(sms_raw$type)

# build a corpus using the text mining (tm) package
install.packages("tm")
library(tm)
sms_corpus <- Corpus(VectorSource(sms_raw$text))

sms_corpus <- tm_map(sms_corpus, function(x) iconv(enc2utf8(x), sub='byte'))

# clean up the corpus using tm_map()
corpus_clean <- tm_map(sms_corpus, tolower)
corpus_clean <- tm_map(corpus_clean, removeNumbers)
corpus_clean <- tm_map(corpus_clean, removeWords, stopwords())
corpus_clean <- tm_map(corpus_clean, removePunctuation)
corpus_clean <- tm_map(corpus_clean, stripWhitespace)


# create a document-term sparse matrix
sms_dtm <- DocumentTermMatrix(corpus_clean)
sms_dtm

# creating training and test datasets
sms_raw_train <- sms_raw[1:4169, ]
sms_raw_test  <- sms_raw[4170:5559, ]

sms_dtm_train <- sms_dtm[1:4169, ]
sms_dtm_test  <- sms_dtm[4170:5559, ]

sms_corpus_train <- corpus_clean[1:4169]
sms_corpus_test  <- corpus_clean[4170:5559]

# check that the proportion of spam is similar
prop.table(table(sms_raw_train$type))
prop.table(table(sms_raw_test$type))


# indicator features for frequent words
# dictionary of words which are used more than 5 times
sms_dict <- findFreqTerms(sms_dtm_train, 5)

sms_train <- DocumentTermMatrix(sms_corpus_train, list(dictionary = sms_dict))
sms_test  <- DocumentTermMatrix(sms_corpus_test, list(dictionary = sms_dict))


# convert counts to a factor
# custom function: if a word is used more than 0 times then mention 1 else mention 0
convert_counts <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
}

# apply() convert_counts() to columns of train/test data
# Margin = 2 is for columns
# Margin = 1 is for rows
sms_train <- apply(sms_train, MARGIN = 2, convert_counts)
sms_test  <- apply(sms_test, MARGIN = 2, convert_counts)

##  Training a model on the data ----
install.packages("e1071")
library(e1071)
sms_classifier <- naiveBayes(sms_train, sms_raw_train$type)
sms_classifier

##  Evaluating model performance ----
sms_test_pred <- predict(sms_classifier, sms_test)

table(sms_test_pred)
prop.table(table(sms_test_pred))

library(gmodels)
CrossTable(sms_test_pred, sms_raw_test$type,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))


library(wordcloud)

spam = subset(sms_raw_train, type == "spam")
ham  = subset(sms_raw_test, type == "ham")

wordcloud(spam$text,
          max.words=80,     # look at the 40 most common words
          scale=c(3, 0, 5),
          random.order = F, colors=rainbow(20)) # adjust max and min font sizes for words shown

wordcloud(ham$text,colors=rainbow(20),
          max.words=80,     # look at the 40 most common words
          random.order = F) # adjust max and min font sizes for words shown
###########################################
#ggplot(aes(x=sms_raw),sms_raw),geom_bar(fill="red",width=0.5)

#ggplot(data=sms_raw,aes(x = sms_raw$type)) +
#  geom_density(alpha = 0.9, color = 'Light Blue')


#sms_classifier2 <- naiveBayes(sms_train, sms_raw_train$type, laplace = 1)
#sms_test_pred2 <- predict(sms_classifier2, sms_test)

#CrossTable(sms_test_pred2, sms_raw_test$type,
#           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
#           dnn = c('predicted', 'actual'))
