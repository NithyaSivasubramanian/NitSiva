R.version.string
#Installing and loading the libraries
install.packages("recommenderlab", dependencies=TRUE)
install.packages("curl")
library("recommenderlab")
library(curl)
library(corrplot)
library(caTools)
library(methods)
library(Matrix)
library(tidyr)


library(stringr)
library(tidyverse)
library(installr)
library(DT)
updateR()
#movie rating data
textbook_data <- read.csv("C://Users//Assignments//Recommendation//book.csv")
View(textbook_data)
textbook_data <- textbook_data[,-1]
#metadata about the variable
str(textbook_data)
class(textbook_data)
#textbook_data$User.ID<-as.factor(textbook_data$User.ID)
#textbook_data$Book.Rating<-as.numeric(textbook_data$Book.Rating)

#rating distribution
hist(textbook_data$Book.Rating)

#the datatype should be realRatingMatrix inorder to build recommendation engine
textbook_data_matrix <- as(textbook_data, 'realRatingMatrix')

#Popularity based 

book_recomm_model1 <- Recommender(textbook_data_matrix, method="POPULAR")

#Predictions for two users 
recommended_items1 <- predict(book_recomm_model1, textbook_data_matrix[45:46], n=5)
as(recommended_items1, "list")


## Popularity model recommends the same movies for all users , we need to improve our model using # # Collaborative Filtering

#User Based Collaborative Filtering
 


textbook_data_mat <- textbook_data_matrix[1:50]


book_recomm_model2 <- Recommender(textbook_data_matrix, method="UBCF")
View(book_recomm_model2)
class(textbook_data_matrix)
class(book_recomm_model2)
dim(textbook_data_matrix)
#dim(textbook_data_mat)[1]
dim(textbook_data_mat)[2]
dim(book_recomm_model2)
recommended_items2 <- predict(book_recomm_model2, textbook_data_matrix[41:42], n=3)
as(recommended_items2, "list")


##### running this from https://rpubs.com/javernw/jwDATA612wk2Proj2
library(tidyverse)
library(Matrix)
library(recommenderlab)
library(kableExtra)
library(gridExtra)
textbook_data$User.ID <- as.factor(textbook_data$User_id)
textbook_data$X <- as.factor(textbook_data$X)

dim(textbook_data)
textbook_data <- textbook_data[,-1]
object.size(textbook_data)
bmatrix <- as(textbook_data, "realRatingMatrix")
dim(bmatrix@data)
sim <- similarity(bmatrix[1:10, ], method = "cosine", which = "users")
image(as.matrix(sim), main = "User Similarity")


sim2 <- similarity(bmatrix[ ,1:10], method = "cosine", which = "items")
image(as.matrix(sim2), main = "Item Similarity")

# users who rated at least 100 books and books rated at least 100 times
bmatrix1 <- bmatrix[rowCounts(bmatrix) > 3, colCounts(bmatrix) > 3]
 
bmatrix1
tbl_ratings <- as.data.frame(table(as.vector(bmatrix1@data)))
tbl_ratings

tbl_ratings <- tbl_ratings[-1,] #0 means missing values so remove missing values
ggplot(tbl_ratings, aes(x = Var1, y = Freq, fill = Var1)) + geom_bar(stat = "identity") + ggtitle("Distribution of Book Ratings")

train <- sample(x = c(T, F), size = nrow(bmatrix), replace = T, prob = c(0.8, 0.2)) 
books_train <- bmatrix[train, ] 
books_test <- bmatrix[-train, ]



Imodel <- Recommender(data = books_train, method = "POPULAR")
Imodel
Ipredict <- predict(Imodel, newdata = books_train[5:6], n = 4)
# %>% list()
Ipredict
as(Ipredict, "list")
popular <- table(unlist(Ipredict[[1]]@items)) %>% barplot(main = "Distribution of the number of items for POPULAR")

# IBCF and UBCF are not working. 

Imodel1 <- Recommender(data = books_test, method = "IBCF")
Imodel1
Ipredict1 <- predict(Imodel1, newdata = books_test[5:6], n = 4)
# %>% list()
Ipredict1
