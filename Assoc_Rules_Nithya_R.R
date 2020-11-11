library(arules)
library(arulesViz)
install.packages("rmarkdown")
library(rmarkdown)
#book<-read.csv(file.choose())
movies_data <- read.csv("C://users//JayNit//Desktop//Nithya//Data Science//ExcelR//Assignments//Association Rules//my_movies.csv")
View(movies_data)
movies_trans<-as(as.matrix(movies_data[,6:15]),"transactions")
inspect(movies_trans[1:100])
# If we inspect book_trans
# we should get transactions of items i.e.
# As we have 2000 rows ..so we should get 2000 transactions 
# Each row represents one transactions 
# After converting the binary format of data frame from matrix to transactions
# Perform apriori algorithm 
rules<-apriori(as.matrix(movies_data[,6:15],parameter=list(support=0.2, confidence = 0.5,minlen=5)))
rules
inspect(rules)
inspect(head(sort(rules, by = "lift")))  
head(quality(rules))
# Apriori algorithm 
plot(rules)

plot(rules, method = "grouped")
plot(rules,method = "graph")

library(readxl)
setwd("C://Users//JayNit//Desktop//Nithya//Data Science//ExcelR//Assignments//Association Rules") # Created a new working directory
write(rules, file = "Movierules.csv",
      sep = ",", quote = TRUE, row.names = FALSE)

# Whenever we have binary kind of data .....load them as csv and convert them into 
# matrix format using as.matrix(data) and use this for forming 
# Association rules and change the values of support,confidence, and minlen 
# to get different rules 


# Whenever we have data containing item names, then load that data using 
# read.transactions(file="path",format="basket",sep=",")
# use this to form association rules 
#-------------------------------------------------------------------------------

# groceries data set
groceries <- read.transactions("C://users//JayNit//Desktop//Nithya//Data Science//ExcelR//Assignments//Association Rules//groceries1.csv", sep=",", format="basket")
summary(groceries)
inspect(groceries[1:10])
class(groceries)
itemFrequencyPlot(groceries)
groceries_rules<-apriori(groceries,parameter = list(support = 0.002,confidence = 0.05,minlen=3))



library(arulesViz)
plot(groceries_rules)

library(arules)
data("Groceries")
summary(Groceries)
rules <- apriori(Groceries)

itemFrequency(groceries[, 1:3])
itemFrequencyPlot(groceries, support = 0.1)
itemFrequencyPlot(groceries, topN = 20)

image(groceries[1:5])
image(sample(groceries, 100))

groceryrules1 <- apriori(groceries, parameter = list(support =
                                                      0.006, confidence = 0.25, minlen = 2))
summary(groceryrules1)

inspect(groceryrules1[1:3])
groceryrules1

inspect(sort(groceryrules1, by = "lift")[1:5])

berryrules <- subset(groceryrules1, items %in% "berries")
inspect(berryrules)
library(readxl)
setwd("C://Users//JayNit//Desktop//Nithya//Data Science//ExcelR//Assignments//Association Rules") # Created a new working directory
write(groceryrules1, file = "groceryrules.csv",
      sep = ",", quote = TRUE, row.names = FALSE)
#-------------------------------------------------------------------------------

# assignment with book data

#book<-read.csv(file.choose())
book_data <- read.csv("C://users//JayNit//Desktop//Nithya//Data Science//ExcelR//Assignments//Association Rules//book1.csv")
View(book_data)
#movies_trans<-as(as.matrix(book_data[,6:15]),"transactions")
#inspect(movies_trans[1:100])
# If we inspect book_trans
# we should get transactions of items i.e.
# As we have 2000 rows ..so we should get 2000 transactions 
# Each row represents one transactions 
# After converting the binary format of data frame from matrix to transactions
# Perform apriori algorithm 
#rules<-apriori(as.matrix(movies_data[,6:15],parameter=list(support=0.2, confidence = 0.5,minlen=5)))

rules <- apriori(as.matrix(book_data),parameter=list(support=0.02, confidence = 0.5,minlen=5))
rules
inspect(rules)
inspect(head(sort(rules, by = "lift")))  
head(quality(rules))
# Apriori algorithm 
plot(rules)

plot(rules, method = "grouped")
plot(rules,method = "graph")

library(readxl)
setwd("C://Users//JayNit//Desktop//Nithya//Data Science//ExcelR//Assignments//Association Rules") # Created a new working directory
write(rules, file = "Bookrules.csv",
      sep = ",", quote = TRUE, row.names = FALSE)
View(rules)
