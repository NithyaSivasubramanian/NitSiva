getwd() # function we use to know the present working directory
setwd("C://Users//Clustering") # Created a new working directory
library(readxl)
install.packages("writexl")
install.packages("xlsx")
library(xlsx)
install.packages("WriteXLS")

crime_data <- read.csv("C://Users//Assignments//Clustering//crime_Data.csv")
View(crime_data)

normalized_data<-scale(crime_data[,2:5]) #excluding the university name columnbefore normalizing
View(normalized_data)

d <- dist(normalized_data, method = "euclidean") # distance matrix
d

# Model Building
fit <- hclust(d, method="complete")
fit

?hclust
plot(fit) # display dendrogram
plot(fit, hang=-1)
groups <- cutree(fit, k=4) # cut tree into 5 clusters

?cutree
rect.hclust(fit, k=4, border="red")
?rect.hclust

membership<-as.matrix(groups)
table(membership)

groups <- cutree(fit, k=4)
table(groups)
final <- data.frame(crime_data, membership)
View(final)
final <- data.frame(crime_data, membership)
View(final)

install.packages("data.table")
library(data.table)
setcolorder(final,c("membership"))

Crime_Rate_Categ<-as.matrix(groups) # groups or cluster numbers
final <- data.frame(crime_data, Crime_Rate_Categ)

final1 <- final[,c(ncol(final),1:(ncol(final)-1))]

View(final1)

library(WriteXLS)
#C://Users//JayNit//Desktop//Nithya//Data Science//ExcelR//Assignments//Clustering//crime_Data.csv")
write.xlsx(final1, file="clustered_crime_data.xlsx")
setwd("E:/Excelr Data/R Codes/Clustering")
