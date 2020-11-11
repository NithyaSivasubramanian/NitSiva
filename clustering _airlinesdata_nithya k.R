getwd() # function we use to know the present working directory
setwd("C://Data Science//ExcelR//Assignments//Clustering") # Created a new working directory
library(readxl)
install.packages("writexl")
install.packages("xlsx")
library(xlsx)
install.packages("WriteXLS")
install.packages("dendextend")
install.packages("dendextendRcpp")
library("dendextend")
library("dendextendRcpp")
install.packages("gridExtra")
install.packages("cluster")
install.packages("factoextra")
install.packages("MASS")
install.packages("fpc")

library("gridExtra")
library("cluster")
library("factoextra")
library("MASS")
library("fpc")

install.packages("Stats")
install.packages("NbClust")
library(NbClust)

air_data <- read.xlsx("C://Data Science//ExcelR//Assignments//Clustering//EastWestAirlines.xlsx",2)
View(air_data)

norm_Airdata<-scale(air_data[,2:11]) #excluding the university name columnbefore normalizing
View(norm_Airdata)

dair <- dist(norm_Airdata, method = "euclidean") # distance matrix
dair

# Model Building
fit <- hclust(dair, method="complete")

fit

?hclust
plot(fit) # display dendrogram
plot(fit, hang=-1)
airgroups <- cutree(fit, k=4) # cut tree into 5 clusters


?cutree
rect.hclust(fit,k=4, border="red")
#res.hc <- hclust(fit)
  #rect.hclust(fit, method = "ward.D2")
?rect.hclust

#Dend1   <- as.dendrogram(res.hc)

membership<-as.matrix(airgroups)
table(membership)

airgroups <- cutree(fit, k=4)
table(airgroups)
final <- data.frame(air_data, membership)
View(final)
#final <- data.frame(crime_data, membership)
#View(final)

install.packages("data.table")
library(data.table)
setcolorder(final,c("membership"))

airlines_Categ<-as.matrix(airgroups) # groups or cluster numbers
final <- data.frame(air_data, airlines_Categ)

final1 <- final[,c(ncol(final),1:(ncol(final)-1))]

View(final1)

library(WriteXLS)
#C://ExcelR//Assignments//Clustering//crime_Data.csv")
write.xlsx(final1, file="clustered_airlines_data.xlsx")
setwd("E:/Excelr Data/R Codes/Clustering")



# kmeans clustering

install.packages("plyr")
library(plyr)
plot(norm_Airdata)

plot(norm_Airdata, type="n")
text(norm_Airdata, rownames(norm_Airdata))
fitK <- kmeans(norm_Airdata, 3)

str(fitK)
fitK$withinss

install.packages("animation")
library(animation)

fitK <- kmeans.ani(norm_Airdata, 4)
fitK$size


#elbow curve & k ~ sqrt(n/2) to decide the k value

wss = (nrow(norm_Airdata)-1)*sum(apply(norm_Airdata, 2, var))		 # Determine number of clusters by scree-plot 
for (i in 2:8) wss[i] = sum(kmeans(norm_Airdata, centers=i)$withinss)
plot(1:8, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Screen-Plot")


# selecting K for kmeans clustering using kselection
install.packages("kselection")
library(kselection)
data(norm_Airdata)
View(norm_Airdata)
k <- kselection(norm_Airdata[,-5], parallel = TRUE, k_threshold = 0.9, max_centers=12)
k$max_centers


install.packages("doParallel")
library(doParallel)
registerDoParallel(cores=4)
k <- kselection(norm_Airdata[,-5], parallel = TRUE, k_threshold = 0.9, max_centers=12)

k

# model Building
fit <- kmeans(norm_Airdata, 5) # 5 cluster solution
fit$cluster
final2<- data.frame(norm_Airdata, fit$cluster) # append cluster membership
View(final2)
library(data.table)
setcolorder(final2, neworder = c("fit.cluster"))
View(final2)
aggregate(norm_Airdata[,2:7], by=list(fit$cluster), FUN=mean)
fit$size


# k clustering alternative for large dataset - Clustering Large Applications (Clara)
install.packages("cluster")
library(cluster)
xds <- rbind(cbind(norm_Airdata))
xcl <- clara(xds, 2, sample = 100)
clusplot(xcl)

#Partitioning around medoids
xpm <- pam(xds, 2)
clusplot(xpm)


library(WriteXLS)
#C://ExcelR//Assignments//Clustering//crime_Data.csv")
write.xlsx(final2, file="Kmeans_clustered_airline_data.xlsx")
setwd("E:/Excelr Data/R Codes/Clustering")
