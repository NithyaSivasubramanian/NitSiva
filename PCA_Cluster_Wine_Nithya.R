setwd("C://Users//Assignments//PCA") # Created a new working directory
library(cluster)
library(factoextra)
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
library(stats)
library(NbClust)

wine_data <- read.csv("C://Users//Assignments//PCA//wine.csv")
View(wine_data)
help(princomp) ## to understand the api for princomp

attach(wine_data)
cor(wine_data)
pcaObj<-princomp(wine_data[-1], cor = TRUE, scores = TRUE, covmat = NULL)

## princomp(mydata, cor = TRUE) not_same_as prcomp(mydata, scale=TRUE); similar, but different
summary(pcaObj)
str(pcaObj)
loadings(pcaObj)


plot(pcaObj) # graph showing importance of principal components 
# Comp.1 having highest importance (highest variance)

biplot(pcaObj)

# Showing the increase of variance with considering principal components
# Which helps in choosing number of principal components
plot(cumsum(pcaObj$sdev*pcaObj$sdev)*100/(sum(pcaObj$sdev*pcaObj$sdev)),type="b")

#pcaObj$loadings

pcaObj$scores[,1:3] # Top 3 PCA Scores which represents the whole data


# cbind used to bind the data in column wise
# Considering top 3 principal component scores and binding them with wine data
mydata<-cbind(wine_data,pcaObj$scores[,1:3])
View(mydata)

# preparing data for clustering (considering only pca scores as they represent the entire data)
clus_data<-mydata[,15:17]
View(clus_data)
# Normalizing the data 
norm_clus<-scale(clus_data) # Scale function is used to normalize data
dist_wine<-dist(norm_clus,method = "euclidean") # method for finding the distance
# here I am considering Euclidean distance

# Clustering the data using hclust function --> Hierarchical
fit_wine<-hclust(dist_wine,method="complete") # method here is complete linkage

plot(fit_wine) # Displaying Dendrogram
rect.hclust(fit_wine, k=7, border="red")

groups_wine<-cutree(fit_wine,5) # Cutting the dendrogram for 5 clusters

membership_wine<-as.matrix(groups_wine) # cluster numbering 

View(membership_wine)

final_wine<-cbind(membership_wine,wine_data) # binding column wise with orginal data
View(final_wine)
View(aggregate(final_wine[,-c(16:18)],by=list(membership_wine),FUN=mean)) # Inferences can be
# drawn from the aggregate of the universities data on membership_1

library(WriteXLS)
write.csv(final_wine,file="wine_clustered.csv",row.names = F,col.names = F)
getwd()


# K-Means Clustering :
library(plyr)

str(mydata)

View(mydata)

normalized_data<-scale(mydata[,15:17])

wss = (nrow(normalized_data)-1)*sum(apply(normalized_data, 2, var))     # Determine number of clusters by scree-plot 
for (i in 1:7) wss[i] = sum(kmeans(normalized_data, centers=i)$withinss)
plot(1:7, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")

fit <- eclust(normalized_data, "kmeans", k = 7, nstart = 25, graph = FALSE) # 7 cluster solution
fviz_cluster(fit, geom = "point", frame.type = "norm")


final2<- data.frame(fit$cluster,mydata) # append cluster membership
View(final2)
aggregate(mydata[,2:17], by=list(fit$cluster),FUN=mean)

table(fit$cluster)

library(WriteXLS)
write.csv(final2,file="wine_KMEANSclustered.csv",row.names = F,col.names = F)
getwd()
