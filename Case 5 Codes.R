#1. Clustering analysis 
pacman::p_load(pacman, rio, tidyverse)
data(iris) #show iris dataset 
iris_1 = iris[sample(nrow(iris), nrow(iris) * 0.9), ] 
library(fpc)

# K-Means Cluster Analysis
fit <- kmeans(iris_1[,1:4], 3) #3 cluster solution
fit1 <- kmeans(iris_1[,1:4], 2) #2 cluster solution
fit2 <- kmeans(iris_1[,1:4], 1) #1 cluster solution
# Display number of clusters in each cluster
table(fit$cluster)
table(fit1$cluster)
table(fit2$cluster)
# Plot cluster in kmeans
plotcluster(iris_1[, 1:4], fit$cluster)
plotcluster(iris_1[, 1:4], fit1$cluster)
plotcluster(iris_1[, 1:4], fit2$cluster)
# See exactly which item are in 1st group and then try with different clusters 
iris$Species[fit$cluster == 1]
iris$Species[fit$cluster == 2]
iris$Species[fit$cluster == 3]

iris$Species[fit1$cluster == 1]
iris$Species[fit1$cluster == 2]

iris$Species[fit2$cluster == 1]

# get cluster means
aggregate(iris_1[,1:4], by = list(fit$cluster), FUN = mean) #it shows cluster of each variable
aggregate(iris_1[,1:4], by = list(fit1$cluster), FUN = mean)
aggregate(iris_1[,1:4], by = list(fit2$cluster), FUN = mean)       

#Hierarchical clustering 
# Wards Method or Hierarchical clustering Calculate the distance matrix
Iris.dist = dist(iris_1[,1:4])
# Obtain clusters using the Wards method
Iris.hclust = hclust(Iris.dist, method = "ward.D2")
plot(Iris.hclust)
# Cut dendrogram at the 3 clusters level and obtain cluster membership
groupIris.3 = cutree(Iris.hclust, k = 3)
table(groupIris.3) #number of items for each cluster 
# See exactly which item are in third group
iris$Species[groupIris.3 == 3]
iris$Species[groupIris.3 == 2]
iris$Species[groupIris.3 == 1]

# Cut dendrogram at the 2 clusters level and obtain cluster membership
groupIris.2 = cutree(Iris.hclust, k = 2)
table(groupIris.2) #number of items for each cluster 
# See exactly which item are in second group
iris$Species[groupIris.2 == 2]
iris$Species[groupIris.2 == 1]

# Cut dendrogram at the 1 clusters level and obtain cluster membership
groupIris.1 = cutree(Iris.hclust, k = 1)
table(groupIris.1) #number of items for each cluster 
# See exactly which item are in first group
iris$Species[groupIris.1 == 1]

# get cluster means for raw data
aggregate(iris_1[,1:4], by = list(groupIris.3), FUN = mean)
aggregate(iris_1[,1:4], by = list(groupIris.2), FUN = mean)
aggregate(iris_1[,1:4], by = list(groupIris.1), FUN = mean)
# Centroid Plot against 1st 2 discriminant functions Load the fpc library
# needed for plotcluster function
plotcluster(iris_1[,1:4], groupIris.3)
plotcluster(iris_1[,1:4], groupIris.2)
plotcluster(iris_1[,1:4], groupIris.1)

#2. Association Rules
#1)	Preliminaries
#a)	Load the arules library (installing it first if necessary).
library ("arules")
#b)	Load the Groceries data set that comes with it by running the command data(Groceries).
#The data set contains a month’s worth of transactions from a small grocery store.
data("Groceries")
Groceries
summary(Groceries)
#2)	Data exploration
#a)	What are the dimensions of the Groceries data set?
dim(Groceries)
#interpretation: the data has 9835 rows and 169 columns 
#b)	Print out the first 10 transactions.
inspect(Groceries[1:10])
#c)	What was the most frequently purchased item and how many times was it purchased?
summary(Groceries[1:10])
summary(Groceries)
#interpretation: the first 10 transactions tell us that whole milk was frequently purchased and 
#it was purchased 4 times. 
#d)	How many items did the average transaction involve?
#3 transactions involved 1 item and 3 transactions involved 4 items, the mean number of items are 3 items
#e)	How many items did the largest transaction involve?
#the largest transaction involved 5 items 
#f)	Create an item frequency plot of all of the items.
itemFrequencyPlot(Groceries)
#g)	Create an item frequency plot showing only those items with a support of at least 10%.
itemFrequencyPlot(Groceries, support = 0.1, cex.names = 0.8)
#3)	Associate rules
#a)	Using the apriori function, generate a set of association rules for the Groceries data.  Adjust the support 
#and confidence parameters so that you generate at least 20 rules. (Many different settings of the parameters will work.)
rules <- apriori(Groceries, parameter = list(support = 0.005, confidence = 0.6))
summary(rules)
inspect(rules)
#b)	How many rules did you generate?
#I generated 22 rules 
#c)	What is the distribution of the number of items in each rule?
#13 rules with 3 items and 9 rules with 4 items 
#d)	What is the average lift of the rules you generated?
#My average lift is 2.599
#e)	Print out the five rules with the highest lift.
inspect(head(sort(rules, by = "lift"), n = 5))
#f)	Comment on the rules that you generated.
