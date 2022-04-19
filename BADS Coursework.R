#Inputting the data set.
data <- read.csv("C:/Users/Priyanshu/Downloads/BrewdogNew.csv", header=TRUE, stringsAsFactors = T)

#Checking the number of Complete cases.
sum(complete.cases(data))

#Number of incomplete cases.
sum(!complete.cases(data))

#Importing a required package to check the number of missing values in the data set.
install.packages("VIM",dependencies = T)
library("VIM")
aggr(data, numbers=TRUE, prop=FALSE)

#8 missing values for ABV and 5 missing values for EBC.

#Checking the summary of the data.
summary(data)
unique(data$Yeast)

#Some Visualizations.
hist(data$ABV)
hist(data$EBC)

install.packages("corrgram")
library("corrgram")
corrgram(data)

#Creating an extra column for the missing values.
missdata <- data
missdata$missing <- as.numeric(!complete.cases(data))
corrgram(missdata)

missdata$missing

#Applying Imputation.
install.packages("mice")
library("mice")

imi <- mice( subset(data, select = c('Name', 'ABV', 'IBU','OG', 'EBC', 'PH','AttenuationLevel', 'FermentationTempCelsius', 'Yeast')), m = 5, maxit = 10)

mi <- complete(imi)

#Checking the changes made to the data due to the imputation process.
summary(data$ABV)

summary(mi$ABV)
summary(data$EBC)

summary(mi$EBC)

#Visualizing the difference in the ABV column in the data set using histogram. 
hist(mi$ABV)
hist(data$ABV, breaks = 6)

summary(mi)
summary(data)

#Visualizing the difference in the ABV column in the data set using boxplot. 
boxplot(data$ABV)
boxplot(mi$ABV)

#Importing the important libraries for clustering.
install.packages("fastcluster")
library("fastcluster")
install.packages("NbClust")
library("NbClust")
install.packages("cluster")
library("cluster")

#hierarchical clustering.
eurc <- hclust(dist(mi[2:8]), "ward.D2")
eurc

plot(eurc, labels=mi$Name)
rect.hclust(eurc, 3)

#K-means Clustering.
res <- NbClust(mi[2:8], min.nc=2, max.nc=15, method="ward.D2")

res$Best.nc
res$Best.partition

# k-means clustering on the data
km <- kmeans(mi[2:8], 3)
# Call back clustering information
km

# See how data has been clustered
eddf <- data.frame(mi, km$cluster)
# Sort data based on cluster number
eddf[order(eddf[3]),]

# Plot ABV vs EBC.
plot(mi[c("ABV","EBC")], col=km$cluster)
# Add labels
text(mi$ABV,mi$EBC, mi$Name, cex=0.6, pos=4)

#Hierarchical clustering using the Daisy function and Agglomeration technique.
dm <- daisy(mi[1:9])
clust <- agnes(dm, diss = TRUE, method="ward")
plot(clust, labels=mi$Name, which.plots= 2, cex = 1)
abline(h = 0.42, col = 'darkgreen')
bb = hclust(dm)
plot(bb)
dm
cluster <- data.frame(mi, cutree(clust, k=3))
cluster[order(cluster[2]),]
table(cutree(clust, k=3), mi$Yeast)
table(cutree(clust, k=3), mi$ABV)

library(dendextend)
hc_single <- clust
hc_tt <- eurc
hc_single <- as.dendrogram(hc_single)
hc_tt <- as.dendrogram(hc_tt)
tanglegram (hc_single, hc_tt)

dm <- daisy(mi[1:9])
clust <- agnes(dm, diss = TRUE, method="ward")
plot(clust, labels=mi$Name, which.plots= 2, cex = 1)