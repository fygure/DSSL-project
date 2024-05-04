# K-Means Clustering :)

install.packages("tidyverse")
library(tidyverse)
install.packages("caret")
library(caret)
install.packages("FactoMineR")
library(FactoMineR)
install.packages("factoextra")
library(factoextra)
install.packages("purrr")
library(purrr)
install.packages("cluster")
library(cluster)


df <- d
df <- df[3:33]

#preprocessing the data
df_pre <- preProcess(df[,c(1:13, 24, 26)], method = c("center", "scale"))

#normalizing 
customers_copy <- predict(df_pre, df[, c(1:13, 24, 26)])
summary(customers_copy)

#Running a PCA.
customers_copy_pca <- PCA(customers_copy, graph = FALSE)

#Exploring PCA()

# Getting the summary of the pca
summary(customers_copy_pca)

#Getting the variance of the first 7 new dimensions
customers_copy_pca$eig[,2][1:7]

#Getting the cummulative variance
customers_copy_pca$eig[,3][1:7]

#Getting the most correlated variables
dimdesc(customers_copy_pca, axes = 1:2)


#Tracing variable contributions in customers_pca
customers_copy_pca$var$contrib

#Creating a factor map for the variable contributions
fviz_pca_var(customers_copy_pca, col.var = "contrib", gradient.cols = c("#002bbb", "#bb2e00"), repel = TRUE)

#Creating a factor map for the top 5 variables with the highest contributions.
fviz_pca_var(customers_copy_pca, select.var = list(contrib = 5), repel = TRUE)

#Bar-plot of contributions of variables
fviz_contrib(customers_copy_pca, choice = "var", axes = 1, top = 5)

#Bi plot of all data entries
fviz_pca_biplot(customers_copy_pca)

#K-means clustering
#Elbow Method
tot_withinss <- map_dbl(1:10, function(k){
  model <- kmeans(x = customers_copy, centers = k)
  model$tot.withinss
})

elbow_df <- data.frame(
  k = 1:10,
  tot_withinss = tot_withinss)
head(elbow_df)
#plotting the elbow plot
ggplot(elbow_df, aes(k, tot_withinss)) + geom_line() + scale_x_continuous(breaks = 1:10) +
  ggtitle("Elbow Method")
#K = 2 or 3 are good ideas

#Silhouette Method
sil_width <- map_dbl(2:10, function(k){
  model <- pam(customers_copy, k = k)
  model$silinfo$avg.width
})
sil_df <- data.frame(
  k = 2:10,
  sil_width = sil_width
)
head(sil_df)
ggplot(sil_df, aes(k, sil_width)) + geom_line() + scale_x_continuous(breaks = 2:10) + labs(y = "Avg sil width") +
  ggtitle("Silhouette Method")
#K = 2 has the highest width, meaning its the optimal cluster amount

set.seed(42)

#Building a k-means model with a k of 2
customers_md <- kmeans(customers_copy, center = 2)
print(customers_md)

#Extracting the vector of cluster assignment from the model
clust_customers <- customers_md$cluster

#Building the segment_customers dataframe
segment_customers <- mutate(customers_copy, cluster = clust_customers)

#Calculating the mean for each category
count(segment_customers, cluster)

customers <- df
#Adding the cluster variable to the original dataframe
customers <- customers %>% mutate(cluster = segment_customers$cluster)
head(customers, n = 4)
#confirming
count(customers, cluster)

#visualizing wines
customers %>% ggplot(aes(MntWines)) + geom_histogram(color = "black", fill = "lightblue") + facet_wrap(vars(cluster)) 
#visualizing Income variable
customers %>% ggplot(aes(Income)) + geom_histogram(color = "black", fill = "lightgreen") + facet_wrap(vars(cluster)) +  geom_vline(aes(xintercept=mean(Income)),color="blue", linetype="dashed", size = 1)
#visualizing Total_spent
customers %>% ggplot(aes(MntTotal)) + geom_histogram(color = "black", fill = "purple") + facet_wrap(vars(cluster))
#visualizing NumCatalogPurchases
customers %>% ggplot(aes(NumCatalogPurchases)) + geom_histogram(color = "black", fill = "orange") + facet_wrap(vars(cluster)) 
#visualizing meat variable
customers %>% ggplot(aes(MntMeatProducts)) + geom_histogram(color = "black", fill = "brown") + facet_wrap(vars(cluster))
#visualizing fish variable
customers %>% ggplot(aes(MntFishProducts)) + geom_histogram(color = "black", fill = "blue") + facet_wrap(vars(cluster))
#visualizing sweet variable
customers %>% ggplot(aes(MntSweetProducts)) + geom_histogram(color = "black", fill = "pink") + facet_wrap(vars(cluster))
#visualizing fruits variable
customers %>% ggplot(aes(MntFruits)) + geom_histogram(color = "black", fill = "red") + facet_wrap(vars(cluster))
#visualizing gold variable
customers %>% ggplot(aes(MntGoldProds)) + geom_histogram(color = "black", fill = "yellow") + facet_wrap(vars(cluster))
