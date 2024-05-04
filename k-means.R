# K-Means Clustering :)
library(factoextra)

# getting rid of text columns
df <- d
df <- df[3:33]

#scaling data
df_scale <- scale(df)
df_scale

# distance
df_dist <- dist(df_scale)
df_dist

#perform PCA
pca_results <- prcomp(df_scale, center = TRUE, scale. = TRUE)

summary(pca_results)

#biplot
biplot(pca_results)

#scree plot
plot(pca_results, type = "lines")

loadings <- pca_results$rotation
print(loadings[, 1:3])

# Visualize the loadings for the first three principal components
library(tidyr)
library(ggplot2)

# Visualize the loadings for the first three principal components
loadings <- pca_results$rotation
df_loadings <- as.data.frame(loadings[, 1:3])
df_loadings$Variable <- row.names(df_loadings)
df_long <- pivot_longer(df_loadings, cols = -Variable, names_to = "PC", values_to = "Loading")

ggplot(df_long, aes(x = Variable, y = Loading, fill = PC)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Loadings Plot", x = "Variables", y = "Loadings")


# Let's say you decide to use the first three principal components
scores <- pca_results$x[, 1:3]

# calculate number of clusters
fviz_nbclust(scores, kmeans, method = "wss") +
  labs(subtitle = "elbow method")
# best number of clusters is 5?

# K-means clustering
km.out <- kmeans(scores, centers = 4, nstart = 100)
print(km.out)

# visualize clusters
km.clusters <- km.out$cluster
rownames(scores) <- paste(1:dim(df)[1])
fviz_cluster(list(data = scores, cluster = km.clusters))
#install.packages("rgl")
#install.packages("scatterplot3d")
#library(rgl)
#library(scatterplot3d)
km.centers <- km.out$centers
# Create a 3D scatter plot
scatterplot3d(scores[,1], scores[,2], scores[,3], color=km.clusters, pch=19,
              main="3D Scatter Plot of Clusters",
              xlab="PC1", ylab="PC2", zlab="PC3")

# Adding a legend to the plot
legend("topright", legend=unique(km.clusters), col=unique(km.clusters), pch=19)