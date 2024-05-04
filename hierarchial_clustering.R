#install.packages("factoextra")

########################################### DATA CLEANING ######################################
library(factoextra)
library(readr)
library(dplyr)
data <- read.csv("C:/Users/maxch/Desktop/UH/MATH/MATH4323/data_project.csv", header=FALSE)
colnames(data) <- as.character(unlist(data[1, ]))
#remove first column & first row of data
data <- data[-1, ]
data <- data[, -1]
#remove the last row from the data
data <- data[-nrow(data), ]
data[] <- lapply(data, function(x) type.convert(as.character(x)))
data$ShareDealsPurchases <- ifelse(is.infinite(data$ShareDealsPurchases), NA, data$ShareDealsPurchases)
median_value <- median(data$ShareDealsPurchases, na.rm = TRUE)
data$ShareDealsPurchases <- ifelse(is.na(data$ShareDealsPurchases), median_value, data$ShareDealsPurchases)
sum(is.na(data$ShareDealsPurchases))
data$`...30` <- NULL
data$Family_Size <- NULL
#binary_columns <- c("AcceptedCmp1", "AcceptedCmp2", "AcceptedCmp3", "AcceptedCmp4", "AcceptedCmp5", "Complain", "Response")
# convert each column to a factor
#for(col in binary_columns) {
#  data[[col]] <- factor(data[[col]], levels = c(0, 1), labels = c("False", "True"))
#}
summary(data)
#View(data)
#write.csv(data, "C:/Users/maxch/Desktop/clean_data.csv")
########################################### EDA ######################################
######################################

library(ggplot2)
library(GGally)
library(corrplot)
library(cluster)
# Histograms for each spending category
if (!require("gridExtra")) install.packages("gridExtra")
library(ggplot2)
library(gridExtra)

# Create each plot
p1 <- ggplot(data, aes(x=MntWines)) + 
  geom_histogram(bins=30, fill="blue", alpha=0.7) + 
  ggtitle("Distribution of Wine Spending") +
  labs(x = "Dollars Spent", y = "Number of People")

p2 <- ggplot(data, aes(x=MntMeatProducts)) + 
  geom_histogram(bins=30, fill="red", alpha=0.7) + 
  ggtitle("Distribution of Meat Spending") +
  labs(x = "Dollars Spent", y = "Number of People")

p3 <- ggplot(data, aes(x=MntFruits)) + 
  geom_histogram(bins=30, fill="green", alpha=0.7) + 
  ggtitle("Distribution of Fruit Spending") +
  labs(x = "Dollars Spent", y = "Number of People")

p4 <- ggplot(data, aes(x=MntSweetProducts)) + 
  geom_histogram(bins=30, fill="pink", alpha=0.7) + 
  ggtitle("Distribution of Sweet Products Spending") +
  labs(x = "Dollars Spent", y = "Number of People")

p5 <- ggplot(data, aes(x=MntFishProducts)) + 
  geom_histogram(bins=30, fill="cyan", alpha=0.7) + 
  ggtitle("Distribution of Fish Products Spending") +
  labs(x = "Dollars Spent", y = "Number of People")

p6 <- ggplot(data, aes(x=MntGoldProds)) + 
  geom_histogram(bins=30, fill="gold", alpha=0.7) + 
  ggtitle("Distribution of Gold Products Spending") +
  labs(x = "Dollars Spent", y = "Number of People")

# Arrange all plots in a grid
grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 3)


#PCA
# Remove categorical columns that won't be used for PCA
data$Education <- NULL
data$Marital_Status <- NULL

#Scale data
numeric_columns <- sapply(data, is.numeric)
data_scaled <- scale(data[, numeric_columns])
#perform PCA
pca_results <- prcomp(data_scaled, center = TRUE, scale. = TRUE)

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

#scores

################### Perform hierarchical clustering ###################
hc <- hclust(dist(scores), method = "ward.D2")

# Plot the dendrogram
plot(hc, main = "Hierarchical Clustering Dendrogram", sub = "", xlab = "")

# To cut the tree at a specific number of clusters, for example 5:
clusters <- cutree(hc, k = 5)

# Optionally, you can plot the clusters on the first two principal components to visualize them
plot(scores[,1:2], col = clusters, pch = 16, main = "Clusters on First Two Principal Components",
     xlab = "PC1", ylab = "PC2")
points(scores[,1:2], col = clusters, pch = 19, cex = 0.5)
legend("topright", legend = unique(clusters), col = unique(clusters), pch = 16)


# Install and load the package
if (!require(scatterplot3d)) install.packages("scatterplot3d")
library(scatterplot3d)

# Create a 3D scatter plot
scatterplot3d(scores[,1], scores[,2], scores[,3], color=clusters, pch=19,
              main="3D Scatter Plot of Clusters",
              xlab="PC1", ylab="PC2", zlab="PC3")

# Adding a legend to the plot
legend("topright", legend=unique(clusters), col=unique(clusters), pch=19)

###
# Example for one campaign
#ggplot(cluster_profiles, aes(x = factor(Cluster), y = Avg_AcceptedCmp1, fill = factor(Cluster))) +
#  geom_col() +
#  theme_minimal() +
#  labs(title = "Average Acceptance Rate for Campaign 1", x = "Cluster", y = "Average Acceptance Rate")



####
if (!require(factoextra)) install.packages("factoextra")
if (!require(cluster)) install.packages("cluster")
library(factoextra)
library(cluster)

# Calculate the silhouette information
silhouette_info <- silhouette(clusters, dist_matrix)

# Use factoextra to visualize the silhouette plot
fviz_silhouette(silhouette_info) + theme_minimal() + ggtitle("Silhouette Plot")

# Calculate the average silhouette score
avg_sil_width <- mean(silhouette_info[, "sil_width"])
print(paste("Average Silhouette Width:", avg_sil_width))

plot(hc, hang = -1, labels = FALSE, main = "Dendrogram of Clustering", sub = "", xlab = "", ylab = "Height")
rect.hclust(hc, k = 5, border = 2:6)

#### map


###
total_responses <- sum(data$AcceptedCmp1, na.rm = TRUE) + sum(data$AcceptedCmp2, na.rm = TRUE) + 
  sum(data$AcceptedCmp3, na.rm = TRUE) + sum(data$AcceptedCmp4, na.rm = TRUE) +
  sum(data$AcceptedCmp5, na.rm = TRUE)

cluster_profiles <- cluster_profiles %>%
  mutate(
    Pct_TotalAcceptedCmp1 = (Avg_AcceptedCmp1 * Count / total_responses) * 100,
    Pct_TotalAcceptedCmp2 = (Avg_AcceptedCmp2 * Count / total_responses) * 100,
    Pct_TotalAcceptedCmp3 = (Avg_AcceptedCmp3 * Count / total_responses) * 100,
    Pct_TotalAcceptedCmp4 = (Avg_AcceptedCmp4 * Count / total_responses) * 100,
    Pct_TotalAcceptedCmp5 = (Avg_AcceptedCmp5 * Count / total_responses) * 100
  )

library(ggplot2)

# Creating a melted data frame for easier plotting with ggplot2
melted_data <- pivot_longer(cluster_profiles, cols = starts_with("Avg_Accepted"), names_to = "Campaign", values_to = "Average_Acceptance")

ggplot(melted_data, aes(x = factor(Cluster), y = Average_Acceptance, fill = Campaign)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_minimal() +
  labs(title = "Campaign Acceptance Rates by Cluster", x = "Cluster", y = "Average Acceptance Rate")

