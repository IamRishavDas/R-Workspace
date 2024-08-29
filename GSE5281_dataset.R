# Install necessary Bioconductor packages if not already installed
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("GEOquery")
BiocManager::install("limma")

# Load required libraries
library(GEOquery)  # For downloading GEO data
library(limma)     # For preprocessing and normalization
library(ggplot2)   # For visualization
library(cluster)   # For clustering


geo_accession <- "GSE5281"
gset <- getGEO(geo_accession, GSEMatrix = TRUE, AnnotGPL = TRUE)

# Extract the expression set
data <- exprs(gset[[1]])

# log transformation
if (max(data, na.rm = TRUE) > 100) { 
  data <- log2(data + 1)
}

# additional normalization using limma package
data <- normalizeBetweenArrays(data)

  
k <- 4 # no of clusters

# Perform k-means clustering
set.seed(123)  # Set seed for reproducibility
kmeans_result <- kmeans(t(data), centers = k, nstart = 25)


# Principal Component Analysis (PCA) for visualization
pca <- prcomp(t(data), scale. = TRUE)
pca_data <- as.data.frame(pca$x)
pca_data$Cluster <- as.factor(kmeans_result$cluster)

# Plot PCA results colored by clusters
ggplot(pca_data, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(title = paste("PCA of Alzheimer's Data - K-means Clustering with", k, "Clusters"),
       x = "Principal Component 1", y = "Principal Component 2")

# Step 5: Evaluate Clustering (Optional)
# Silhouette analysis to evaluate clustering quality
sil <- silhouette(kmeans_result$cluster, dist(t(data)))
plot(sil, border = NA, col = rainbow(3), main = "Silhouette Plot for K-means Clustering")

# Save clustering results for further analysis
clustering_results <- data.frame(Sample = colnames(data), Cluster = kmeans_result$cluster)
write.csv(clustering_results, file = "clustering_results.csv", row.names = FALSE)

# End of script




# fviz cluster
fviz_cluster(kmeans_result, data = t(data), 
             geom = "point", 
             stand = FALSE,  # Do not standardize data, already normalized
             ellipse = TRUE,  # Add confidence ellipses
             show.clust.cent = TRUE,  # Show cluster centers
             main = paste("K-means Clustering of Alzheimer's Data with", k, "Clusters"))


# Enhanced Visualization with fviz_cluster
fviz_cluster(kmeans_result, data = t(data), 
             geom = "point",  # Use point geometry for data points
             stand = FALSE,  # Do not standardize data if already normalized
             ellipse.type = "convex",  # Use convex hulls instead of ellipses
             palette = "jco",  # Use the "jco" color palette for vibrant colors
             ggtheme = theme_minimal(),  # Apply a minimal theme for a clean look
             main = paste("K-means Clustering of Alzheimer's Data with", k, "Clusters"),
             pointsize = 2,  # Increase point size for better visibility
             labelsize = 5,  # Increase label size for better readability
             repel = TRUE,  # Use repel to avoid overlapping labels
             show.clust.cent = TRUE,  # Show cluster centers
             ellipse.level = 0.95)  # Adjust ellipse confidence level for clarity


# Step 5: Evaluate Clustering (Optional)
# Silhouette analysis to evaluate clustering quality
sil <- silhouette(kmeans_result$cluster, dist(t(data)))
plot(sil, border = NA, col = rainbow(3), main = "Silhouette Plot for K-means Clustering")

# Save clustering results for further analysis
clustering_results <- data.frame(Sample = colnames(data), Cluster = kmeans_result$cluster)
write.csv(clustering_results, file = "clustering_results.csv", row.names = FALSE)



