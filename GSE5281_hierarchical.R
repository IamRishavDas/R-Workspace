library(GEOquery)
library(factoextra)
library(ggplot2)
install.packages('igraph')

# downloading the GSE5281 dataset using the bioconductor 
gse <- getGEO("GSE5281", GSEMatrix = TRUE)

gse_5281 <- gset # already downloaded in the previous 5281 file

gse_data <- gset[[1]]
expression_data <- exprs(gse_data)
expression_data <- log2(expression_data + 1) # skipped for now

print(expression_data)
write.csv(expression_data, file = "GSE_5281.csv", row.names = FALSE)

dist_matrix <- dist(t(expression_data), method = "euclidean")
print(dist_matrix)

# Perform hierarchical clustering using Ward's method
hc <- hclust(dist_matrix, method = "ward.D2")

# Enhanced visualization of the dendrogram
fviz_dend(hc, k = 4,  # Set the number of clusters you want to cut the tree into
          cex = 0.6,  # Label size
          k_colors = c("#00AFBB", "#E7B800", "#FC4E07", "#2E9FDF"),  # Custom colors for clusters
          color_labels_by_k = TRUE,  # Color labels by clusters
          rect = TRUE,  # Add rectangle around clusters
          rect_fill = TRUE,  # Fill the rectangle with colors
          rect_border = "jco",  # Use jco color palette for rectangles
          main = "Hierarchical Clustering Dendrogram of GSE5281 Dataset")


# Enhanced visualization with customization
fviz_dend(
  hc,
  k = 4,  # Number of clusters
  cex = 0.5,  # Label size
  k_colors = c("#FF6347", "#4682B4", "#32CD32", "#FF1493"),  # Custom colors
  color_labels_by_k = TRUE,  # Color labels by clusters
  rect = TRUE,  # Add rectangle around clusters
  rect_fill = TRUE,  # Fill the rectangle with colors
  rect_border = "jco",  # Use jco color palette for rectangles
  lwd = 0.8,  # Line width for the dendrogram branches
  type = "phylogenic",  # Use a circular or phylogenic type dendrogram
  repel = TRUE,  # Repel overlapping labels
  ggtheme = theme_minimal(),  # Use a minimal theme for cleaner appearance
  main = "Enhanced Hierarchical Clustering Dendrogram",
  xlab = "Sample Clusters",
  ylab = "Height"
) +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold")
  )

