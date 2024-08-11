data <- EntorhinalCortex_male_86yrs_indiv73
head(data)

# Load necessary library
library(ggplot2)


# Select only numerical columns
data_numeric <- data[-1, c("VALUE", "PLIER")]

# Apply k-means clustering
set.seed(123) # for reproducibility
k <- 2
kmeans_result <- kmeans(data_numeric, centers = k)

# Add the cluster information to the original data
data$Cluster <- as.factor(kmeans_result$cluster)

# Visualize the clusters
ggplot(data, aes(x = VALUE, y = PLIER, color = "#cc0000")) +
  geom_point(size = 3) +
  labs(title = "K-means Clustering",
       x = "VALUE",
       y = "PLIER") +
  theme_minimal()

# (Optional) Analyze cluster centers
print(kmeans_result$centers)
