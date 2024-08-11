library(datasets)
library(factoextra)

iris
table(iris$Species)
data <- iris[1:4]
data

iris_data_scale <- scale(data)
iris_data_scale

iris_data <- dist(iris_data_scale)
iris_data

# how many clusters using elbow method
# withing sum squeares
fviz_nbclust(iris_data_scale, kmeans, method = "wss") +
  labs(subtitle = "Elbow method")

km.out <- kmeans(iris_data_scale, centers = 3, nstart = 100)
print(km.out)


# visualize the clustering algorithm result
km.clusters <- km.out$cluster
rownames(iris_data_scale) <- paste(iris$Species, 1: dim(iris)[1], sep = "_")

# visulalizing the clusters
fviz_cluster(list(data = iris_data_scale, cluster = km.clusters))
table(km.clusters, iris$Species)
