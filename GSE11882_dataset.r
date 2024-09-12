library(GEOquery)

geo_data <- getGEO("GSE11882", GSEMatrix = TRUE)
head(geo_data)
?geo_data

geo_data <- geo_data[[1]]


head(geo_data)
str(geo_data)

expression_data <- exprs(geo_data)
sample_data <- pData(geo_data)
feature_data <- fData(geo_data)

head(expression_data)
print(expression_data[1:10, 1])
head(sample_data)
head(feature_data)

plot(expression_data)

#saving the GSE data in csv format
write.csv(expression_data, "GSE11882_expression_data.csv", row.names = TRUE)

data <- GSE11882_expression_data 
head(data[1:5, 2:4])


#normalization function
min_max_normalize <- function(x){
  return ((x-min(x))/(max(x) - min(x)))
}


normalized_dataset <- as.data.frame(lapply(data[, 2:174], function(x) {
  if(is.numeric(x)) {
    print("normalized")
    return(min_max_normalize(x))
  } else {
    return(x)
  }
}))


write.csv(normalized_dataset, "Normalized_GSE11882_expression_data.csv", row.names = FALSE)

print(n_data)
n_data <- Normalized_GSE11882_expression_data
n_data <- GSE11882_expression_data # for testing on raw data

first_col <- expression_data[, 1]
df2 <- cbind(first_col, n_data)
colnames(df2)[1] <- "id"
head(df2)
df2 <- df2[, -1]
head(df2)
write.csv(df2, "Normalized_GSE11882_expression_data_with_ID.csv", row.names = FALSE)

n_data <- Normalized_GSE11882_expression_data_with_ID

first_col <- n_data[, 1]
main_data <- data.frame(lapply(n_data[, -1], as.numeric))
main_data <- na.omit(main_data)

install.packages("clValid")
library(clValid)

# k means for k - 2
km.out <- kmeans(main_data, centers = 2)
print(km.out)
cluster_centers <- km.out$centers
cluster_centers_df <- data.frame(cluster_centers)
cluster_centers_with_strings <- cbind(first_col[1:nrow(cluster_centers_df)], cluster_centers_df)
print(cluster_centers_with_strings)
write.csv(cluster_centers_with_strings, "cluster_centers11882_k2.csv", row.names = FALSE)

set.seed(1234)


# k means for k - 3
km.out <- kmeans(main_data, centers = 3)
print(km.out)
cluster_centers <- km.out$centers
cluster_centers_df <- data.frame(cluster_centers)
cluster_centers_with_strings <- cbind(first_col[1:nrow(cluster_centers_df)], cluster_centers_df)
print(cluster_centers_with_strings)
write.csv(cluster_centers_with_strings, "cluster_centers11882_k3.csv", row.names = FALSE)

# problem with findint the dunn index
dunn_index <- dunn(clusters = cluster_centers, Data = main_data)
print(dunn_index)

# problem with sillhoutte index finding
print(km.out$cluster)
dist_matrix <- dist(main_data)
sil <- silhouette(km.out$cluster, dist_matrix)


# k means for k - 4
km.out <- kmeans(main_data, centers = 4)
print(km.out)
cluster_centers <- km.out$centers
cluster_centers_df <- data.frame(cluster_centers)
cluster_centers_with_strings <- cbind(first_col[1:nrow(cluster_centers_df)], cluster_centers_df)
print(cluster_centers_with_strings)
write.csv(cluster_centers_with_strings, "cluster_centers11882_k4.csv", row.names = FALSE)

# k means for k - 5
km.out <- kmeans(main_data, centers = 5)
print(km.out)
cluster_centers <- km.out$centers
cluster_centers_df <- data.frame(cluster_centers)
cluster_centers_with_strings <- cbind(first_col[1:nrow(cluster_centers_df)], cluster_centers_df)
print(cluster_centers_with_strings)
write.csv(cluster_centers_with_strings, "cluster_centers11882_k5.csv", row.names = FALSE)



