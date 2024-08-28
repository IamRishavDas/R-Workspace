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

n_data <- Normalized_GSE11882_expression_data
n_data <- GSE11882_expression_data # for testing on raw data

# clustering k means using multiple sample space
library(cluster)
library(factoextra)
?clara
clara_result <- clara(n_data, k = 6, samples = 50, metric = c("euclidean"))
plot(clara_result, xlab = "main x", ylab = "main y")


km.out <- kmeans(n_data[ ,-1], centers = 3)
print(km.out)


km.clusters <- km.out$cluster
print(table(km.out$centers))
write.csv(km.out$centers,  "cluster centers", row.names = FALSE)
print(table(km.clusters))
print(km.clusters)
rownames(n_data) <- paste(data$X, 1: dim(n_data)[1], sep = "_")

fviz_cluster(list(data = n_data[,-1], cluster = km.clusters))
table(km.clusters, data$X)


# chat gpt

library(factoextra)
fviz_cluster(list(data = n_data, cluster = km.clusters), 
             geom = "point", # change geometry
             ellipse.type = "norm", # add normal confidence ellipses
             palette = "jco", # use a different color palette
             ggtheme = theme_minimal()) # use a minimal theme

