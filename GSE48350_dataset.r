library(GEOquery)

#loading gse data

geo_data <- getGEO("GSE48350", GSEMatrix = TRUE)
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
write.csv(expression_data, "GSE48350_expression_data.csv", row.names = TRUE)

data <- GSE48350_expression_data 
head(data)


#normalization function
min_max_normalize <- function(x){
  return ((x-min(x))/(max(x) - min(x)))
}

normalized_dataset <- as.data.frame(lapply(data, function(x) {
  if(is.numeric(x)) {
    return(min_max_normalize(x))
  } else {
    return(x)
  }
}))

head(normalized_dataset)

write.csv(normalized_dataset, "normalized_dataset.csv", row.names = FALSE)

n_data <- normalized_dataset
print(n_data)


library(factoextra)
library(cluster)



# how many clusters using elbow method
# withing sum squeares

# first 15000 dataset samples
fviz_nbclust(n_data[2: 15000, 2:50], kmeans, method = "wss") +
  labs(subtitle = "Elbow method")

# second 15000 dataset samples
fviz_nbclust(n_data[15000: 30000, 2:50], kmeans, method = "wss") +
  labs(subtitle = "Elbow method")


# clustering k means using multiple sample space
clara_result <- clara(n_data, k = 7, samples = 10)
plot(clara_result, xlab = "main x", ylab = "main y")





