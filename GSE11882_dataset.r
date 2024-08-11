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


write.csv(normalized_dataset, "Normalized_GSE11882_expression_data.csv", row.names = FALSE)

n_data <-  normalized_dataset

# clustering k means using multiple sample space
library(cluster)
?clara
clara_result <- clara(n_data, k = 7, samples = 10, metric = c("euclidean"))
plot(clara_result, xlab = "main x", ylab = "main y")

