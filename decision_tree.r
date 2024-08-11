data <- Cardiotocographic

str(data) # print the structure of the data

# the NSP is the class label of this data so in order to make R realize that
# we have to make the  NSP data as a factor and make a new col in the data set
data$nspf <- factor(data$NSP)

# creating the training and testing data
set.seed(1234)
pd <- sample(2, nrow(data), replace = TRUE, prob = c(0.8, 0.2))
training_data <- data[pd==1, ]
testing_data  <- data[pd==2, ]

# creating the decision tree
install.packages("party")
library(party)

# creating the decision tree model
tree <- ctree(nspf~LB + AC + FM + UC + DL + DS + DP + ASTV, data = training_data) # nspf is the label
plot(tree)

# prediction process
predict(tree, testing_data) # it will give us the probability of all the outcomes possible
