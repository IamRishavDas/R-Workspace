library(datasets)
?iris
?plot

#printing the data set
print(iris) #print the entire data set
head(iris) #print only the first 6 line of the data set


#basic plotting
plot(iris)  #print the whole data frame
plot(iris$Species) #categorical data
plot(iris$Sepal.Length, col = "#cc0000", pch = 19) #quantitative data
plot(iris$Species, iris$Petal.Width)  #categorical vs quantitative data


#plot with options
plot(iris$Petal.Length, iris$Petal.Width, 
     pch = 19, #pch = point character (there are many more)
     main = "Petal length vs Petal width",
     xlab = "petal length",
     ylab = "petal width")


#plot with formulas
plot(cos, 0, 2*pi) #cos function with range 0 to 2pi
plot(exp, 1, 5, col = "#cc0000")
plot(dnorm, +3, -3) #dnorm = density normalization


#plot formula with options
plot(dnorm, 3, -3,
     lwd = 8 #line width
     )