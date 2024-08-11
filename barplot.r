library(datasets)

?mtcars
head(mtcars)

barplot(mtcars$cyl) #does not work

#need a table with frequency of each category
cylinder <- table(mtcars$cyl) #create table
barplot(cylinder)             #bar chart
plot(cylinder)                #default x-y plot (lines)

