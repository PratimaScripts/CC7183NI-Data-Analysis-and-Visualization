
remove(list = ls())


library(dplyr)
library(ggplot2)
# install.packages("DataExplorer")
library(DataExplorer)

library(tidyr)

mtcars_df <- mtcars

head(mtcars_df)

View(mtcars_df)

class(mtcars_df)
class(mtcars_df$mpg)

sapply(mtcars_df,class)
sapply(mtcars, sd)


#Now we are going to do clustering. 

# We use the function known as kmeans where we give data, center and iter.max 

cluster_model <- kmeans(mtcars_df, 3, iter.max = 10)
help("kmeans")

summary(cluster_model)


cluster_model$cluster
cluster_model$centers
cluster_model$totss



### If you want to use whole variables for cluster plotting, PCA is used. 
### PCA means Principal Component Analysis. 
### You can look for its implementation in R. 

mtcars_df$cluster_number <- cluster_model$cluster

View(mtcars_df)


### ggplot ### data, variable to plot, and what to plot ##
### data = data, variables = aes, plot comes under geom (geometry)

ggplot(mtcars_df, aes(x=mpg, y=wt,col=as.factor(cluster_number)))+geom_point()



### Now, you can make a new cluster model with 5 centers. And then plot the clusters. 

### Now onto Hierarchical Clustering 

distance_cluster <- dist(mtcars_df)
cluster_hier_model <- hclust(distance_cluster)
plot(cluster_hier_model)


### Now doing it with only two variables mpg and wt

distance_cluster_only_two <- dist(mtcars_df[,c('mpg','wt')])
cluster_hier_model_only_two <- hclust(distance_cluster_only_two)

plot(cluster_hier_model_only_two)


cluster_model_two_variables <- kmeans(mtcars_df[,c('mpg','wt')], 4, iter.max = 10)
mtcars_df$cluster_number_two_variables <- cluster_model_two_variables$cluster
ggplot(mtcars_df, aes(x=mpg, y=wt,col=as.factor(cluster_number_two_variables)))+geom_point()


# 1, 2, 3 ... 10
# Scale this into 0 and 1 so that 1 becomes 0 and 10 becomes 1. 

### Does scale influence the result of clustering? 

### This is for you to think! 

### Now we will learn how to scale / normalize / standardize data.

scale(mtcars_df$mpg)#Just scaling a variable mpg. 
scale(mtcars_df$wt)
# Try to avoid for loop as much as possible in data science. Use apply function.
mtcars_scale <- sapply(mtcars_df, scale)

### You have the scaling part ### 

create_report(mtcars)
