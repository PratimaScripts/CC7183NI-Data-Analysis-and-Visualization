setwd("~/Desktop/Islington Lab")
library(ggplot2)
library(dplyr)
library(igraph) # install.packages("igraph")

got_df <- read.csv("GOT Network.csv",header = TRUE,sep = ',',stringsAsFactors = FALSE)
dim(got_df)
View(got_df)

head(got_df)

head(arrange(got_df, desc(Weight)),10)

#To create a graph out of a data frame. 
graph_relationship <- graph.data.frame(got_df, directed = FALSE) #Using graph.data.frame function 
class(graph_relationship) # As we see, it gets stored as a graph object. 

help("graph.data.frame")

#To plot a graph 
plot(graph_relationship)

plot(graph_relationship,layout=layout.fruchterman.reingold(graph_relationship))

plot(graph_relationship,layout=layout.fruchterman.reingold(graph_relationship,
                                                           weights=E(graph_relationship)$Weight^5))

#To see edges and vertices in a graph. 
V(graph_relationship)
E(graph_relationship)


#Now we will calculate all the centrality measures that we learnt in our lecture. 


#To calculate the degree centrality of a graph 
whole_degree <- degree(graph_relationship)
class(whole_degree)
whole_degree

degree_centrality_df <- data.frame(name=names(whole_degree),Value=whole_degree)
degree_centrality_df <- arrange(degree_centrality_df, desc(Value))
head(degree_centrality_df)
tail(degree_centrality_df)

whole_closeness <- closeness(graph_relationship)
closeness_centrality_df <- data.frame(name=names(whole_closeness),Value=whole_closeness)
closeness_centrality_df <- arrange(closeness_centrality_df, desc(Value))
head(closeness_centrality_df)

whole_betweenness <- betweenness(graph_relationship)
betweenness_centrality_df <- data.frame(name=names(whole_betweenness),Value=whole_betweenness)
betweenness_centrality_df <- arrange(betweenness_centrality_df, desc(Value))
head(betweenness_centrality_df)


whole_eigen_centrality <- eigen_centrality(graph_relationship)
class(whole_betweennes_centrality)
length(whole_betweennes_centrality)
eigen_centrality_df <- data.frame(name=names(whole_eigen_centrality$vector),Value=whole_eigen_centrality$vector)
eigen_centrality_df <- arrange(eigen_centrality_df, desc(Value))
head(eigen_centrality_df)


whole_page_rank <- page_rank(graph_relationship)
class(whole_page_rank)
pagerank_df <- data.frame(name=names(whole_page_rank$vector),pr=whole_page_rank$vector)
pagerank_df <- arrange(pagerank_df, desc(pr))
head(pagerank_df)


#The below part is out of our syllabus. 

#Now we will try to detect clusters/communities in our network.

#We are going to use a louvain clustering algorithm. 
cluster_df <- cluster_louvain(graph_relationship)
class(cluster_df)
length(cluster_df)
summary(cluster_df)
cluster_df$memberships
cluster_df$names
community_df <- data.frame(Concepts=cluster_df$names, Community=cluster_df$membership)
head(community_df)
View(community_df)

filter(community_df, Community==1)
filter(community_df, Community==2)

filter(community_df, Concepts == "Tyrion")

#We want to calculate the total number of communities. 

table(community_df$Community)

plot(cluster_df,graph_relationship,vertex.label=community_df$Community)

plot(graph_relationship, labels=NULL,layout=layout.fruchterman.reingold(graph_relationship,weights=E(graph_relationship)$Weight),vertex.color=rainbow(length(unique(community_df$Community))
, alpha=0.9)[cluster_df$membership],vertex.label=community_df$Community)


plot(graph_relationship, labels=NULL,layout=layout.fruchterman.reingold(graph_relationship,weights=E(graph_relationship)$Weight),vertex.color=rainbow(length(unique(community_df$Community))
                                                                                  , alpha=0.9)[cluster_df$membership],vertex.label=V(graph_relationship))
