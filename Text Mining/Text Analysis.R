setwd("~/Desktop/Islington Lab")

remove(list = ls())

library(pdftools)
library(dplyr)
library(NLP)
library(tm)
library(ggplot2)
library(RWeka)
library(gplots)
library(corrplot)
library(RColorBrewer)
library(slam)
library(topicmodels)
library(syuzhet)
library(stringr)

# stop words : also even can just will 


text <- pdf_text("1951 Constitution English.pdf")
length(text)
topic_text <- text
length(topic_text)
text <- strsplit(text, "\n")
sentenceCorpus <- unlist(text, recursive=FALSE)

length(sentenceCorpus)

sentences_df <- data.frame(sentences = sentenceCorpus)
head(sentences_df)
colnames(sentences_df)

sentence_frame <- data.frame(doc_id=row.names(sentences_df),
                     text=sentences_df$sentences)


ds <- DataframeSource(sentence_frame)
head(ds, n = 5)


corpus <- Corpus(ds)


stop_words_manual <- c('also', 'even', 'can', 'just', 'will','may','shall','ani')
total_stop_words <- c(stop_words_manual, stopwords("english"))
length(stopwords("english"));length(total_stop_words)

skipWords <- function(x) removeWords(x, total_stop_words)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, FUN = tm_reduce, tmFuns = list(skipWords, removePunctuation, stripWhitespace, removeNumbers, stemDocument))


length(corpus)
class(corpus)

dtm <- DocumentTermMatrix(corpus)
inspect(dtm)


# Find associate words
findAssocs(dtm, c("nation"), 0.05)[[1]][1:5]

findMostFreqTerms(dtm, n = 10, INDEX = rep(1, dtm$nrow))[[1]]

freq_words <- colnames(t(findMostFreqTerms(dtm, n = 10, INDEX = rep(1, dtm$nrow))[[1]]))
freq_words


# Frequency table

m <- t(as.matrix(dtm))
freq_table <- data.frame(term = rownames(m), 
                         freq = rowSums(m), 
                         row.names = NULL)
freq_table <- freq_table[order(-freq_table$freq),][1:20,]
freq_table


# Frequency plot
freq_plot <- ggplot(freq_table, aes(x = reorder(term, -freq), freq)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(x = "Terms", y = "Frequency", title = "Frequent terms") +
  geom_text(aes(label = freq), vjust = -0.5, size = 3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
freq_plot


########### NETWORK ANALYSIS ############# 

### Graph Stuff ###

minimumFrequency <- 10

binDTM <- DocumentTermMatrix(corpus, control=list(bounds = list(global=c(minimumFrequency, Inf)), weighting = weightBin))

# Convert to sparseMatrix matrix
library(Matrix)
binDTM <- sparseMatrix(i = binDTM$i, j = binDTM$j, x = binDTM$v, dims = c(binDTM$nrow, binDTM$ncol), dimnames = dimnames(binDTM))

# Matrix multiplication for cooccurrence counts
coocCounts <- t(binDTM) %*% binDTM
dim(coocCounts)

termMatrix <- as.matrix(coocCounts)



library(igraph)
# build a graph from the above matrix
g <- graph.adjacency(termMatrix, weighted=T, mode = "undirected")
# remove loops
g <- simplify(g)
# set labels and degrees of vertices
V(g)$label <- V(g)$name
V(g)$degree <- degree(g)


# set seed to make the layout reproducible
set.seed(3952)
layout1 <- layout.fruchterman.reingold(g)
plot(g, layout=layout1)


plot(g, layout=layout.kamada.kawai)


V(g)$label.cex <- 1.25 * V(g)$degree / max(V(g)$degree)+ .4
V(g)$label.color <- rgb(0, 0, .2, .8)
V(g)$frame.color <- NA
egam <- (log(E(g)$weight)+.8) / max(log(E(g)$weight)+.8)
E(g)$color <- rgb(.5, .5, 0, egam)
E(g)$width <- egam
# plot the graph in layout1
plot(g, layout=layout.kamada.kawai)

