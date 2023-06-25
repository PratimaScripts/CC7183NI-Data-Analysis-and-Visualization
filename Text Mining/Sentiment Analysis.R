setwd("~/Desktop/Islington Lab")

remove(list = ls())

library(pdftools) #reading pdf
library(dplyr)
library(NLP) #NLP
library(tm) #Text Mining 
library(ggplot2)
library(RWeka) #Text 
#library(gplots)
#library(corrplot)
library(RColorBrewer)
#library(slam)
#library(topicmodels)
library(syuzhet) #Sentiment Analysis 
#library(stringr)
#library(tidytext)
library(tokenizers) #Text Mining
library(wordcloud) #Word Cloud
#library(plotly)
library(highcharter) #Data Visualisation 

text <- pdf_text("1951 Constitution English.pdf")
length(text)
class(text)
text[10]


### I am splitting on the basis of space here. ###
text <- strsplit(text, "\n")
sentenceCorpus <- unlist(text, recursive=FALSE)
length(sentenceCorpus)

sentences_df <- data.frame(sentences = sentenceCorpus)
dim(sentences_df)
head(sentences_df)

### SENTIMENT ANALYSIS ###


# Emotions for each tweet using NRC dictionary
emotions <- get_nrc_sentiment(as.character(sentences_df$sentences))

get_nrc_sentiment("Politics")
get_nrc_sentiment("Love")
head(emotions)
dim(emotions)
emo_bar = colSums(emotions)
emo_sum = data.frame(count=emo_bar, emotion=names(emo_bar))
emo_sum$emotion = factor(emo_sum$emotion, levels=emo_sum$emotion[order(emo_sum$count, decreasing = TRUE)])

head(emotions)
emo_sum_arranged <- arrange(emo_sum,desc(count))
head(emo_sum_arranged)


highchart() %>% 
  hc_chart(type = "column") %>% 
  hc_title(text = "Sentiment Analysis") %>% 
  hc_colors("#00B2FF") %>% 
  hc_xAxis(categories = emo_sum_arranged$emotion) %>% 
  hc_add_series(data = emo_sum_arranged$count,
                name = "Bar Graph")

# Create comparison word cloud data

wordcloud_tweet = c(
  paste(sentences_df$sentences[emotions$anger > 0], collapse=" "),
  paste(sentences_df$sentences[emotions$anticipation > 0], collapse=" "),
  paste(sentences_df$sentences[emotions$disgust > 0], collapse=" "),
  paste(sentences_df$sentences[emotions$fear > 0], collapse=" "),
  paste(sentences_df$sentences[emotions$joy > 0], collapse=" "),
  paste(sentences_df$sentences[emotions$sadness > 0], collapse=" "),
  paste(sentences_df$sentences[emotions$surprise > 0], collapse=" "),
  paste(sentences_df$sentences[emotions$trust > 0], collapse=" ")
)

length(wordcloud_tweet)

# create corpus
corpus = Corpus(VectorSource(wordcloud_tweet)) 


# remove punctuation, convert every word in lower case and remove stop words
removal_words <- c("Try")
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, c(stopwords("english"),removal_words))
#corpus = tm_map(corpus, stemDocument) #beautiful, beauty -> beauti 

# create document term matrix

tdm = TermDocumentMatrix(corpus) 
dim(tdm)
inspect(tdm)

# convert as matrix
tdm = as.matrix(tdm)
#tdmnew <- tdm[nchar(rownames(tdm)) < 11,]

# column name binding
colnames(tdm) = c('anger','anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise', 'trust')

comparison.cloud(tdm, random.order=FALSE,
                 colors = c("#00B2FF","grey", "red", "#FF0099", "green", "orange", "blue", "brown"),
                 title.size=0.8, max.words=500, scale=c(1.7, 0.5),rot.per=0.3)


dict <- get_sentiment_dictionary('nrc')

### Find out which words are under which emotion category ###

dict_df <- data.frame(word=dict$word, sentiment=dict$sentiment)
head(dict_df)
tail(dict_df)
View(dict_df)
