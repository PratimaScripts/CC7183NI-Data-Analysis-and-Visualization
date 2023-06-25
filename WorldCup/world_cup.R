# ----------------------------------------------------
# -------------------TEXT ANALYSIS--------------------
# ----------------------------------------------------

remove(list=ls()) # Start from a clean environment 

setwd("D:/Development/CC7183NI-Data-Analysis-and-Visualization/WorldCup") # Set your working directory 

worldcup <- read.csv("world_cup.csv", header=T, stringsAsFactors = F)
summary(worldcup)

# install.packages("dplyr")
# install.packages("sentimentr")
# install.packages("tidyverse")
# install.packages("tidytext")
# install.packages("topicmodels")
# install.packages("ggplot2")


library(dplyr)
library(sentimentr)
library(tidyverse)
library(tidytext)
library(topicmodels)
library(ggplot2)

# FILTERING AND COUNTING
worldcup %>%
  filter (source_of_tweet == "Twitter Web App") %>%
  count(sentiment)

# GROUPBY
worldcup %>%
  group_by (sentiment) %>%
  summarize (
    avg = mean (number_of_likes)
  )

# NUMBER OF TWEETS IN EACH SENTIMENT
worldcup %>%
  group_by (sentiment) %>%
  summarize (
    avg = mean (number_of_likes)
    , n = n ()
  )

# COUNTING SOURCES OF USERS IN EACH SENTIMENT
worldcup %>%
  filter (sentiment == "positive") %>%
  count (source_of_tweet)


# TOKENIZING
tokens <- worldcup %>%
  unnest_tokens(word, tweet)
dim (worldcup)

# COUNTING WORDS
tokens %>%
  count(word) %>%
  arrange(desc(n))

# STOP WORDS
tokens_clean <- worldcup %>%
  unnest_tokens(word, tweet) %>%
  anti_join (stop_words)

# FILTERING THE WORDS IN POSITIVE TWEETS
tokens_clean %>%
  filter (sentiment == "positive") %>%
  count(word) %>%
  arrange(desc(n))

# FILTERING THE WORDS IN NEGATIVE TWEETS
tokens_clean %>%
  filter (sentiment == "negative") %>%
  count(word) %>%
  arrange(desc(n))

# FILTERING THE WORDS IN NEUTRAL TWEETS
tokens_clean %>%
  filter (sentiment == "neutral") %>%
  count(word) %>%
  arrange(desc(n))

#CREATING A BAR PLOT
tokens_id <- worldcup %>%
  mutate (id = row_number()) %>%
  unnest_tokens(word, tweet) %>%
  anti_join (stop_words)

word_count <- tokens_id %>%
  count(word) %>%
  arrange (desc(n))

ggplot(
  word_count, aes (x = word, y = n)
) +
  geom_col()

# CREATING A BAR PLOT WITH WORD LIMIT
word_count <- tokens_id %>%
  count(word) %>%
  filter (n >500) %>%
  arrange (desc(n))

ggplot(
  word_count, aes (x = word, y = n)
) +
  geom_col()

# CREATING A BAR PLOT WITH WORD CORD FLIP
word_count <- tokens_id %>%
  count(word) %>%
  filter (n >500) %>%
  arrange (desc(n))

ggplot(
  word_count, aes (x = word, y = n)
) +
  geom_col() +
  coord_flip ()

# BAR PLOT FOR POSITIVE TWEETS
word_count <- tokens_id %>%
  filter (sentiment == "positive") %>%
  count(word) %>%
  filter (n >500) %>%
  arrange (desc(n))

ggplot(
  word_count, aes (x = word, y = n)
) +
  geom_col() +
  coord_flip ()

# BAR PLOT FOR NEGATIVE TWEETS
word_count <- tokens_id %>%
  filter (sentiment == "negative") %>%
  count(word) %>%
  filter (n >500) %>%
  arrange (desc(n))

ggplot(
  word_count, aes (x = word, y = n)
) +
  geom_col() +
  coord_flip ()

# BAR PLOT FOR NEUTRAL TWEETS
word_count <- tokens_id %>%
  filter (sentiment == "neutral") %>%
  count(word) %>%
  filter (n >500) %>%
  arrange (desc(n))

ggplot(
  word_count, aes (x = word, y = n)
) +
  geom_col() +
  coord_flip ()

# REMOVING CUSTOM STOP WORDS
custom_stop_words <- tribble(
  ~word, ~lexicon,
  "worldcupqatar2022" , "CUSTOM",
  "worldcup2022" , "CUSTOM"
)

stop_words_new <- stop_words %>%
  bind_rows(custom_stop_words)

# PLOTTING AFTER CUSTOM STOP WORDS
tokens_id <- worldcup %>%
  mutate (id = row_number()) %>%
  unnest_tokens(word, tweet) %>%
  anti_join (stop_words_new)

# PLOTTING AFTER CUSTOM STOP WORDS
word_count <- tokens_id %>%
  filter (sentiment == "positive") %>%
  count(word) %>%
  filter (n >500) %>%
  mutate(word2 = fct_reorder (word, n))

ggplot(
  word_count, aes (x = word2, y = n)
) +
  geom_col() +
  coord_flip ()

# CREATING WORD CLOUDS
# install.packages("wordcloud")
library (wordcloud)
word_counts <- tokens_id %>%
  count(word)
wordcloud(
  words = word_counts$word,
  freq = word_counts$n,
  max.words = 20,
  colors = "red"
)

# CREATING WORD CLOUDS FOR POSITIVE SENTIMENT
library (wordcloud)
word_counts <- tokens_id %>%
  filter(sentiment == "positive") %>%
  count(word)

wordcloud(
  words = word_counts$word,
  freq = word_counts$n,
  max.words = 20,
  colors = "red"
)

# CREATING WORD CLOUDS FOR NEGATIVE SENTIMENT
library (wordcloud)
word_counts <- tokens_id %>%
  filter(sentiment == "positive") %>%
  count(word)

wordcloud(
  words = word_counts$word,
  freq = word_counts$n,
  max.words = 20,
  colors = "red"
)

# CREATING WORD CLOUDS FOR NEUTRAL SENTIMENT
library (wordcloud)
word_counts <- tokens_id %>%
  filter(sentiment == "positive") %>%
  count(word)

wordcloud(
  words = word_counts$word,
  freq = word_counts$n,
  max.words = 20,
  colors = "red"
)

# ----------------------------------------------------
# ------------------SENTIMENT ANALYSIS----------------
# ----------------------------------------------------

# SENTIMENTR
install.packages("sentimentr")
install.packages("tidyverse")
install.packages("tidytext")
Install.packages("topicmodels")

library("sentimentr")

# BING
get_sentiments('bing') %>%
  count(sentiment) %>%
  arrange(desc(n))

sentiment_counts <- get_sentiments('bing') %>%
  count(sentiment) %>%
  mutate(sentiment2 = fct_reorder(sentiment,n))

ggplot(sentiment_counts, aes(x=sentiment2, y=n))
geom_col() +
  coord_flip()

# drop the sentiment column
tokens_id <- worldcup %>%
  mutate(id=row_number()) %>%
  unnest_tokens(word,tweet) %>%
  anti_join(stop_words)

twitter_clean <- subset(tokens_id, select = -sentiment)

head(twitter_clean)

# install.packages('textdata')
library(textdata)

# Finding Sentiment with dictionaries
sentiment_twitter_loughran <- twitter_clean %>%
  inner_join(get_sentiments('loughran'))

#dimension
dim(sentiment_twitter_loughran)

dim(twitter_clean)

sentiment_twitter_bing <- twitter_clean %>%
  inner_join(get_sentiments('bing'))

dim(sentiment_twitter_bing)

# checking for sentiments in each
sentiment_twitter_loughran %>%
  count(sentiment) %>%
  arrange(desc(n))

sentiment_twitter_bing %>%
  count(sentiment) %>%
  arrange(desc(n))

# Analyzing with Loughran - uncertainty

word_count <- sentiment_twitter_loughran %>%
  filter(sentiment =="uncertainty") %>%
  count(word) %>%
  mutate(word2 = fct_reorder(word,n))

ggplot(
  word_count, aes(x=word2, y=n)
) +
  geom_col() +
  coord_flip()

# filtering count - frequency
word_count <- sentiment_twitter_loughran %>%
  count(word) %>%
  filter (n >50) %>%
  arrange (desc(n))

ggplot(
  word_count, aes (x = word, y = n)
) +
  geom_col() +
  coord_flip ()

# Analyzing with Loughran - negative
word_count <- sentiment_twitter_loughran %>%
  filter(sentiment =="negative") %>%
  count(word) %>%
  filter (n >50) %>%
  mutate(word2 = fct_reorder(word,n))

ggplot(
  word_count, aes(x=word2, y=n)
) +
  geom_col() +
  coord_flip()

# Analyzing with Loughran - positive
# marketing team can use this 
word_count <- sentiment_twitter_loughran %>%
  filter(sentiment =="positive") %>%
  count(word) %>%
  filter (n >50) %>%
  mutate(word2 = fct_reorder(word,n))

ggplot(
  word_count, aes(x=word2, y=n)
) +
  geom_col() +
  coord_flip()

# Analyzing with Loughran - litigious
word_count <- sentiment_twitter_loughran %>%
  filter(sentiment =="litigious") %>%
  count(word) %>%
  #filter (n >50) %>%
  mutate(word2 = fct_reorder(word,n))

ggplot(
  word_count, aes(x=word2, y=n)
) +
  geom_col() +
  coord_flip()

# Analyzing with Loughran - constraining
word_count <- sentiment_twitter_loughran %>%
  filter(sentiment =="constraining") %>%
  count(word) %>%
  #filter (n >50) %>%
  mutate(word2 = fct_reorder(word,n))

ggplot(
  word_count, aes(x=word2, y=n)
) +
  geom_col() +
  coord_flip()


# Analyzing with BING - positive
word_count <- sentiment_twitter_bing %>%
  filter(sentiment =="positive") %>%
  count(word) %>%
  filter (n >100) %>%
  mutate(word2 = fct_reorder(word,n))

ggplot(
  word_count, aes(x=word2, y=n)
) +
  geom_col() +
  coord_flip()

# Analyzing with BING - negative
word_count <- sentiment_twitter_bing %>%
  filter(sentiment =="negative") %>%
  count(word) %>%
  filter (n >100) %>%
  mutate(word2 = fct_reorder(word,n))

ggplot(
  word_count, aes(x=word2, y=n)
) +
  geom_col() +
  coord_flip()


# Grouping Results - loughran
sentiment_twitter_loughran %>%
  count(source_of_tweet,sentiment) %>%
  spread(sentiment,n)

# Grouping Results - bing
sentiment_twitter_bing %>%
  count(source_of_tweet,sentiment) %>%
  spread(sentiment,n)

# Latent Dirichlet Allocation (LDA)
# tf - term frequency
# idf - inverse document frequency
# if-idf
# Document Term Matrix (DTM)

# creating a DTM
dtm_output <- tokens_id %>%
  count(word, id) %>%
  cast_dtm(id, word, n)

dtm_output

# creatubg a model based on LDA
lda_output_two <- LDA(
  dtm_output,
  k=2,
  method = "Gibbs",
  control = list(seed = 42)
)

glimpse(lda_output_two)

# visualizing topics

# install.packages('reshape2')
library(reshape2)

lda_topics_two <- lda_output_two %>%
  tidy(matrix = 'beta') %>%
  arrange(desc(beta))

lda_output_three <- LDA(
  dtm_output,
  k=3,
  method = "Gibbs",
  control = list(seed = 42)
)

glimpse(lda_output_three)

lda_topics_three <- lda_output_three %>%
  tidy(matrix = 'beta') %>%
  arrange(desc(beta))

# Grouping topics 2
word_probs_two <- lda_topics_two %>%
  group_by(topic) %>%
  top_n(15,beta) %>%
  ungroup() %>%
  mutate(term2 = fct_reorder(term,beta))

ggplot(
  word_probs_two,
  aes(term2, beta, fill = as.factor(topic))
) +
  geom_col(show.legent = FALSE)+
  facet_wrap(~ topic, scales="free")+
  coord_flip()

# Grouping topics 3
word_probs_three <- lda_topics_three %>%
  group_by(topic) %>%
  top_n(15,beta) %>%
  ungroup() %>%
  mutate(term2 = fct_reorder(term,beta))

ggplot(
  word_probs_three,
  aes(term2, beta, fill = as.factor(topic))
) +
  geom_col(show.legent = FALSE)+
  facet_wrap(~ topic, scales="free")+
  coord_flip()

