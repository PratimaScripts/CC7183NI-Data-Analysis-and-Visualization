# ----------------------------------------------------
# ----------------SENTIMENT ANALYSIS------------------
# ----------------------------------------------------

# Command + Shift + C on macOS to comment out a line of text

# install.packages("dplyr")
# install.packages("tidytext")
# install.packages("ggplot2")

setwd("D:/Development/CC7183NI-Data-Analysis-and-Visualization/TextAnalysis")

library(dplyr)
library(tidytext)
library(ggplot2)

# LOAD THE DATASETS
depressed <- read.csv("depressed.csv")
depression_help <- read.csv("depression_help.csv")
depressionrecovery <- read.csv("depressionrecovery.csv")

# WE NEED A SOURCE COLUMN
depressed$source <- "depressed"
depression_help$source <- "depression_help"
depressionrecovery$source <- "depressionrecovery"

depression <- rbind(depressed, depression_help, depressionrecovery)

# TOKENIZING
tokens <- depression %>%
  unnest_tokens(word, selftext)

# COUNTING WORDS
tokens %>%
  count(word) %>%
  arrange(desc(n))

# STOP WORDS
tokens_clean <- depression %>%
  unnest_tokens(word, selftext) %>%
  anti_join(stop_words)

# FILTERING THE WORDS FROM DEPRESSED
tokens_clean %>%
  filter(source == "depressed") %>%
  count(word) %>%
  arrange(desc(n))

# FILTERING THE WORDS FROM DEPRESSION HELP
tokens_clean %>%
  filter(source == "depression_help") %>%
  count(word) %>%
  arrange(desc(n))

# FILTERING THE WORDS FROM DEPRESSION RECOVERY
tokens_clean %>%
  filter(source == "depressionrecovery") %>%
  count(word) %>%
  arrange(desc(n))

# CREATING A BAR PLOT
tokens_id <- depression %>%
  mutate(id = row_number()) %>%
  unnest_tokens(word, selftext) %>%
  anti_join(stop_words)

# CREATING A BAR PLOT WITH WORD CORD FLIP
word_count <- tokens_id %>%
  count(word) %>%
  filter(n > 10000) %>%
  arrange(desc(n))

ggplot(
  word_count, aes(x = word, y = n)
) +
  geom_col() +
  coord_flip()

# BAR PLOT FOR DIFFERENT SOURCES - DEPRESSED
word_count <- tokens_id %>%
  filter(source == "depressed") %>%
  count(word) %>%
  filter(n > 4000) %>%
  arrange(desc(n))

ggplot(
  word_count, aes(x = word, y = n)
) +
  geom_col() +
  coord_flip()

# BAR PLOT FOR DIFFERENT SOURCES - DEPRESSIONRECOVERY
word_count <- tokens_id %>%
  filter(source == "depressionrecovery") %>%
  count(word) %>%
  filter(n > 100) %>%
  arrange(desc(n))

ggplot(
  word_count, aes(x = word, y = n)
) +
  geom_col() +
  coord_flip()

# BAR PLOT FOR DIFFERENT SOURCES - DEPRESSION_HELP
word_count <- tokens_id %>%
  filter (source == "depression_help") %>%
  count(word) %>%
  filter (n >1000) %>%
  arrange (desc(n))

ggplot(
  word_count, aes (x = word, y = n)
) +
  geom_col() +
  coord_flip ()

# REMOVING CUSTOM STOP WORDS
custom_stop_words <- tribble(
  ~word, ~lexicon,
  "wordexample1" , "CUSTOM",
  "wordexample2" , "CUSTOM"
)

stop_words_new <- stop_words %>%
  bind_rows(custom_stop_words)

# PLOTTING AFTER CUSTOM STOP WORDS
tokens_id <- depression %>%
  mutate (id = row_number()) %>%
  unnest_tokens(word, selftext) %>%
  anti_join (stop_words_new)

# PLOTTING AFTER CUSTOM STOP WORDS - DEPRESSED
word_count <- tokens_id %>%
  filter (source == "depressed") %>%
  count(word) %>%
  filter (n >100) %>%
  mutate(word2 = fct_reorder (word, n))

ggplot(
  word_count, aes (x = word2, y = n)
) +
  geom_col() +
  coord_flip ()

# PLOTTING AFTER CUSTOM STOP WORDS - DEPRESSIONRECOVERY
word_count <- tokens_id %>%
  filter (source == "depressionrecovery") %>%
  count(word) %>%
  filter (n >100) %>%
  mutate(word2 = fct_reorder (word, n))

ggplot(
  word_count, aes (x = word2, y = n)
) +
  geom_col() +
  coord_flip ()

# PLOTTING AFTER CUSTOM STOP WORDS - DEPRESSION_HELP
word_count <- tokens_id %>%
  filter (source == "depression_help") %>%
  count(word) %>%
  filter (n >100) %>%
  mutate(word2 = fct_reorder (word, n))

ggplot(
  word_count, aes (x = word2, y = n)
) +
  geom_col() +
  coord_flip ()

# CREATING WORD CLOUDS FOR DEPRESSED
library (wordcloud)
word_counts <- tokens_id %>%
  filter(source == "depressed") %>%
  count(word)

wordcloud(
  words = word_counts$word,
  freq = word_counts$n,
  max.words = 20,
  colors = "red"
)

# CREATING WORD CLOUDS FOR DEPRESSION HELP
library (wordcloud)
word_counts <- tokens_id %>%
  filter(source == "depression_help") %>%
  count(word)

wordcloud(
  words = word_counts$word,
  freq = word_counts$n,
  max.words = 20,
  colors = "red"
)

# CREATING WORD CLOUDS FOR DEPRESSIONRECOVERY
library (wordcloud)
word_counts <- tokens_id %>%
  filter(source == "depressionrecovery") %>%
  count(word)

wordcloud(
  words = word_counts$word,
  freq = word_counts$n,
  max.words = 20,
  colors = "red"
)

# TOPIC MODELLING
# CREATING A DTM
dtm_output <- tokens_id %>%
  count(word, id) %>%
  cast_dtm(id, word, n)

# CREATING A MODEL BASED ON LDA
# install.packages("topicmodels")
library(topicmodels)

lda_output_two <- LDA(
  dtm_output,
  k = 2,
  method = "Gibbs",
  control = list (seed = 42)
)

glimpse(lda_output_two)

# VISUALIZING TOPICS

# install.packages('reshape2')

library(reshape2)

lda_topics_two <- lda_output_two %>%
tidy(matrix = "beta") %>%
arrange(desc(beta))

# GROUPING TOPICS
word_probs_two <- lda_topics_two %>%
  group_by(topic) %>%
  top_n(15, beta) %>%
  ungroup() %>%
  mutate(term2 = fct_reorder(term, beta))

ggplot(
  word_probs_two,
  aes(term2, beta, fill = as.factor(topic))
) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()


