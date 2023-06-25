setwd("D:/Development/CC7183NI-Data-Analysis-and-Visualization/HypothesisTesting")

install.packages("dplyr")
install.packages("magrittr")

library(dplyr)
library(ggplot2)

# CALCULATING THE P-VALUE
titanic_df <- read.csv("titanic.csv")

head(titanic_df)
dim(titanic_df)
hist(titanic_df$Age)
mean(titanic_df$Age)
hist(titanic_df$Survived) # Not usable

titanic_clean <- na.omit(titanic_df)
titanic_survivors <- titanic_clean %>%
filter(Survived == 1)
hypothesis_average <- 40
sample_average <- mean(titanic_survivors$Age)
n <- nrow(titanic_survivors)

# Standard Error
# CALCULATING THE P-VALUE
sample_deviation <- sd(titanic_survivors$'Age')

standard_error <- sample_deviation /sqrt (290L)
standard_error <- sample_deviation / sqrt(n)

# Z Score
z_score <- (sample_average - hypothesis_average) / standard_error

# ONE TAILED TEST
p_value <- pnorm (z_score, lower.tail = TRUE)

# TWO TAILED TEST
p_value <- 2*(1 - pnorm(abs(z_score)))


#Hypothesis 1 - Average fare is less than 30

hypothesis_average_fare <- 30
sample_average_fare <- mean(titanic_clean$Fare)

n <- nrow(titanic_clean)
