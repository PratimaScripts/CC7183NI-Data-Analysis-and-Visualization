setwd("~/Desktop/")

remove(list = ls())

df <- read.csv("beverage.csv")

# Outcome - binary 0 or 1

head(df)

# revise the graphs

# random_sample <- sample(1:32, 25)
# random_sample
# 
# df_beverage <- df[random_sample,] # Rows random sample 25 observation, all column
# df_test <- df[-random_sample,]
# dim(df_beverage);dim(df_test)
# 
# 
# glm( ~ fixed.acidity, data = df, family = "binomial")

df <- subset(df, select = -(type))

install.packages("dplyr")
library(dplyr)

df_scaled <- df %>% select(-quality) %>% scale() %>% as.data.frame()

df$label <- ifelse(df$quality >4, 1, 0)

df_scaled$label <- df$label

# MODEL BUILDING

install.packages("caTools")
library(caTools)
split = sample.split(df_scaled, SplitRatio = 0.8)
df_train = subset(df_scaled, split == TRUE)
df_test = subset(df_scaled, split == FALSE)


model <- glm(label~., family="binomial", data = df_train)
predict <- predict(model, df_test, type="response")

df_test$predict <- predict


# CUTOFF and ACCURACY


#install.packages("caret")
library(caret)

#create confusion matrix
confusionMatrix(as.factor(mtcars_df_test[,"am"]), as.factor(predicted))
df_test$label_final <- ifelse(df_test$predict>0.95, 1, 0)
cm <- confusionMatrix(data=as.factor(df_test$label), reference = as.factor(df_test$label_final))

# variation
df_test$label_final <- ifelse(df_test$predict>0.90, 1, 0)
cm <- confusionMatrix(data=as.factor(df_test$label), reference = as.factor(df_test$label_final))
