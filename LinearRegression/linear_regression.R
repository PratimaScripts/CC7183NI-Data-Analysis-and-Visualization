setwd("~/Desktop/")

remove(list = ls())

install.packages("dplyr")
install.packages("caTools")
library(dplyr)
library(caTools)

# LOAD THE SALARY DATASET
salary_df <- read.csv("salary.csv")
model <- lm(Salary~YearsExperience, data = salary_df)
summary(model)
new_data <- data.frame(YearsExperience = 20)
prediction <- predict(model,new_data)

# LOAD THE HOUSING DATASET
housing_df <- read.csv("housing.csv")
head(housing_df)
df_scaled <- housing_df%>% select(-price)%>% scale() %>% as.data.frame()
head(df_scaled)

# sigmoid - 0 to 1 capping

# TRAIN TEST SPLIT
library(caTools)
split = sample.split(df_scaled, SplitRatio=0.8)
df_train = subset(df_scaled, split == TRUE)
df_test = subset(df_scaled, split == FALSE)


# MODEL BUILDING
df_scaled$price <- housing_df$price
# run the df_test and df_train after adding the price because earlier we have removed the price
model <- lm(price~.,data = df_train)
summary(model)
prediction <- predict(model, newdata = df_test)


# TESTING
# MEAN SQUARE ERROR (MSE)
MSE <- mean((df_test$price - prediction)^2)
RMSE <- sqrt(mean((df_test$price-prediction)^2))
R2 <- 1 - sum((prediction - df_test$price)^2)/sum((df_test$price - mean(df_test$price))^2)
# Adjusted R2
R2adj <- 1 - (1-R2)*(nrow(df_test)-1)/(nrow(df_test)-ncol(df_test)-1)
cor(df_scaled) # find the correlations between the variables here