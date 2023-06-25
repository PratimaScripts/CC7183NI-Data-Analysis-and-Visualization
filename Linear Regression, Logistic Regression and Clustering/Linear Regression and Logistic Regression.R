### Things to cover ###

# 1. Simple Linear Regression
# 2. Multiple Linear Regression (Concept of P-Value and R Squared too)
# 3. Factor Variables as independent variables
# 4. Model Selection
# 5. Variable Selection
# 6. Prediction in Linear Regression
# 7. Cross Validation 
# 8. Logistic Regression
# 9. Prediction in Logistic Regression


remove(list=ls())

mtcars_df <- mtcars
head(mtcars_df)
View(mtcars_df)

table(mtcars_df$cyl)
table(mtcars_df$am)

### Starting with Linear Regression ###

### Simple Linear Regression ### 

### Mileage (mpg) vs Weight (wt)

plot(mtcars_df$wt, mtcars_df$mpg)

cor(mtcars_df$mpg, mtcars_df$wt)

### We need to find the relationship between mpg and wt, that's why we need regression.


# output ~ input # Output regresses on input

### ~ regresses to : Output ~ Input 

first_model <- lm(mpg ~ wt, data = mtcars_df) #Linear Model where mpg regresses on wt
summary(first_model)



### mpg = 37.285 - 5.344 * wt, this is the equation we get after regression 

### wt = 1, mpg = 37.285 - 5.344 *1 
### wt = 2, what is the mpg? mpg = 37.285 - 5.344 * 2
### wt = 3, mpg = 37.285 - 5.344 * 3
### wt = 10, mpg = 37.285 - 5.344 * 10

### salary = Intercept + 5*Education 

# Concept of R Square 

# Y, X, Y= B*X, after that we predict Y1 as B1*X1

# R Square = (Y-Ymean)^2 - (Y-Y-predicted)^2 / (Y - Ymean)^2
# R Square = Variation of Mean - Variation of Model / Variation of Mean 

### Total difference = 100 
### 100 - 100/100 = 0, so 0 percent information is captured by the model 
### 100 - 0 / 100 = 1, so 100 percent information is captured by the model 
### 100 - 20 /100 = 80 percent 


weight_model <- lm(mpg ~ wt, data = mtcars_df)
summary(weight_model)

# R square is square of correlation (R). 
# R square is the percentage of variation in output variable captured by our input variable. 
# 0 R square means no variation in output captured by input, 1 means all variation in output captured by input.

### Residuals / Errors test 
### Residuals should be random and near to normal distribution 

weight_model$residuals
plot(weight_model$residuals)
abline(0, 0) # First point is intercept, second is slope 
hist(weight_model$residuals)
plot(density(weight_model$residuals))


model_weight_hp <- lm(mpg ~ wt + hp + drat + disp, data=mtcars_df)
summary(model_weight_hp)
# mpg = 29.14 - 3.47*wt - 0.03*hp + 1.76*drat + 0.003*disp



mtcars_df$cyl <- as.factor(mtcars_df$cyl)
table(mtcars_df$cyl)

model_with_cyl <- lm(mpg ~ wt + hp + cyl, data = mtcars_df)
summary(model_with_cyl)
head(mtcars_df)



factor_variables <- c("cyl","vs","am","gear","carb")
mtcars_df[factor_variables] <- lapply(mtcars_df[factor_variables], factor)
sapply(mtcars_df, class)


lm(mpg ~ wt + am, data = mtcars_df)

#Y ~. , this means Y on whole input 


model_all <- lm(mpg ~., data = mtcars_df) # . means all
summary(model_all)


#install.packages("MASS")
#install.packages("caret")

library(MASS)
library(caret)

stepAIC(model_all,direction = "both")



predict(model_best, mtcars_df[25:32, c("wt","qsec","am")]) # Model, Test Input
mtcars_df[25:32, 'mpg']


View(mtcars_df)

sample(1:100,10) # Selects 10 random numbers between 1 and 100 

random_sample <- sample(1:32, 25)
random_sample

mtcars[3, 4] # 3rd row, 4th Column 
mtcars[-3, 4] # All rows except 3rd, 4th Column 




mtcars_df_train <- mtcars_df[random_sample,]# Rows random sample 25 observation, all column
dim(mtcars_df_train)
mtcars_df_test <- mtcars_df[-random_sample,]
dim(mtcars_df_train);dim(mtcars_df_test)


model_final <- lm(mpg ~ wt + hp + am + cyl, data = mtcars_df_train)
predict(model_final,mtcars_df_test[,c("wt","hp","am","cyl")])
mtcars_df_test['mpg']




### Logistic regression and other types of regressions come under General Linear Model. 

mtcars_df <- mtcars
head(mtcars_df)

random_sample <- sample(1:32, 25)
random_sample

mtcars_df_train <- mtcars_df[random_sample,] # Rows random sample 25 observation, all column
mtcars_df_test <- mtcars_df[-random_sample,]
dim(mtcars_df_train);dim(mtcars_df_test)

glm(am ~ wt, data = mtcars_df, family = "binomial")
# I am trying to create a model where output is am variable, and input is wt, hp and cyl 

# family = "binomial" this means you are doing a binary classifiction that is 0 vs 1. 


## You find Beta. Then you calculate B0 + B1*X1 + B2*2 + B3*X3

### When we have wt, hp and cyl
### y = B0 + B1*wt + B2*hp + B3*cyl

### Now we pass this y through Sigmoid Function. 

### e^(B0 + B1*wt + B2*hp + B3*cyl) / 1 + e ^ (B0 + B1*wt + B2*hp + B3*cyl)


model_train_glm <- glm(am ~ wt + hp + cyl, data = mtcars_df_train, family = "binomial")

predict(model_train_glm, mtcars_df_test[,c("wt","hp","cyl")])
mtcars_df_test[,"am"]

predict(model_train_glm, mtcars_df_test[,c("wt","hp","cyl")],type = "response")

### Since I want to use 0.5 as the decision boundary, I am simply rounding off the probabilities. 
predicted <- round(predict(model_train_glm, mtcars_df_test[,c("wt","hp","cyl")],type = "response"))

predicted
mtcars_df_test[,"am"]

#create confusion matrix
confusionMatrix(as.factor(mtcars_df_test[,"am"]), as.factor(predicted))
