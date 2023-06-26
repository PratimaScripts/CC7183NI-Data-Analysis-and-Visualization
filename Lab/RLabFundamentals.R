
remove(list=ls()) # Clearing your environment 

setwd("D:/Development/CC7183NI-Data-Analysis-and-Visualization/Lab") # Setting your working directory 

### Atomic Structure of Data ###

### Basic/Atomic Data Types 

# Character, Number, Boolean

# "name", 20, TRUE

### Assignment Operator 
my_variable = 10 
my_variable <- 10 # Conventional assignment operator in R. Avoid = 

### Vector Data Type ### Don't confuse with mathematical vector
### Vector (vector basically stores collection of data of same types)
age = 20 
total_ages = c(10, 20, 30)

### Indexing the individual values
total_ages[1] # Indexing starts from 1 

total_names <- c("Jennifer", "Aleksandra")

class(total_names) # To see the data type

total_info <- c(1, 2, 3, "Jennifer") # Implicit Coercion (When type conversion occurs by itself in R) 
class(total_info)

total_ages*10

### List
jennifer_info <- list("Jennifer", 26)
jennifer_info[2]

my_list <- list(name="Nepal", districts=c("Kathmandu", "Lalitpur", "Bhaktapur"), days = 10)
my_list$districts # Dollar Operator 
my_list$districts[2]

class(my_list)

# Matrix Data Type
matrix(c(1, 2, 3, 4), nrow = 2)

1:4 # This give me vector from 1 to 4 

mat1 <- matrix(1:4, nrow = 2)
mat2 <- matrix(1:4, nrow = 2)

mat1*mat2 # Point wise matrix multiplication 

mat1 %*% mat2 # Mathematical matrix multiplication 

# Factor data type. For Categorical data # 

factor(c("Male", "Female", "Male", "Male"))
factor(c("Nepal", "Vietnam", "Bhutan"))

### Data Frame ### The most used data structure in Data Analysis ###

# CSV # Basic properties 
# Rows and Column # Rectangular/Tabular 
# All columns of same length (Length of all your columns should be equal)
# Data types within each column of same type
# Different columns can have different data type 
# There is only one dimension/shape of your data frame. 

### Data Frame is basically collection of multiple vectors (could be of different type) of same length 

## Example 1
my_df <- data.frame(age = c(1, 2, 3, 4), name = c("A","B","C","D"))
my_df[1,1] # First Row, First Column 
my_df[1,2] # First Row, Second Column 

my_df$name[2] # Second element of name variable/column 

## Example 2
country_df <- data.frame(country=c("Nepal", "Vietnam", "Turkey"), 
                         capital=c("Kathmandu", "Hanoi", "Ankara"),
                         area = c(20, 30, NA),
                         code = 1)

country_df$country[2] # Give me second element of country column
country_df$capital[3]

country_df[1, 3] # Give me first row third column value 
country_df[3, ] # Give me third row all column 

1:10 # range 
country_df[1:2, 3] # Give me 1st to 2nd row, and 3rd column 

### Factor ### 

# Salary Name Age Gender 

# Number Character Number Character 
# For variables (usually for character) that has deeper meaning, we use factor data type 
# Factor in R = Category in Python 

unclass(factor(c("Male", "Female", "Male")))

my_df

my_df$name <- as.factor(my_df$name)
my_df["name"] = my_df["name"].as_category()

new_df <- data.frame(district=c("Kathmandu", "Lalitpur", "Bhaktapur"), area = 141181)

### Using different libraries/packages in R (most frequently imported) ###
install.packages("dplyr") # To install dplyr package # Data Analysis
install.packages("ggplot2") # Data Visualization

### Importing the library in R 
library(dplyr)

library(readr)

# More libraries means more dependencies.
# Use standard libraries only if needed.

data() # You can see many built in data sets of R

mtcars #Built in Data Set in R  

help("mtcars")

class(mtcars) 
mtcars[3, 4]
mtcars[1:10, ]
mtcars$mpg
mtcars$carb[10]

df <- mtcars # Usually df is noted for dataframe 

head(df, 3) # df.head() # To see the initial first few observations 
tail(df)

dim(df) # df.shape  # To know the dimension/shape of the dataframe 
View(df) # To see your dataset in the form of Excel type sheet 

sapply(df, mean) # This applies the function mean to all variables of df 

### Basic Data Analysis and Manipulation ### 

mean(df$wt)

# Filter Operation 

filter(df, df$wt > 3.21) # To filter all the rows that satisfy the condition 

df[df$wt>3.21, ] # 

df[1, 5] # Give me 1st row 5th column value 
df[1, ] # Give me 1st row and all column value


filter(df, wt > 3.21 & cyl ==6) # You can club different conditions 
filter(df, wt > 3.21 | cyl ==6) # Or operator 


### Select Operation ###

select(df, mpg, wt, cyl) # Select certain columns out of all 
df[, c("mpg", "cyl")] # Same operation without dplyr # Alternative 

### Sorting / Arranging the dataset ###

arrange(df, wt) # By default ascending order 
arrange(df, desc(wt)) # Now sorts the dataframe on the basis of descending order

order() # Using order function also you can sort your dataframe 

table(df$wt) # Just to see the frequency count of values
table(df$cyl) # Helps you know if a numerical variable is continuous or categorical 

# %>% Pipe Operator # Used in combining multiple operators 
group_by(df, cyl) %>% summarise(mean(wt))

filter(df, cyl == 6) %>% select(wt, cyl)

### Mutate # To change your dataset by adding/updating variables 

df <- mutate(df, new_wt = wt*100) # Adding a new variable 

df <- mutate(df, new_wt = new_wt/100) # Updating existing variable 

df$extra_weight <- df$wt * 100 # This is also same as mutate 
head(df)

rename(df, renamed_weight = extra_weight) # To rename a variable 
colnames(df) # To see the column names of dataframe 
# colnames(df) <- c("") # You could give a full list of new names this way

# To create user defined function 
my_function <- function(x, y){
  return(x+y)
}

my_function(2, 3)


### Now we will see Data Visualization ###


library(ggplot2)

### gg = Grammar of Graphics  # 2 basically means 2nd version 

# Univariate Analysis # 

# Histogram vs Bar Plot # 

table(df$wt)

hist(df$wt, main="Histogram of Weight")

plot(density(df$wt)) # To see the distribution/density of your variable

table(df$cyl)

barplot(df$cyl)
barplot(table(df$cyl)) # Now this is problematic 

boxplot(df$wt) # Plots 5 num summary along with outliers 
fivenum(df$wt) # This gives me 5 num summary 

### Bivariate Analysis ###

plot(x=df$wt, y = df$mpg, main="Scatter Plot", xlab = "Weight", ylab = "Mileage") 

plot(df$wt, df$mpg)
ggplot(df, aes(wt, mpg)) + geom_point()

### Grammar of Graphics ###

### First Part: Data 
### Second Part: Aesthetics (Variables that we are concerned with)
### Third Part: Geometry (Which shape/figure that we want to plot)


ggplot(df, aes(wt)) + geom_histogram()
ggplot(df, aes(cyl)) + geom_bar()


ggplot(df, aes(x=wt, y=mpg, shape=am)) + geom_point()

table(df$wt)

### Changing the type of variable ### From cont to factor

df$am <- as.factor(df$am) # To change into factor/categorical 

class(df$am)
class(df$cyl)

my_plot <- ggplot(df, aes(x=wt, y=mpg, shape=as.factor(carb), color=as.factor(cyl))) + geom_point()

my_plot + labs(x="Weight", y="Mileage")

my_plot + geom_smooth(method="lm")

ggplot(df, aes(wt, mpg))+geom_point()+geom_smooth(method="lm")

# Filter Operation # Selecting certain observations/rows based on some condition
mean(mtcars_df$wt)
filter(mtcars_df, wt>3.21) # Filter out all the observations with weight greater than mean
mtcars_df[mtcars_df$wt>3.21, ] # This traditional R operation does the same thing as above

filter(mtcars_df, wt>3.21 & am==0) # To club/combine different conditions for filter
filter(mtcars_df, wt>3.21 | am==0) 

### Mutate Operation 
df$new_mpg <- df$mpg * 100
df <- mutate(df, mutate_mpg=mpg*100)

head(df)
df$new_mpg <- df$new_mpg*1000

### Groupby and Summarise
group_by(df, cyl) %>% summarise(mean(wt)) #  %>% Pipeline operator
group_by(df, cyl) %>% summarise(max(wt)) 

group_by(df, am) %>% summarise(mean(mpg))
group_by(df, am) %>% summarise(sd(mpg))

max(df$wt)

# Select am and wt variable of vehicles with wt>4
filter(df, wt>4) %>% select(am, wt)
select(filter(df, wt>4) , am, wt)


f(g(h(x))) # Composite Function 
h(x) %>% g() %>% f() # Same line as above 

df <- rename(df, new_wt=wt) # Rename your column

my_func <- function(a, b){
  return(a+b)
}

my_func(10, 20)

# Merging/Joining multiple data frames with common variable 
mean(df$wt)

# Different vectorization/apply functions in R 

sapply(df, min)
sapply(df, class)

# Map, Reduce in R and Python 
help(merge)

