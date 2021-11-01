if (!require(lubridate)) install.packages("lubridate")
library(lubridate)
if (!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)
if (!require(caret)) install.packages("caret")
library(caret)
if (!require(data.table)) install.packages("data.table")
library(data.table)

################################################################################
#  MovieLens prediction                                                        #
#  --------------------------------------------------------------------------  #
#  Mgr. Lucie Schaynová, Ph.D., 2021, schaynova.lucie@seznam.cz                #
################################################################################

########################### 1) Introduction

dl <- tempfile()
movies <- str_split_fixed(readLines("ml-10M100K/movies.dat"), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))
ratings <- str_split_fixed(readLines("ml-10M100K/ratings.dat"), "\\::", 4)
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
ratings <- as.data.frame(ratings) %>% mutate(userId = as.numeric(userId),
                                             movieId = as.numeric(movieId),
                                             rating = as.numeric(rating),
                                             timestamp = as.numeric(timestamp))
movielens <- left_join(ratings, movies, by = "movieId")
set.seed(1)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)

edx <- rbind(edx, removed)

rm(dl, ratings, temp, test_index, movielens, removed)

########################### 2) Analysis

# Show first 7 rows including a header
head(edx) 

# Create new column with year of premiere and day of rate
# library(lubridate) # need to load lubridate library to use wday function
edx <- edx %>% mutate(
  year = as.numeric(str_sub(title,-5,-2)), 
  # str_sub(): replace substrings from a character vector, 
  # as.numeric(): retype to numeric
  rateday = lubridate::wday(as_datetime(timestamp), week_start = 1))
# wday(): returns the day of the week as a decimal number
# as_datetime(): converts an object to a date-time, defaults to using UTC

# Show structure of data
str(edx)

# Basic summary statistics
summary(edx) 

# Number of unique movies and users
edx %>% summarize(n_users = n_distinct(userId), n_movies = n_distinct(movieId))
# summarize(): summarize each group to fewer rows
# n_distinct(): count the number of unique values

# All unique ratings
unique(edx$rating)

# Bar chart to visualize distribution of ratings
edx %>%
  group_by(rating) %>% 
  summarize(count = n()) %>%
  ggplot(aes(x = rating, y = count)) +
  geom_col()
# group_by(): takes an existing table and converts it into a grouped table where        operations are performed
# n(): number of rows
# ggplot(aes()): declare the input data frame for a graphic
# geom_col(): depics bar chart

# The greatest number of ratings
edx  %>% group_by(movieId) %>% 
  summarize(numRatings = n(), movieTitle = first(title)) %>%
  arrange(desc(numRatings)) %>%
  top_n(1, numRatings)
# firts(): first item of an object
# arrange(): orders the rows of a data frame by the values of selected columns
# desc(): descending order
# top_n(): select top n rows by value

# Top three movies with the greatest number of ratings equal to 5.0
edx %>% filter(rating == 5.0) %>% group_by(movieId) %>% 
  summarize(numRatings = n(), movieTitle = first(title)) %>%
  arrange(desc(numRatings)) %>%
  top_n(3, numRatings)
# fitler(): gives subset of data frame, retaining all rows that satisfy conditions

# Total movie ratings per genre
edx%>%
  group_by(genres) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  top_n(3, count)

# Users activity
edx %>% count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(color = "grey") + 
  scale_x_log10()
# count(): count the number of occurrences 
# geom_histogram(): visualize the distribution of a single variable
# scale_x_log10(): log10 transformation of the scale x

# Popularity of genres per year
edx %>% group_by(year, genres) %>% 
  summarise(number = n()) %>% 
  filter(genres %in% c("Comedy","Drama","War", "Sci-Fi", "Animation", "Western")) %>%
  ggplot(aes(x = year, y = number)) +
  geom_line(aes(color=genres)) 
# c(): this function stands for 'combine', is used to get the output by giving          parameters inside the function
# geom_line(): connects the observations in order of the variable on the x axis

# Number of ratings over the years
edx %>% 
  group_by(year) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = year, y = count)) +
  geom_line() +
  geom_vline(xintercept=1995, color = "blue")
# geom_vline(): add vertical reference line to a plot

# Average ratings over the years
edx %>% group_by(year) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(year, rating)) +
  geom_point() +
  geom_smooth()
# mean(): average of numbers
# geom_poin(): create a scatterplot
# geom_smooth(): aids the eye in seeing patterns in the presence of overplotting

# New 'datetime' and 'month' columns
months <- edx %>% 
  mutate(datetime = as.POSIXct(timestamp,origin = "1970-01-01",tz = "GMT")) %>%
  mutate(month = as.integer(str_sub(datetime,6,7)) ) 
#as.POSIXct(): date-time conversion function

#  Ratings through months over the years
months %>% 
  group_by(month) %>% 
  summarize(count = n()) %>% 
  ggplot(aes(x = month,y = count)) + 
  geom_col() +
  scale_x_continuous(breaks = seq(1, 12, 1),minor_breaks = NULL) +
  scale_y_continuous(breaks = seq(0, 1000000,100000), limits=c(0,1e6), minor_breaks = NULL)

# Average ratings over the months
months %>% 
  group_by(month) %>% 
  summarize(mean = mean(rating)) %>% 
  ggplot(aes(x = month,y = mean)) + 
  geom_line() +
  scale_x_continuous(breaks = seq(1, 12, 1),minor_breaks = NULL) 

# Number of premieres per year
movies %>% mutate(year = as.numeric(str_sub(title,-5,-2))) %>%
  group_by(year) %>%
  summarise(number = n()) %>% 
  ggplot(aes(x = year, y = number)) +
  geom_line() +
  geom_vline(xintercept=2002, color = "blue")

# Activity of users
edx %>% 
  group_by(rateday) %>% 
  summarize(count = n()) %>% 
  ggplot(aes(x = rateday,y = count)) + 
  geom_line() +
  scale_x_continuous(breaks = seq(1, 7, 1),minor_breaks = NULL)

########################### 3) Model preparation

# Split the data set
edx_test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.2, list = FALSE)
edx_train <- edx[-edx_test_index,]
edx_test <- edx[edx_test_index,]

# Remove movieId and userId columns
edx_test <- edx_test %>%
  semi_join(edx_train, by = "movieId") %>%
  semi_join(edx_train, by = "userId")
#semi_join(): filter rows from x based on the presence of matches in y

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}
# function(): definition of a new function 
# sqrt(): square root of the argument

########################### 3.1.) Naive model

# Average of ratings in the training set
mu <- mean(edx_train$rating)
message('average:')
mu

baseline_rmse <- RMSE(edx_test$rating,mu)
results <- tibble(method = "Using mean only", RMSE = baseline_rmse) 
message('RMSE:')
baseline_rmse
# tibble(): constructs a data frame

########################### 3.2.) Modeling movie effects

# Modeling movie effects
movie_averages <- edx_train %>% 
  group_by(movieId) %>%
  summarise(b_i = mean(rating - mu))

# Predictions
predictions_m <- edx_test %>%
  left_join(movie_averages, by = "movieId") %>%
  mutate(pred = mu + b_i)
# left_join(): the mutating joins add columns from y to x, includes all rows in x

# Results
movie_rmse <- RMSE(edx_test$rating,predictions_m$pred)
message('RMSE:')
movie_rmse
results <- bind_rows(results,
                     tibble(method = "Movie Effect Model", RMSE = movie_rmse))
# bind_rows(): bind multiple data frames by row

########################### 3.3.) Modeling movie and user effect

# Modeling movie and user effect
user_averages <- edx_train %>%
  left_join(movie_averages, by = "movieId") %>%
  group_by(userId) %>%
  summarise(b_u = mean(rating-mu-b_i))

# Predictions
predictions_m_u <- edx_test %>%
  left_join(movie_averages, by = "movieId") %>%
  left_join(user_averages, by = "userId") %>%
  mutate(pred = mu + b_i + b_u)

# Results
user_rmse <- RMSE(edx_test$rating,predictions_m_u$pred)
message('RMSE:')
user_rmse
results <- bind_rows(results,
                     tibble(method = "Movie and User Effect Model", RMSE = user_rmse))

########################### 3.4.) Regularization 

# Regularization 
lambda <- seq(0, 10, 0.25)
rmses <- sapply(lambda, function(l){
  
  mu <- mean(edx$rating)
  
  b_i <- edx_train %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- edx_train %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  # Predictions
  predicted_ratings <- edx_test %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  
  return(RMSE(edx_test$rating,predicted_ratings))
})

qplot(lambda, rmses) 
# qplot(): quick plot

lambda_min <- lambda[which.min(rmses)]
message('minimum of lambda:')
lambda_min
# which.min(): determines the index of the first minimum of a vector

# Regularized estimates of b_i
movie_averages_reg <- edx_train %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+lambda_min), n_i = n())

# Regularized estimates of b_u
user_averages_reg <- edx_train %>% 
  left_join(movie_averages_reg, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - mu - b_i)/(n()+lambda_min), n_u = n())

# Prediction
predicted_ratings_reg <- edx_test %>% 
  left_join(movie_averages_reg, by='movieId') %>%
  left_join(user_averages_reg, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>% 
  .$pred

# Test and save results
model_3_rmse <- RMSE(edx_test$rating,predicted_ratings_reg)
results <- bind_rows(results,
                     tibble(method="Regularized Movie and User Effect Model",  
                            RMSE = model_3_rmse ))
message('RMSE:')
model_3_rmse

########################### 4) Results

# Table of all results
results
