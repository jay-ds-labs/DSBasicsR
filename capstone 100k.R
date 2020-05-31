###################################################################################
# PART 0 - Setting up R environment
# PART 1 - Data Import, Audit & Cleaning
# PART 2 - Data Exploration
# PART 3 - Data Preparation for Model Building
# PART 4 - Model Selection on 10K Movie Dataset
# PART 5 - Model Training & Validation on 1Mn Movie Dataset using selected model
###################################################################################

###################################################################################
# PART 0 - Setting up R environment
# 1. Installing/updating packages if required
###################################################################################
required.packages 

installed.packages()[,1]
c(1,2,3) %in% c(10,4,1,30,3,20)
################################
# Create edx set, validation set
################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-latest-small.zip

setwd('C:/Users/SABHA/Documents/capstone')
dl <- file.path(getwd(),'Movielens100k.zip')
download.file("http://files.grouplens.org/datasets/movielens/ml-latest-small.zip", dl)
ratings <- read.csv(unzip(dl, "ml-latest-small/ratings.csv"))
head(ratings)
movies <- read.csv(unzip(dl, "ml-latest-small/movies.csv"), stringsAsFactors = F)
head(movies)
str(ratings)
str(movies)

movielens <- left_join(ratings, movies, by = "movieId")
View(movielens)

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
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

rm(dl, ratings, movies, test_index, temp, movielens, removed)
save.image('Capstone100k_1.RData')


#0 Load Data 
load('Capstone100k_1.RData')

# 6 Types of models to be built
# Type 1 - Random Prediction
#   Mean Rating
#   Random Rating - Same prob 
#   Random Rating - Existing prob
# Type 2 - Linear Models
#   Movie Effect
#   Movie & User Effect
#   Movie, User & Genre Effect
#   Movie, User, Genre & Time Effect
#   Movie Effect with Regularization
#   Movie & User Effect with Regularization
#   Movie, User & Genre Effect with Regularization
#   Movie, User, Genre & Time Effect with Regularization
# Type 3 - Memory-Based Collaborative Filtering
#   User Based Collaborative Filtering
#   Item Based Collaborative Filtering
# Type 4 - Model-Based Collaborative Filtering
#   Slope One
#   SVD
#   SVD with Approximation
#   Matrix Factorization with Stochastic Gradient Descent
# Type 5 - Ensemble Methods
# Type 6 - Deep Learning


###################################################################################
# Get ready for model building
# Model 1 - Mean Rating
###################################################################################

###################################################################################
# Model Building on 100K data
# Part 1 - Type 1 - Random Prediction
# Model 1 - Mean Rating
###################################################################################













