# 10 points: 0.86550 <= RMSE <= 0.89999
# 15 points: 0.86500 <= RMSE <= 0.86549
# 20 points: 0.86490 <= RMSE <= 0.86499
# 25 points: RMSE < 0.86490
################################
# Create edx set, validation set
################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

setwd('C:/Users/Tsomteachers/Documents/My Work/capstone')
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

save.image('capstone.RData')
movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")
save.image('capstone.RData')

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
save.image('capstone2.RData')

load('capstone2.RData')

names(edx)
table(edx$rating)
length(unique(edx$movieId))
length(unique(edx$userId))
genre.count <-  as.data.frame(table(edx$genres))
names(genre.count)[1] <- 'Genre'

genre.count %>% filter(str_detect(Genre,'Drama')) %>% summarize(count = sum(Freq))
genre.count %>% filter(str_detect(Genre,'Comedy')) %>% summarize(count = sum(Freq))
genre.count %>% filter(str_detect(Genre,'Thriller')) %>% summarize(count = sum(Freq))
genre.count %>% filter(str_detect(Genre,'Romance')) %>% summarize(count = sum(Freq))

movie.count <-  as.data.frame(table(edx$title))
movie.count$Var1[which.max(movie.count$Freq)]

#####################
movielens <- edx
movielens %>% 
  summarize(n_users = n_distinct(userId),
            n_movies = n_distinct(movieId))

library(caret)
set.seed(1, sample.kind='Rounding')
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
train_set <- movielens[-test_index,]
test_set <- movielens[test_index,]

test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

removed <- anti_join(movielens[test_index,], test_set)
train_set <- rbind(train_set, removed)

save.image('capstone3.RData')

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

mu_hat <- mean(train_set$rating)
mu_hat

naive_rmse <- RMSE(test_set$rating, mu_hat)
naive_rmse

predictions <- rep(3, nrow(test_set))
just3_rmse <-  RMSE(test_set$rating, predictions)

rmse_results <- tibble(method = c("Just 3 rating for all", "Just the average"), 
                       RMSE = c(just3_rmse, naive_rmse))

rmse_results

# Individual movie effect is considered
mu <- mean(train_set$rating) 
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

predicted_ratings <- mu + test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  pull(b_i)
movie_effect_rmse = RMSE(predicted_ratings, test_set$rating)
rmse_results <-  rbind(rmse_results, c('Movie effect considered',movie_effect_rmse))
rmse_results

# Individual user effect is considered alongwith movie effect
user_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)
movie_n_user_effect_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <-  rbind(rmse_results, c('Movie & user effect considered',movie_n_user_effect_rmse))
rmse_results

save.image('capstone4.RData')


# Choosing best lambda for regularization
lambdas <- seq(0, 10, 0.25)

rmses <- sapply(lambdas, function(l){
  
  mu <- mean(train_set$rating)
  
  b_i <- train_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  predicted_ratings <- 
    test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, test_set$rating))
})

qplot(lambdas, rmses)   
lambda <- lambdas[which.min(rmses)]
regularized_rmse = min(rmses)

rmse_results <-  rbind(rmse_results, c('Regularization effect considered',regularized_rmse))
rmse_results

# Choosing different lambda for movie & user
lambdas_m <- seq(2, 7, 0.25)
lambdas_u <- seq(2, 7, 0.25)
lambdas <- expand.grid(lambdas_m, lambdas_u)
save.image('capstone4.RData')
rm(edx,movielens, test_index)

rmses <- apply(lambdas, 1, function(l){
  
  mu <- mean(train_set$rating)
  
  b_i <- train_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l[1]))
  
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l[2]))
  
  predicted_ratings <- 
    test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)

  return(RMSE(predicted_ratings, test_set$rating))
})
lambdas[which.min(rmses),]
# 4.75    5
min(rmses)
#0.8641358

regularized_rmse = min(rmses)
rmse_results <-  rbind(rmse_results, c('Regularization effect considered',regularized_rmse))
rmse_results
####On the Validation set#################

set.seed(1234, sample.kind = "Rounding")

# Convert 'edx' and 'validation' sets to recosystem input format
edx_reco <-  with(edx, data_memory(user_index = userId, 
                                   item_index = movieId, 
                                   rating = rating))
validation_reco  <-  with(validation, data_memory(user_index = userId, 
                                                  item_index = movieId, 
                                                  rating = rating))

# Create the model object
r <-  recosystem::Reco()

# Tune the parameters
opts <-  r$tune(edx_reco, opts = list(dim = c(10, 20, 30), 
                                      lrate = c(0.1, 0.2),
                                      costp_l2 = c(0.01, 0.1), 
                                      costq_l2 = c(0.01, 0.1),
                                      nthread  = 4, niter = 10))

# Train the model
r$train(edx_reco, opts = c(opts$min, nthread = 4, niter = 20))

# Calculate the prediction
y_hat_final_reco <-  r$predict(validation_reco, out_memory())

# Update the result table
rmse_results <- cbind(rmse_results, RMSE(validation$rating, y_hat_final_reco))


#################### RecommenderLab #######
library("recommenderlab")
data("MovieLense")
if(!require('RCurl')) install.packages('RCurl')
library(RCurl)
library(tidyr)
library(dplyr)
library(knitr)
library(ggplot2)
if(!require('DT')) install.packages('DT')
library(DT)
if(!require('pander')) install.packages('pander')
library(pander)
if(!require('Matrix')) install.packages('Matrix')

movielense <- MovieLense
class(movielense)

# Loading the metadata that gets loaded with main dataset
moviemeta <- MovieLenseMeta

# Verifying records and variables
nrow(moviemeta)
ncol(moviemeta)
head(moviemeta)
pander(head(moviemeta), caption = "Sample Movie Meta Data")
View(moviemeta)

# Extracting data tha comprises of at least 20 ratings per user and 50 ratings
# per movie
movielenseorig <- movielense
movielense <- movielense[rowCounts(movielense) > 20, colCounts(movielense) > 50]
minrowcnt <- min(rowCounts(movielense))
nrow(movielense)
ncol(movielense)

set.seed(101, sample.kind = 'Rounding')
which_train <- sample(x = c(TRUE, FALSE), size = nrow(movielense), replace = TRUE, 
                      prob = c(0.8, 0.2))

recc_data_train <- movielense[which_train, ]
recc_data_test <- movielense[!which_train, ]

# Find top 10 recomm movies with Item based collab filter
recc_model1 <- Recommender(data = recc_data_train, method = "IBCF", parameter = list(k = 25, method = "Cosine"))
recc_model1

# Applying model to test
num_rec <- 10  # Lets recommend top 5 movies to each of users

recc_predicted1 <- predict(object = recc_model1, newdata = recc_data_test, n = num_rec,type = "ratings")
recc_predicted1
nrow(recc_predicted1)
ncol(recc_predicted1)
nrow(recc_data_test)
ncol(recc_data_test)
calcPredictionAccuracy(x = recc_predicted1, data = recc_data_test, 
                       byUser = FALSE)

# The recc_predicted object contains the recommendations which is topN
# recommendations for each of the users.The slots are: . items: This is the list
# with the indices of the recommended items for each user . itemLabels: This is
# the name of the items . n: This is the number of recommendations . ratings
# predicted

# We try to find the latest among those predicted for each user as most
# recommended.


recdf <- data.frame(user = sort(rep(1:length(recc_predicted1@items), recc_predicted1@n)), 
                    rating = unlist(recc_predicted1@ratings), index = unlist(recc_predicted1@items))

recdf$title <- recc_predicted1@itemLabels[recdf$index]
recdf$year <- moviemeta$year[recdf$index]
datatable(recdf[recdf$user %in% (1:10), ])


#######################################
install.packages('recommenderlab')
library(recommenderlab)
library(ggplot2)
library(data.table)
library(reshape2)

setwd('C:/Users/Tsomteachers/Documents/My Work/capstone/ml-latest-small/ml-latest-small')

movie_data <-  read.csv('movies.csv')
head(movie_data)
ratings_data <- read.csv('ratings.csv')
head(ratings_data)
ratings_data2 <- ratings_data %>% group_by(movieId) %>% filter(n()>=50) %>% ungroup() %>% group_by(userId) %>% filter(n()>=50) %>% ungroup()
ratings_data2 <- ratings_data %>% group_by(movieId) %>% mutate(selected_movie = n()>=50) %>% ungroup() %>% group_by(userId) %>% mutate(selected_user = n()>=50) %>% ungroup() %>% filter(selected_user==TRUE & selected_movie==TRUE)
avg_ratings_per_user <- ratings_data2 %>% group_by(userId) %>% summarize(avgRatings = mean(rating))
avg_ratings_per_user %>% ggplot(aes(x=avgRatings)) + geom_histogram()
# dont know from here
ratings_movies_norm <- normalize(ratings_movies)

eval_sets <- evaluationScheme(data = ratings_movies_norm,
                              method = "cross-validation",
                              k = 10,
                              given = 5,
                              goodRating = 0)

models_to_evaluate <- list(
  `IBCF Cosinus` = list(name = "IBCF", 
                        param = list(method = "cosine")),
  `IBCF Pearson` = list(name = "IBCF", 
                        param = list(method = "pearson")),
  `UBCF Cosinus` = list(name = "UBCF",
                        param = list(method = "cosine")),
  `UBCF Pearson` = list(name = "UBCF",
                        param = list(method = "pearson")),
  `Zufälliger Vorschlag` = list(name = "RANDOM", param=NULL)
)

n_recommendations <- c(1, 5, seq(10, 100, 10))

list_results <- evaluate(x = eval_sets, 
                         method = models_to_evaluate, 
                         n = n_recommendations)

vector_nn <- c(5, 10, 20, 30, 40)

models_to_evaluate <- lapply(vector_nn, function(nn){
  list(name = "UBCF",
       param = list(method = "pearson", nn = vector_nn))
})
names(models_to_evaluate) <- paste0("UBCF mit ", vector_nn, "Nutzern")
list_results <- evaluate(x = eval_sets, 
                         method = models_to_evaluate, 
                         n = n_recommendations)

####################
library("recommenderlab")
data("MovieLense")



## look at the first few ratings of the first user
head(as(MovieLense[1,], "list")[[1]])


## visualize part of the matrix
image(MovieLense[1:100,1:100])

## number of ratings per user
hist(rowCounts(MovieLense))

## number of ratings per movie
hist(colCounts(MovieLense))

## mean rating (averaged over users)
mean(rowMeans(MovieLense))

## available movie meta information
head(MovieLenseMeta)






MovieLense100 <- MovieLense[rowCounts(MovieLense) >100,]
MovieLense100

train <- MovieLense100[1:50]
rec <- Recommender(train, method = "UBCF")
rec

pre <- predict(rec, MovieLense100[101:102], n = 10)
pre

as(pre, "list")



#########################################
#### Kaggle in Class Problem. ....
####Reference: https://inclass.kaggle.com/c/predict-movie-ratings

# Set data path as per your data file (for example: "c://abc//" )
setwd("/home/ashokharnal/Documents/")

# If not installed, first install following three packages in R
library(recommenderlab)
library(reshape2)
library(ggplot2)
# Read training file along with header
tr<-read.csv("train_v2.csv",header=TRUE)
# Just look at first few lines of this file
head(tr)
# Remove 'id' column. We do not need it
tr<-tr[,-c(1)]
# Check, if removed
tr[tr$user==1,]
# Using acast to convert above data as follows:
#       m1  m2   m3   m4
# u1    3   4    2    5
# u2    1   6    5
# u3    4   4    2    5
g<-acast(tr, user ~ movie)
# Check the class of g
class(g)

# Convert it as a matrix
R<-as.matrix(g)

# Convert R into realRatingMatrix data structure
#   realRatingMatrix is a recommenderlab sparse-matrix like data-structure
r <- as(R, "realRatingMatrix")
r

# view r in other possible ways
as(r, "list")     # A list
as(r, "matrix")   # A sparse matrix

# I can turn it into data-frame
head(as(r, "data.frame"))

# normalize the rating matrix
r_m <- normalize(r)
r_m
as(r_m, "list")

# Draw an image plot of raw-ratings & normalized ratings
#  A column represents one specific movie and ratings by users
#   are shaded.
#   Note that some items are always rated 'black' by most users
#    while some items are not rated by many users
#     On the other hand a few users always give high ratings
#      as in some cases a series of black dots cut across items
image(r, main = "Raw Ratings")       
image(r_m, main = "Normalized Ratings")

# Can also turn the matrix into a 0-1 binary matrix
r_b <- binarize(r, minRating=1)
as(r_b, "matrix")

# Create a recommender object (model)
#   Run anyone of the following four code lines.
#     Do not run all four
#       They pertain to four different algorithms.
#        UBCF: User-based collaborative filtering
#        IBCF: Item-based collaborative filtering
#      Parameter 'method' decides similarity measure
#        Cosine or Jaccard
rec=Recommender(r[1:nrow(r)],method="UBCF", param=list(normalize = "Z-score",method="Cosine",nn=5, minRating=1))
rec=Recommender(r[1:nrow(r)],method="UBCF", param=list(normalize = "Z-score",method="Jaccard",nn=5, minRating=1))
rec=Recommender(r[1:nrow(r)],method="IBCF", param=list(normalize = "Z-score",method="Jaccard",minRating=1))
rec=Recommender(r[1:nrow(r)],method="POPULAR")

# Depending upon your selection, examine what you got
print(rec)
names(getModel(rec))
getModel(rec)$nn

############Create predictions#############################
# This prediction does not predict movie ratings for test.
#   But it fills up the user 'X' item matrix so that
#    for any userid and movieid, I can find predicted rating
#     dim(r) shows there are 6040 users (rows)
#      'type' parameter decides whether you want ratings or top-n items
#         get top-10 recommendations for a user, as:
#             predict(rec, r[1:nrow(r)], type="topNList", n=10)
recom <- predict(rec, r[1:nrow(r)], type="ratings")
recom

########## Examination of model & experimentation  #############
########## This section can be skipped #########################

# Convert prediction into list, user-wise
as(recom, "list")
# Study and Compare the following:
as(r, "matrix")     # Has lots of NAs. 'r' is the original matrix
as(recom, "matrix") # Is full of ratings. NAs disappear
as(recom, "matrix")[,1:10] # Show ratings for all users for items 1 to 10
as(recom, "matrix")[5,3]   # Rating for user 5 for item at index 3
as.integer(as(recom, "matrix")[5,3]) # Just get the integer value
as.integer(round(as(recom, "matrix")[6039,8])) # Just get the correct integer value
as.integer(round(as(recom, "matrix")[368,3717])) 

# Convert all your recommendations to list structure
rec_list<-as(recom,"list")
head(summary(rec_list))
# Access this list. User 2, item at index 2
rec_list[[2]][2]
# Convert to data frame all recommendations for user 1
u1<-as.data.frame(rec_list[[1]])
attributes(u1)
class(u1)
# Create a column by name of id in data frame u1 and populate it with row names
u1$id<-row.names(u1)
# Check movie ratings are in column 1 of u1
u1
# Now access movie ratings in column 1 for u1
u1[u1$id==3952,1]

########## Create submission File from model #######################
# Read test file
test<-read.csv("test_v2.csv",header=TRUE)
head(test)
# Get ratings list
rec_list<-as(recom,"list")
head(summary(rec_list))
ratings<-NULL
# For all lines in test file, one by one
for ( u in 1:length(test[,2]))
{
  # Read userid and movieid from columns 2 and 3 of test data
  userid <- test[u,2]
  movieid<-test[u,3]
  
  # Get as list & then convert to data frame all recommendations for user: userid
  u1<-as.data.frame(rec_list[[userid]])
  # Create a (second column) column-id in the data-frame u1 and populate it with row-names
  # Remember (or check) that rownames of u1 contain are by movie-ids
  # We use row.names() function
  u1$id<-row.names(u1)
  # Now access movie ratings in column 1 of u1
  x= u1[u1$id==movieid,1]
  # print(u)
  # print(length(x))
  # If no ratings were found, assign 0. You could also
  #   assign user-average
  if (length(x)==0)
  {
    ratings[u] <- 0
  }
  else
  {
    ratings[u] <-x
  }
  
}
length(ratings)
tx<-cbind(test[,1],round(ratings))
# Write to a csv file: submitfile.csv in your folder
write.table(tx,file="submitfile.csv",row.names=FALSE,col.names=FALSE,sep=',')
# Submit now this csv file to kaggle
########################################
