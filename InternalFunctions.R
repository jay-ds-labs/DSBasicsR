###################################################################################
# PART 1 - Data Import, Audit & Cleaning
# 1. Importing 10Mn Dataset & Creating Train & Test
###################################################################################

data.load <- function(data.size){
  if(data.size == '10mn'){
    dl <- file.path(getwd(),'Movielens10Mn.zip')
    download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

    ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                     col.names = c("userId", "movieId", "rating", "timestamp"))
    
    movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
    colnames(movies) <- c("movieId", "title", "genres")
    movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                               title = as.character(title),
                                               genres = as.character(genres))
  }else if(data.size == '10k'){
    dl <- file.path(getwd(),'Movielens10Mn.zip')
    download.file("http://files.grouplens.org/datasets/movielens/ml-latest-small.zip", dl)
    
    ratings <- read.csv(unzip(dl, "ml-latest-small/ratings.csv"))
    
    movies <- read.csv(unzip(dl, "ml-latest-small/movies.csv"), stringsAsFactors = F)
  }else{
    print('Use input 10k for smaller dataset and 10mn for larger dataset')
    return()
  }
  
  movielens <- left_join(ratings, movies, by = "movieId")
  
  # Checking for R Version and using set.seed function appropriately
  if(as.numeric(R.Version()$major)==3 & as.numeric(R.Version()$minor) >5) set.seed(1, sample.kind="Rounding") else set.seed(1)
  
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
  
  return(list(edx=edx,validation=validation))
}

# Function for performing basic data audit after creating test & train datasets
data.audit <- function(ds){
  options(scipen = 20)
  invisible(readline(prompt="Increase the width of your console. Press [enter] to continue"))
  cat('Top 10 rows of the dataset -')
  pander(head(ds,10), style='simple', split.table = 160)
  cat(rep('=',65),'\n\n\n')
  invisible(readline(prompt="Press [enter] to continue"))
  cat('Dataset structure -\n')
  glimpse(ds)
  cat(rep('=',65),'\n\n\n')
  invisible(readline(prompt="Press [enter] to continue"))
  cat('Dataset columnwise summary -\n')
  describe(ds)
}


###################################################################################
# PART 1 - Data Import, Audit & Cleaning
# 3. Data cleaning- Getting all the genres in separate columns
###################################################################################

genre.detect <- function(ds){
  for(i in 1:nrow(genre.summary)){
    ds[,ncol(ds)+1] <- str_detect(ds$genres,genre.summary$value[i])
    names(ds)[ncol(ds)] <- genre.summary$value[i]
  }
  return(ds)
}



###################################################################################
# PART 3 - Data Preparation for Model Building
# 1. Creating train & test datasets from edx dataset for model parameter estimation
#    so that Validation set is used only for final model validation      
###################################################################################

create.test.and.train.from.edx <- function(ds){
  # Checking for R Version and using set.seed function appropriately
  if(as.numeric(R.Version()$major)==3 & as.numeric(R.Version()$minor) >5) set.seed(1, sample.kind="Rounding") else set.seed(1)
  
  test_index <- createDataPartition(y = ds$rating, times = 1, p = 0.1, list = FALSE)
  train_set <- ds[-test_index,]
  test_set <- ds[test_index,]
  
  test_set <- test_set %>% 
    semi_join(train_set, by = "movieId") %>%
    semi_join(train_set, by = "userId")
  
  removed <- anti_join(ds[test_index,], test_set)
  train_set <- rbind(train_set, removed)
  
  return(list(train_set=train_set,test_set=test_set))
}

# Function for calculating RMSE
rmse <- function(true_ratings, predicted_ratings){
  round(sqrt(mean((true_ratings - predicted_ratings)^2)),5)
}

# Function for viewing RMSE across models
view.rmse <- function(){
  pander(rmse.results, style='simple', split.table = 160)
}



###################################################################################
# PART 4 - Model Selection on 10K Movie Dataset
# Type 2 - Linear Models
# Model 6 - Movie & User Effect with Regularization
###################################################################################

model.regularization <- function(l,train,test){
  
  user.effect <- edxS %>% left_join(movie.effect,by='movieId') %>% 
    group_by(userId) %>% summarize(u_i = mean(rating - mean.rating- b_i))
  
  predicted.rating <- validationS %>% left_join(movie.effect,by='movieId') %>% 
    left_join(user.effect,by='userId') %>% 
    mutate(pred = mean.rating + b_i + u_i) %>% 
    pull(pred) 
  model.rmse <- rmse(validationS$rating,predicted.rating)
  
  mean.rating <- mean(train$rating) 
  
  movie.effect <- train %>% group_by(movieId) %>% summarize(b_i = sum(rating - mean.rating)/(n()+l[1]))
  
  user.effect <- train %>% left_join(movie.effect,by='movieId') %>% 
    group_by(userId) %>% summarize(u_i = sum(rating - mean.rating- b_i)/(n()+l[2]))
  temp <- train %>% left_join(movie.effect,by='movieId') %>% 
    left_join(user.effect, by='userId')
  
  predicted.rating <- test %>% left_join(movie.effect,by='movieId') %>% 
    left_join(user.effect,by='userId') %>%  
    mutate(pred = mean.rating + b_i + u_i) %>% 
    pull(pred)
  
  return(rmse(test$rating, predicted.rating))
}


###################################################################################
# PART 4 - Model Selection on 10K Movie Dataset
# Type 2 - Linear Models
# Model 7 - Movie, User & Genre Effect with Regularization
###################################################################################

model.regularization.with.genre <- function(l,train,test){
  
  mean.rating <- mean(train$rating) 
  
  movie.effect <- train %>% group_by(movieId) %>% summarize(b_i = sum(rating - mean.rating)/(n()+l[1]))
  
  user.effect.Drama <- train %>% left_join(movie.effect,by='movieId') %>% 
    group_by(userId,Drama) %>% summarize(u_i_Drama = sum(rating - mean.rating- b_i)/(n()+l[2]))
  temp <- train %>% left_join(movie.effect,by='movieId') %>% 
    left_join(user.effect.Drama,by=c('userId','Drama'))
  
  user.effect.Comedy <- temp %>% group_by(userId,Comedy) %>% 
    summarize(u_i_Comedy = sum(rating - mean.rating- b_i-u_i_Drama)/(n()+l[2]))
  temp <- temp %>% left_join(user.effect.Comedy,by=c('userId','Comedy'))
  
  user.effect.Action <- temp %>% group_by(userId,Action) %>% 
    summarize(u_i_Action = sum(rating - mean.rating- b_i-u_i_Drama-u_i_Comedy)/(n()+l[2]))
  temp <- temp %>% left_join(user.effect.Action,by=c('userId','Action'))
  
  user.effect.Thriller <- temp %>% group_by(userId,Thriller) %>% 
    summarize(u_i_Thriller = sum(rating - mean.rating- b_i-u_i_Drama-u_i_Comedy-u_i_Action)/(n()+l[2]))
  temp <- temp %>% left_join(user.effect.Thriller,by=c('userId','Thriller'))
  
  user.effect.Adventure <- temp %>% group_by(userId,Adventure) %>% 
    summarize(u_i_Adventure = sum(rating - mean.rating- b_i-u_i_Drama-u_i_Comedy-u_i_Action-u_i_Thriller)/(n()+l[2]))
  temp <- temp %>% left_join(user.effect.Adventure,by=c('userId','Adventure'))
  
  user.effect.Romance <- temp %>% group_by(userId,Romance) %>% 
    summarize(u_i_Romance = sum(rating - mean.rating- b_i-u_i_Drama-u_i_Comedy-u_i_Action-u_i_Thriller-u_i_Adventure)/(n()+l[2]))
  temp <- temp %>% left_join(user.effect.Romance,by=c('userId','Romance'))
  
  user.effect.OtherGenre <- temp %>% group_by(userId) %>% 
    summarize(u_i_OtherGenre = sum(rating - mean.rating- b_i-u_i_Drama-u_i_Comedy-u_i_Action-u_i_Thriller-u_i_Adventure-u_i_Romance)/(n()+l[2]))
  temp <- temp %>% left_join(user.effect.OtherGenre,by='userId')
  
  predicted.rating <- test %>% left_join(movie.effect,by='movieId') %>% 
    left_join(user.effect.Drama,by=c('userId','Drama')) %>% 
    left_join(user.effect.Comedy,by=c('userId','Comedy')) %>% 
    left_join(user.effect.Action,by=c('userId','Action')) %>% 
    left_join(user.effect.Thriller,by=c('userId','Thriller')) %>% 
    left_join(user.effect.Adventure,by=c('userId','Adventure')) %>% 
    left_join(user.effect.Romance,by=c('userId','Romance')) %>% 
    left_join(user.effect.OtherGenre,by='userId') %>% 
    mutate(pred = mean.rating + b_i + coalesce(u_i_Drama,0) + coalesce(u_i_Comedy,0) + coalesce(u_i_Action,0) + coalesce(u_i_Thriller,0) + coalesce(u_i_Adventure,0) + coalesce(u_i_Romance,0) + coalesce(u_i_OtherGenre,0)) %>% 
    pull(pred)
  
  return(rmse(test$rating, predicted.rating))
}


###################################################################################
# PART 4 - Model Selection on 10K Movie Dataset
# Type 3 - Memory-Based Collaborative Filtering
# Model 9 - User Based Collaborative Filtering
###################################################################################

convert.df.to.realRatingMatrix <- function(df){
  sm <- sparseMatrix(i = df$userId, j = df$movieId, x = df$rating,
                                 dims = c(length(unique(df$userId)), length(unique(df$movieId))),
                                 dimnames = list(paste("u", 1:length(unique(df$userId)), sep = ""),
                                                 paste("m", 1:length(unique(df$movieId)), sep = "")))
  return(new("realRatingMatrix", data = sm))
  
}
  
df <- edxS[,1:3]  
load('ratings.rda')
str(ratings)  
class(ratings)  
class(edxS)  
df <- as.data.table(edxS[,1:3])  
class(df)  
str(df)  
edxS[, sapply(.SD, function(x) 
  as.numeric(factor(x, levels=unique(x))))]
  

  
sparse_ratings <- sparseMatrix(i = ratings$user, j = ratings$item, x = as.numeric(ratings$rating),
                               dims = c(length(unique(ratings$user)), length(unique(ratings$item))),
                               dimnames = list(paste("u", 1:length(unique(ratings$user)), sep = ""),
                                               paste("m", 1:length(unique(ratings$item)), sep = "")))


u <- ratings$user
i <- ratings$item
r <- as.numeric(ratings$rating)

i <- edxS2$userId
u <- edxS2$movieId
r <- edxS2$rating

i
u
r

i <- as.numeric(factor(i, levels=unique(i)))
u <- as.numeric(factor(u, levels=unique(u)))
r <- as.numeric(factor(r, levels=unique(r)))

i <- ratings$user
u <- ratings$item
r <- as.numeric(ratings$rating)

u <- c(1,2,3,4,5,6,7,8,9,10)
i <- c(1,1,1,1,1,1,1,1,1,1)
r <- c(1,2,3,4,5,1,2,3,4,5)
sparse_ratings <- sparseMatrix(i = u, j = i, x = r,
                               dims = c(10,1),
                               dimnames = list(paste("u", 1:length(unique(u)), sep = ""),
                                               paste("m", 1:length(unique(i)), sep = ""))
                               )

sparse_ratings
edxS2 <- edxS[1:10,1:3]

i
u
r
