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

