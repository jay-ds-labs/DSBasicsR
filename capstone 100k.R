# q(save="no")
# .rs.restartR()
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
# 1. Installing packages if required
###################################################################################

# Set working directory
# Please change this as per your choice
setwd(getwd())

# Required package list
required.packages.data.manipulation <- c('Hmisc','data.table','tidyverse','pander')
required.packages.visualization <- c('RColorBrewer','ggplot2','gridExtra')
required.packages.model <- c('caret','recommenderlab','recosystem')
required.packages <- c(required.packages.data.manipulation,
                       required.packages.visualization,
                       required.packages.model)

# Installing required packages if needed
packages.to.install <- required.packages[which(!required.packages %in% installed.packages()[,1])]
cat('Following packages will be installed:', packages.to.install)
if(length(packages.to.install)>0) install.packages(packages.to.install)
packages.to.install <- required.packages[which(!required.packages %in% installed.packages()[,1])]
if(length(packages.to.install)>0) cat('Failed to install:', packages.to.install) else print('All required packages are installed')


###################################################################################
# PART 0 - Setting up R environment
# 2. Loading required packages & functions in memory
###################################################################################

sapply(required.packages, require, character.only = TRUE)

# SlopeOne & SVDApproximation Packages loaded through source code as they generally have trouble installing
source('ExternalFunctions.R')

# Functions are created to make this code modular and easy to understand.
# Key tasks are done using functions kept in InternalFunctions.R file.
# All functions are required to run the code.
source('InternalFunctions.R')


# Might need - 
# rrecsys plyr  dplyr

###################################################################################
# PART 1 - Data Import, Audit & Cleaning
# 1. Importing 10Mn Dataset & Creating Train & Test
###################################################################################

# Note: this process could take a couple of minutes
train.and.test = data.load('10mn')

# Using suffix B in the names for Big (10Mn) and S for Small (10K) dataset size
edxB = train.and.test$edx
validationB = train.and.test$validation
rm(train.and.test)

# Performing basic audit on data loaded
data.audit(edxB)
data.audit(validationB)

###################################################################################
# PART 1 - Data Import, Audit & Cleaning
# 2. Importing 10K Dataset & Creating Train & Test
###################################################################################

train.and.test = data.load('10k')

# Using suffix B in the names for Big (10Mn) and S for Small (10K) dataset size
edxS = train.and.test$edx
validationS = train.and.test$validation
rm(train.and.test)

# Performing basic audit on data loaded
data.audit(edxS)
data.audit(validationS)


###################################################################################
# PART 1 - Data Import, Audit & Cleaning
# 3. Data cleaning- Getting all the genres in separate columns
###################################################################################

max.count.of.genres.in.a.movie <- max(str_count(unique(edxS$genres),'\\|'))+1
genre.colnames <- paste0('genre',c(1:max.count.of.genres.in.a.movie))
genre.summary <- edxS %>% separate(col = genres, into = genre.colnames,sep = '\\|', fill = 'right') %>% 
                select(genre.colnames) %>% gather() %>% group_by(value) %>% summarize(genre.count=n()) %>% 
                arrange(desc(genre.count)) %>% filter(value != 'NA') %>% 
                mutate(genre.perc=round(100*genre.count/nrow(edxS),0))

edxS <- genre.detect(edxS)
validationS <- genre.detect(validationS)

###################################################################################
# PART 3 - Data Preparation for Model Building
# 1. Creating train & test datasets from edx dataset for model parameter estimation
#    so that Validation set is used only for final model validation      
###################################################################################

# Creating train_set & test_set on edxB (larger dataset, used for final model train & validation using selected modelling technique)
test.and.train.big <- create.test.and.train.from.edx(edxB)
train.setB <- test.and.train.big$train_set                # will be used in PART 5
test.setB <- test.and.train.big$test_set                  # will be used in PART 5
rm(test.and.train.big)

# Creating train_set & test_set on edxS (smaller dataset, used for selecting modeling technique with lowest RMSE )
test.and.train.small <- create.test.and.train.from.edx(edxS)
train.setS <- test.and.train.small$train_set              # will be used in PART 4
test.setS <- test.and.train.small$test_set                # will be used in PART 4
rm(test.and.train.small)

###################################################################################
# PART 3 - Data Preparation for Model Building
# 2. Create a blank dataset called rmse.results to store the RMSE of all  modeling 
#    techniques applied on smaller dataset for final modeling technique selection
###################################################################################

rmse.results <- data.frame(SNo = integer(), ModelType = character(), Algorithm = character(), RMSE = double(), stringsAsFactors=F)


###################################################################################
# PART 4 - Model Selection on 10K Movie Dataset
# Type 1 - Baseline Model
# Model 1 - Random Rating with equal probability
###################################################################################

# 0.1 rating is allocated to each possible 10 ratings between 0.5 to 5
model.param.rating.prob <- rep(1/length(unique(edxS$rating)),10)
predicted.rating <- sample(seq(0.5,5,0.5), size = nrow(validationS), replace = T, prob = model.param.rating.prob)
model.rmse <- rmse(validationS$rating,predicted.rating)
rmse.results[nrow(rmse.results)+1,] <- c(1, 'Baseline model', 'Random rating with 0.1 prob', model.rmse)
view.rmse()


###################################################################################
# PART 4 - Model Selection on 10K Movie Dataset
# Type 1 - Baseline Model
# Model 2 - Random Rating with existing probability in training dataset
###################################################################################

# Existing prob of ratings in training dataset
model.param.rating.prob <- edxS %>% group_by(rating) %>% summarise(perc = n()/nrow(edxS)) %>% pull(perc)
predicted.rating <- sample(seq(0.5,5,0.5), size = nrow(validationS), replace = T, prob = model.param.rating.prob)
model.rmse <- rmse(validationS$rating,predicted.rating)
rmse.results[nrow(rmse.results)+1,] <- c(2, 'Baseline model', 'Random rating with existing prob', model.rmse)
view.rmse()


###################################################################################
# PART 4 - Model Selection on 10K Movie Dataset
# Type 1 - Baseline Model
# Model 3 - Mean Rating
###################################################################################

model.param.meanRating <- mean(edxS$rating)
predicted.rating <- rep(model.param.meanRating,nrow(validationS))
model.rmse <- rmse(validationS$rating,predicted.rating)
rmse.results[nrow(rmse.results)+1,] <- c(3, 'Baseline model', 'Mean rating', model.rmse)
view.rmse()


###################################################################################
# PART 4 - Model Selection on 10K Movie Dataset
# Type 2 - Linear Models
# Model 4 - Movie Effect
###################################################################################

# Individual movie effect is considered
mean.rating <- mean(edxS$rating) 
movie.effect <- edxS %>% group_by(movieId) %>% summarize(b_i = mean(rating - mean.rating))

predicted.rating <- validationS %>% left_join(movie.effect,by='movieId') %>% mutate(pred = mean.rating + b_i) %>% pull(pred) 
model.rmse <- rmse(validationS$rating,predicted.rating)
rmse.results[nrow(rmse.results)+1,] <- c(4, 'Linear model', 'Movie effect only', model.rmse)
view.rmse()


###################################################################################
# PART 4 - Model Selection on 10K Movie Dataset
# Type 2 - Linear Models
# Model 5 - Movie & User Effect
###################################################################################

# Individual user effect is considered
mean.rating <- mean(edxS$rating) 
user.effect <- edxS %>% left_join(movie.effect,by='movieId') %>% 
  group_by(userId) %>% summarize(u_i = mean(rating - mean.rating- b_i))

predicted.rating <- validationS %>% left_join(movie.effect,by='movieId') %>% 
  left_join(user.effect,by='userId') %>% 
  mutate(pred = mean.rating + b_i + u_i) %>% 
  pull(pred) 
model.rmse <- rmse(validationS$rating,predicted.rating)
rmse.results[nrow(rmse.results)+1,] <- c(5, 'Linear model', 'Movie & user effect', model.rmse)
view.rmse()


###################################################################################
# PART 4 - Model Selection on 10K Movie Dataset
# Type 2 - Linear Models
# Model 6 - Movie, User & Genre Effect
###################################################################################

# Individual user effect is considered within a genre
# Top 6 genres in terms of popularity is considered

mean.rating <- mean(edxS$rating) 

user.effect.Drama <- edxS %>% left_join(movie.effect,by='movieId') %>% 
  group_by(userId,Drama) %>% summarize(u_i_Drama = mean(rating - mean.rating- b_i))
edxS2 <- edxS %>% left_join(movie.effect,by='movieId') %>% 
  left_join(user.effect.Drama,by=c('userId','Drama'))

user.effect.Comedy <- edxS2 %>% group_by(userId,Comedy) %>% 
              summarize(u_i_Comedy = mean(rating - mean.rating- b_i-u_i_Drama))
edxS2 <- edxS2 %>% left_join(user.effect.Comedy,by=c('userId','Comedy'))

user.effect.Action <- edxS2 %>% group_by(userId,Action) %>% 
  summarize(u_i_Action = mean(rating - mean.rating- b_i-u_i_Drama-u_i_Comedy))
edxS2 <- edxS2 %>% left_join(user.effect.Action,by=c('userId','Action'))

user.effect.Thriller <- edxS2 %>% group_by(userId,Thriller) %>% 
  summarize(u_i_Thriller = mean(rating - mean.rating- b_i-u_i_Drama-u_i_Comedy-u_i_Action))
edxS2 <- edxS2 %>% left_join(user.effect.Thriller,by=c('userId','Thriller'))

user.effect.Adventure <- edxS2 %>% group_by(userId,Adventure) %>% 
  summarize(u_i_Adventure = mean(rating - mean.rating- b_i-u_i_Drama-u_i_Comedy-u_i_Action-u_i_Thriller))
edxS2 <- edxS2 %>% left_join(user.effect.Adventure,by=c('userId','Adventure'))

user.effect.Romance <- edxS2 %>% group_by(userId,Romance) %>% 
  summarize(u_i_Romance = mean(rating - mean.rating- b_i-u_i_Drama-u_i_Comedy-u_i_Action-u_i_Thriller-u_i_Adventure))
edxS2 <- edxS2 %>% left_join(user.effect.Romance,by=c('userId','Romance'))

user.effect.OtherGenre <- edxS2 %>% group_by(userId) %>% 
  summarize(u_i_OtherGenre = mean(rating - mean.rating- b_i-u_i_Drama-u_i_Comedy-u_i_Action-u_i_Thriller-u_i_Adventure-u_i_Romance))
edxS2 <- edxS2 %>% left_join(user.effect.OtherGenre,by='userId')

predicted.rating <- validationS %>% left_join(movie.effect,by='movieId') %>% 
  left_join(user.effect.Drama,by=c('userId','Drama')) %>% 
  left_join(user.effect.Comedy,by=c('userId','Comedy')) %>% 
  left_join(user.effect.Action,by=c('userId','Action')) %>% 
  left_join(user.effect.Thriller,by=c('userId','Thriller')) %>% 
  left_join(user.effect.Adventure,by=c('userId','Adventure')) %>% 
  left_join(user.effect.Romance,by=c('userId','Romance')) %>% 
  left_join(user.effect.OtherGenre,by='userId') %>% 
  mutate(pred = mean.rating + b_i + coalesce(u_i_Drama,0) + coalesce(u_i_Comedy,0) + coalesce(u_i_Action,0) + coalesce(u_i_Thriller,0) + coalesce(u_i_Adventure,0) + coalesce(u_i_Romance,0) + coalesce(u_i_OtherGenre,0)) %>% 
  pull(pred)

model.rmse <- rmse(validationS$rating,predicted.rating)
rmse.results[nrow(rmse.results)+1,] <- c(6, 'Linear model', 'Movie, user & genre effect', model.rmse)
view.rmse()


###################################################################################
# PART 4 - Model Selection on 10K Movie Dataset
# Type 2 - Linear Models
# Model 7 - Movie & User Effect with Regularization
###################################################################################

# Choosing different lambda for movie & user
lambdas_m <- seq(2, 10, 0.25)
lambdas_u <- seq(2, 10, 0.25)
lambdas <- expand.grid(lambdas_m, lambdas_u)

# selecting best combination of lambdas. 
# Using train.setS & test.setS to estimate the lambdas.
# Note - this will take time to run.
rmses <- apply(lambdas, 1, model.regularization, train=train.setS, test=test.setS)
cat('Best model parameters \nMovie Lambda - ', lambdas[which.min(rmses),1],
    '\nUser Lambda - ', lambdas[which.min(rmses),2], '\nMin RMSE: ', min(rmses))

# Best model parameters 
# Movie Lambda -  3 
# User Lambda -  2 
# Min RMSE:  0.8476

model.rmse <- model.regularization(c(lambdas[which.min(rmses),1],lambdas[which.min(rmses),2]), edxS, validationS)
rmse.results[nrow(rmse.results)+1,] <- c(6, 'Linear model', 'Movie & user effect with regularization', model.rmse)
view.rmse()

###################################################################################
# PART 4 - Model Selection on 10K Movie Dataset
# Type 2 - Linear Models
# Model 8 - Movie, User & Genre Effect with Regularization
###################################################################################

# Choosing different lambda for movie & user
lambdas_m <- seq(2, 10, 0.25)
lambdas_u <- seq(2, 10, 0.25)
lambdas <- expand.grid(lambdas_m, lambdas_u)

# selecting best combination of lambdas. 
# Using train.setS & test.setS to estimate the lambdas.
# Note - this will take time to run.
rmses <- apply(lambdas, 1, model.regularization.with.genre, train=train.setS, test=test.setS)
cat('Best model parameters \nMovie Lambda - ', lambdas[which.min(rmses),1],
      '\nUser Lambda - ', lambdas[which.min(rmses),2], '\nMin RMSE: ', min(rmses))

# Best model parameters 
# Movie Lambda -  3 
# User Lambda -  10 
# Min RMSE:  0.83289

# Looks like there is further scope to improve RMSE, as user Lambda is at the boundary
# Makes sense to increase the range of user lambda 10 onwards, while keeping movie lambda 
# fixed at 3, to reduce the number of combinations
lambdas_m <- lambdas[which.min(rmses),1]
lambdas_u <- seq(10, 30, 0.25)
lambdas <- expand.grid(lambdas_m, lambdas_u)
rmses <- apply(lambdas, 1, model.regularization.with.genre, train=train.setS, test=test.setS)
cat('Best model parameters \nMovie Lambda - ', lambdas[which.min(rmses),1],
    '\nUser Lambda - ', lambdas[which.min(rmses),2], '\nMin RMSE: ', min(rmses))

tibble(lambdas_u=lambdas_u, rmses=rmses) %>% ggplot(aes(x=lambdas_u, y=rmses)) +
                                              geom_point()+ggtitle('Movie lambda fixed at 3')
# Best model parameters 
# Movie Lambda -  3 
# User Lambda -  19.25 
# Min RMSE:  0.83239

# final check with user lambda now fixed at 19.5 and moview lambdas changing
lambdas_m <- seq(2, 10, 0.25)
lambdas_u <- lambdas[which.min(rmses),2]
lambdas <- expand.grid(lambdas_m, lambdas_u)
rmses <- apply(lambdas, 1, model.regularization.with.genre, train=train.setS, test=test.setS)
cat('Best model parameters \nMovie Lambda - ', lambdas[which.min(rmses),1],
    '\nUser Lambda - ', lambdas[which.min(rmses),2], '\nMin RMSE: ', min(rmses))

tibble(lambdas_m=lambdas_m, rmses=rmses) %>% ggplot(aes(x=lambdas_m, y=rmses)) +
  geom_point()+ggtitle('User lambda fixed at 19.25')

# Best model parameters 
# Movie Lambda -  3 
# User Lambda -  19.25 
# Min RMSE:  0.83239

# Using these model parameters (movie & user lambda) on validation dataset
model.rmse = model.regularization.with.genre(l = c(lambdas[which.min(rmses),1], lambdas[which.min(rmses),2]), train = edxS, test = validationS)
rmse.results[nrow(rmse.results)+1,] <- c(7, 'Linear model', 'Movie, user & genre effect with regularization', model.rmse)
view.rmse()


###################################################################################
# PART 4 - Model Selection on 10K Movie Dataset
# Type 3 - Memory-Based Collaborative Filtering
# Model 9 - User Based Collaborative Filtering
###################################################################################

# recommenderlab package will be used for this
# Training dataset needs to be in realRatingMatrix format
# At first training data is converted into sparse matrix & then sparse matrix
# is converted into realRatingMatrix
edxS.realRatingMatrix <- convert.df.to.realRatingMatrix(edxS)
sparse_ratings[1:10, 1:10]

real_ratings <- new("realRatingMatrix", data = sparse_ratings)
real_ratings
as(real_ratings,"matrix")[1:5,1:5]

model <- Recommender(real_ratings, method = "POPULAR", param=list(normalize = "center"))
prediction <- predict(model, real_ratings[1:5], type="ratings")
as(prediction, "matrix")[,1:5]

set.seed(1, sample.kind = 'Rounding')
e <- evaluationScheme(real_ratings, method="split", train=0.8, given=-5)
#5 ratings of 20% of users are excluded for testing

model <- Recommender(getData(e, "train"), "POPULAR")
prediction <- predict(model, getData(e, "known"), type="ratings")

rmse_popular <- calcPredictionAccuracy(prediction, getData(e, "unknown"))[1]
rmse_popular

as(getData(e, "train"),"matrix")[1:5,1:5]
as(getData(e, "known"),"matrix")[1:5,1:5]
as(prediction,"matrix")[1:5,1:5]
as(getData(e, "unknown"),"matrix")[1:5,1:5]

model <- Recommender(real_ratings, method = "UBCF",
                     param=list(normalize = "center", method="Cosine", nn=50))
prediction <- predict(model, real_ratings[1:5], type="ratings")
as(prediction, "matrix")[,1:5]

set.seed(1)

model <- Recommender(getData(e, "train"), method = "UBCF",
                     param=list(normalize = "center", method="Cosine", nn=50))

prediction <- predict(model, getData(e, "known"), type="ratings")

rmse_ubcf <- calcPredictionAccuracy(prediction, getData(e, "unknown"))[1]
rmse_ubcf


model <- Recommender(real_ratings, method = "IBCF",
                     param=list(normalize = "center", method="Cosine", k=350))
prediction <- predict(model, real_ratings[1:5], type="ratings")
as(prediction, "matrix")[,1:8]

set.seed(1)

model <- Recommender(getData(e, "train"), method = "IBCF",
                     param=list(normalize = "center", method="Cosine", k=350))

prediction <- predict(model, getData(e, "known"), type="ratings")

rmse_ubcf <- calcPredictionAccuracy(prediction, getData(e, "unknown"))[1]
rmse_ubcf







# 6 Types of models to be built
# Type 1 - Random Prediction
#   Random Rating - Same prob 
#   Random Rating - Existing prob
#   Mean Rating
# Type 2 - Linear Models
#   Movie Effect
#   Movie & User Effect
#   Movie, User & Genre Effect
#   Movie & User Effect with Regularization
#   Movie, User & Genre Effect with Regularization
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















