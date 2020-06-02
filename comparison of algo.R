setwd('C:/Users/Tsomteachers/Documents/My Work/capstone')

library(recommenderlab)
library(recosystem)
library(data.table)
library(RColorBrewer)
library(ggplot2)
library(gridExtra)
library(plyr)
library(dplyr)
#library(rrecsys)
source('SVDApproximation.R')
source('SlopeOne.R')

options(digits=3)
options(scipen = 20)

head(ratings)
visualize_ratings(ratings_table = ratings)
ratings <- edxS
sparse_ratings <- sparseMatrix(i = ratings$user, j = ratings$item, x = ratings$rating,
                               dims = c(length(unique(ratings$user)), length(unique(ratings$item))),
                               dimnames = list(paste("u", 1:length(unique(ratings$user)), sep = ""),
                                               paste("m", 1:length(unique(ratings$item)), sep = "")))
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

######slope one#####

names(ratings) <- c("user_id", "item_id", "rating")
ratings <- data.table(ratings)

ratings[, user_id := as.character(user_id)]
ratings[, item_id := as.character(item_id)]

setkey(ratings, user_id, item_id)

set.seed(1)

in_train <- rep(TRUE, nrow(ratings))
in_train[sample(1:nrow(ratings), size = round(0.2 * length(unique(ratings$user_id)), 0) * 5)] <- FALSE

ratings_train <- ratings[(in_train)]
ratings_test <- ratings[(!in_train)]

ratings_train_norm <- normalize_ratings(ratings_train)
memory.limit()
memory.size()
memory.size(max = TRUE)
model <- build_slopeone(ratings_train_norm$ratings)



