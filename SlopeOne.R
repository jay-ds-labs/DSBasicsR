load('MovieLense.rda')


#' Build slope one model
#'
#' @param ratings \code{data.table} of ratings. Should contain 3 columns: user_id (id of user, character), item_id (id of item, character) and rating (rating of item by user, integer or numeric)
#'
#' @return a \code{data.table} of (item_id1, item_id2, b, support) where b represents the average rating difference of 'item 2 rating' - 'item 1 rating'. support represents number of ratings used to compute b
#'
#' @examples
#' set.seed(1)
#' in_train <- rep(TRUE, nrow(MovieLense))
#' in_train[sample(1:nrow(MovieLense), size = length(unique(MovieLense$user_id)) * 5)] <- FALSE
#'
#' MovieLense_train <- MovieLense[(in_train)]
#'
#' ratings <- normalize_ratings(MovieLense_train)
#' model <- build_slopeone(ratings$ratings)
#'
#' @import data.table plyr
#'
#' @export

build_slopeone <- function(ratings, ...) {
  if (NROW(ratings) == 0) {
    return(data.table(data.frame(item_id1=c(), item_id2=c(), b=c(), support=c())))
  }
  score_diff_per_user <- dlply(ratings, .(user_id), function(rows) {
    if (NROW(rows) > 1) {
      # Get diffs for all item_id pairs.
      pair_rows_nums <- subset(
        expand.grid(rows_num1=1:NROW(rows), rows_num2=1:NROW(rows)),
        rows_num1 != rows_num2 &
          rows[rows_num1, 'item_id'] != rows[rows_num2, 'item_id'])
      data.table(
        item_id1=rows[pair_rows_nums$rows_num1, 'item_id'],
        item_id2=rows[pair_rows_nums$rows_num2, 'item_id'],
        diff=rows[pair_rows_nums$rows_num2, 'rating']
        - rows[pair_rows_nums$rows_num1, 'rating'])
    }
  }, ...)
  # ddply is slow when merging data frames within list while rbindlist is
  # much faster.
  score_diff_per_user <- rbindlist(score_diff_per_user)
  if (NROW(score_diff_per_user) == 0) {
    return(data.table(data.frame(item_id1=c(), item_id2=c(), b=c(), support=c())))
  }
  score_diff_per_user$item_id1 <- as.character(score_diff_per_user$item_id1)
  score_diff_per_user$item_id2 <- as.character(score_diff_per_user$item_id2)
  # Compute average score diff between item 1 and item 2.
  model <- score_diff_per_user[,
                               list(b=mean(diff), support=NROW(diff)),
                               by='item_id1,item_id2']
  setkey(model, item_id1, item_id2)
  return(model)
}


#' Normalize rating table
#'
#' @param ratings \code{data.table} of ratings. Should contain 3 columns: user_id (id of user, character), item_id (id of item, character) and rating (rating of item by user, integer or numeric)
#'
#' @return List of 4 objects:
#' \itemize{
#'   \item global - global rating mean
#'   \item user - \code{data.table} of users' means (id, mean)
#'   \item item - \code{data.table} of items' means (id, mean)
#'   \item ratings - \code{data.table} of normalized ratings
#' }
#'
#' @details
#' Factor rating table into global mean + user mean + item mean. This is usually a preliminary step before building a model
#'
#' @examples
#' set.seed(1)
#' in_train <- rep(TRUE, nrow(MovieLense))
#' in_train[sample(1:nrow(MovieLense), size = length(unique(MovieLense$user_id)) * 5)] <- FALSE
#'
#' MovieLense_train <- MovieLense[(in_train)]
#'
#' ratings <- normalize_ratings(MovieLense_train)
#'
#' @import data.table
#'
#' @export

normalize_ratings <- function(ratings, ...) {
  result <- list()
  result$global <- ratings[, mean(rating)]
  result$user <- ratings[, list(mean_rating=mean(rating)), by='user_id']
  result$item <- ratings[, list(mean_rating=mean(rating)), by='item_id']
  
  ratings$rating <- ratings$rating - result$global
  setkey(result$user, user_id)
  ratings$rating <- ratings$rating - result$user[J(ratings$user_id), ]$mean_rating
  setkey(result$item, item_id)
  ratings$rating <- ratings$rating - result$item[J(ratings$item_id), ]$mean_rating
  result$ratings <- ratings
  return(result)
}


#' Predict ratings for multiple users and items given known ratings
#'
#' @param model \code{data.table} of produced by \code{\link{build_slopeone}}
#' @param target \code{data.table} of (user_id, item_id) to predict ratings
#' @param ratings \code{data.table} of known ratings. Should contain 3 columns: user_id (id of user, character), item_id (id of item, character) and rating (rating of item by user, integer or numeric)
#'
#' @return a \code{data.table} containig (user_id, item_id, predicted_rating)
#'
#' @examples
#' set.seed(1)
#' in_train <- rep(TRUE, nrow(MovieLense))
#' in_train[sample(1:nrow(MovieLense), size = length(unique(MovieLense$user_id)) * 5)] <- FALSE
#'
#' MovieLense_train <- MovieLense[(in_train)]
#'
#' ratings <- normalize_ratings(MovieLense_train)
#' model <- build_slopeone(ratings$ratings)
#'
#' predictions <- predict_slopeone(model, MovieLense[(!in_train), c(1, 2), with = FALSE], ratings$ratings)
#'
#' @import data.table
#'
#' @export

predict_slopeone <-function(model, targets, ratings, ...) {
  setkey(ratings, user_id)
  adply(targets,
        1,
        function(row) {
          data.frame(
            predicted_rating=predict_slopeone_for_user(
              model, row$item_id, ratings[J(row$user_id), ]))
        }, ...)
}


#' Predict score for target_item_id given the known ratings of a single user
#'
#' @param model \code{data.table} of produced by \code{\link{build_slopeone}}
#' @param target_item_id target item id to predict rating
#' @param ratings \code{data.table} of user's known ratings. Should contain 3 columns: user_id (id of user, character), item_id (id of item, character) and rating (rating of item by user, integer or numeric)
#'
#' @return predicted rating score
#'
#' @examples
#' set.seed(1)
#' in_train <- rep(TRUE, nrow(MovieLense))
#' in_train[sample(1:nrow(MovieLense), size = length(unique(MovieLense$user_id)) * 5)] <- FALSE
#'
#' MovieLense_train <- MovieLense[(in_train)]
#'
#' ratings <- normalize_ratings(MovieLense_train)
#' model <- build_slopeone(ratings$ratings)
#' predict_slopeone_for_user(model, "Volcano (1997)", ratings$ratings[user_id == "1", c(2, 3), with = FALSE])
#'
#' @import data.table
#'
#' @export

predict_slopeone_for_user <- function(model, target_item_id, ratings) {
  # If target_id is already rated by the user, return that rating.
  already_rated <- subset(ratings, ratings$item_id == target_item_id)
  if (NROW(already_rated) == 1) {
    return(already_rated$rating)
  } else if (NROW(already_rated) > 1) {
    warning(paste(target_item_id,
                  ' is already rated by user, but there are multiple ratings.'))
    return(already_rated[1, ]$rating)
  }
  if (NROW(model) == 0) {
    return(NA)
  }
  # Compute weighted average ratings.
  ratings <- rename(ratings, c('item_id'= "item_id1"))
  ratings <- cbind(ratings, item_id2=target_item_id)
  setkey(ratings, item_id1, item_id2)
  joined <- model[ratings, ]
  joined <- joined[complete.cases(joined), ]
  if (NROW(joined) == 0) {
    return(NA)
  }
  return(sum(joined[, (b + rating) * support]) /
           sum(joined[, sum(support)]))
}


#' Un-normalize rating table
#'
#' @param normalized normalization information generated by \code{\link{normalize_ratings}}
#' @param ratings \code{data.table} of ratings. Should contain 3 columns: userI_id (id of user, character), item_id (id of item, character) and predicted_rating (predicted rating of item by user, integer or numeric)
#'
#' @return a ratings \code{data.table} after un-normalization
#'
#' @details
#' Need to apply this after prediction is made using a model built from normalized ratings
#'
#' @examples
#' set.seed(1)
#' in_train <- rep(TRUE, nrow(MovieLense))
#' in_train[sample(1:nrow(MovieLense), size = length(unique(MovieLense$user_id)) * 5)] <- FALSE
#'
#' MovieLense_train <- MovieLense[(in_train)]
#'
#' ratings <- normalize_ratings(MovieLense_train)
#' model <- build_slopeone(ratings$ratings)
#'
#' predictions <- predict_slopeone(model, MovieLense[(!in_train), c(1, 2), with = FALSE], ratings$ratings)
#' real_ratings <- unnormalize_ratings(normalized = ratings, ratings = predictions)
#'
#' rmse <- sqrt(mean((real_ratings$predicted_rating - MovieLense[(!in_train)]$rating) ^ 2))
#'
#' @import data.table
#'
#' @export


unnormalize_ratings <- function(normalized, ratings) {
  ratings$predicted_rating <- ifelse(is.na(ratings$predicted_rating), 0,
                                     ratings$predicted_rating)
  ratings$predicted_rating <- ratings$predicted_rating + normalized$global
  setkey(normalized$user, user_id)
  user_mean <- normalized$user[J(ratings$user_id), ]$mean_rating
  ratings$predicted_rating <- ratings$predicted_rating +
    ifelse(!is.na(user_mean), user_mean, 0)
  setkey(normalized$item, item_id)
  item_mean <- normalized$item[J(ratings$item_id), ]$mean_rating
  ratings$predicted_rating <- ratings$predicted_rating +
    ifelse(!is.na(item_mean), item_mean, 0)
  return(ratings)
}
