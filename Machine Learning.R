#install.packages('tidyverse')
library(tidyverse)
#install.packages('caret')
library(caret)
#install.packages('dslabs')
library(dslabs)
data(heights)

# define the outcome and predictors
y <- heights$sex
x <- heights$height

# generate training and test sets
set.seed(2007)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]

# guess the outcome
y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE)
y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE) %>% 
  factor(levels = levels(test_set$sex))

# compute accuracy
mean(y_hat == test_set$sex)
heights %>% group_by(sex) %>% summarize(mean(height), sd(height))
y_hat <- ifelse(x > 62, "Male", "Female") %>% factor(levels = levels(test_set$sex))
mean(y == y_hat)

# examine the accuracy of 10 cutoffs
cutoff <- seq(61, 70)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% 
    factor(levels = levels(test_set$sex))
  mean(y_hat == train_set$sex)
})
max(accuracy)
best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff
y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))
y_hat <- factor(y_hat)
mean(y_hat == test_set$sex)


# Labs
library(dslabs)
library(dplyr)
library(lubridate)
data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

dat %>% mutate(f = ifelse(sex == 'Female',1,0)) %>% group_by(type) %>% summarize(mean(f))
y_hat = ifelse(x=='inclass','Female', 'Male')
mean(y == y_hat)

install.packages('caret')
library(caret)
?sensitivity

table(y_hat,y)
26/(26+42)
69/(13+69)
(26+42)/150

# Labs
library(caret)
library(tidyverse)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

set.seed(2, sample.kind="Rounding")
test_index = createDataPartition(y,times = 1, p =0.5, list = F)
test <- iris[test_index,]
train <- iris[-test_index,]
y_train = ifelse(train$Species=='virginica', 1, 0)
fn.best.cutoff = function(x_name){
  x = train %>% pull(x_name)
  acc = numeric()
  for(i in x){
    y_hat = ifelse(x>i, 1,0)
    acc = c(acc,mean(y_train==y_hat))
  }
  x.cutoff = x[min(which(acc==max(acc)))]
  print(paste('varname =',x_name, 'cutoff=',x.cutoff,'accuracy',max(acc)))
}
names(iris)
for(i in 1:4) fn.best.cutoff(names(iris)[i])

x = test %>% pull(names(iris)[3])
y_test = ifelse(test$Species=='virginica', 1, 0)
y_hat = ifelse(x>4.7, 1,0)
mean(y_test==y_hat)

x_p_length = test %>% pull(names(iris)[3])
x_p_width = test %>% pull(names(iris)[4])
y_test = ifelse(test$Species=='virginica', 1, 0)
y_hat = ifelse(x_p_length>4.7 | x_p_width > 1.5, 1,0)
mean(y_test==y_hat)

# Labs
# P(d | t+)= p(t+ | d) * p(d) /p(t+)
# 0.85 * 0.02 / p(t+)
# p(t-|healthy) = 


# Labs
set.seed(1, sample.kind="Rounding")
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))
mean(test)
sum(test==0 & disease==1)/sum(test==0)
(sum(test==1 & disease==1)/sum(test==1))/(sum(disease==1)/length(disease))

# Labs






