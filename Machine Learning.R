# KEY UNDERSTANDINGS
# EVALUATION METRICS
# accuracy is not the best metric, as we can have high accuracy even with lower
# sensitivity or specificity. It depends on prevelance. 
# So if the prevelance of 1 is high (i.e. %age of 1's high) 
# and you have high sensitivity (% of actual 1's predicted), then even with low specificity (i.e. your ability to predict correctly 0's out of all 0's)
# you will end up having high accuracy.
# Hence a better metric is avg of sensitivity & specificity (we have to take harmonic avg as they are rates)
# or the other metric is F1. It is the harmonic avg of recall (or sensitivity or TPR - true positive rate) & precision (or % of actual 1's out of predicted 1's)
# in F1 you can even add weight of 
# F1 = 1/[1/2* (1/recall + 1/precision)] = 2 * precision * recall / (precision + recall)
# precision is used instead of specificity as it considers prevalance
# to add weight in F1, we use B - how much more important is sensitivity to specificity
# F1 = 1/ [ ( B^2/(1 + B^2) * 1/RECALL) + (1/(1+B^2) * 1/PRECISION)]

# Another aspect is you can use confusion matrix OR bayes theorem to find an unknown form the matrix

# ROC curve is between TPR & FPR. I.E. SENSITIVITY & 1-SPECIFICITY
# The problem of ROC curve is that it doesnt take into account prevelance
# so if you predict for 0 or you predict for 1, you will get a similar curve
# Precision vs Recall takes into account prevalance
# so the curve will show higher precision if you are predicting 1 and 1 has high prevelance
# while the curve will show lower precision in the above case, if you are predicting 0

# SMOOTHING
# It is like curve fitting or low pass filter
# It is used to detect the trend by removing the noise
# This should be done on the dependant var that we are trying to predict
# it basically takes into account nearby data points and considers an avg behavior
# There is a bin smoothing technique which uses ksmooth function and weighted avg techniques
# Weights basically give more importance to nearby points and less to far away
# Kernels are used to define weight function. in ksmooth there are 2 kernels - 
# box & normal. They are called with these names as the shape of the function weights =f(x) when plotted will have these shapes.
span <- 7
fit <- with(polls_2008, 
            ksmooth(day, margin, kernel = "normal", bandwidth = span))
# The better smoothing function is Loess smoothing technique, as it uses a 
# linear regression of weights. If we use larger samples it will result in smoother trend
# In bin smoothing the bin size is same. For e.g. if the size is 7 than even if the actual data points in a particular interval of x:x+6 is only 4 points than only those will be considered for avg
# In loess the number of points are considered same. So of the span is 21, than for every datapoint, 10 points before and 10 point after and the point itself is considered for a linear regression and then the predicted value at that point is considered the new value. 
# if we use the argument family = symmetric in loess than it means that the an iterative proces is taken for each estimation, in the first iteration outliers are determined and then in the next iteration their weights are downgraded
# Loess technique is based on Taylor's theorem which says that if you look at 
# any function closely enough it looks like a parabola. The default version of 
# loess technique is with degree = 2. But it doesnt smooth so much
# So we use degree = 1, which means that a line is fitted resultig in a smoother fit.

# ggplot uses geom_smooth which uses loess technique with default paramters which are generally not a good fit 

# remember that all models try to minimize the loss function which is basically MSE









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
library(dslabs)
data("heights")
heights %>% 
  mutate(height = round(height)) %>%
  group_by(height) %>%
  summarize(p = mean(sex == "Male")) %>%qplot(height, p, data =.)


ps <- seq(0, 1, 0.1)
heights %>% 
  mutate(g = cut(height, quantile(height, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(p = mean(sex == "Male"), height = mean(height)) %>%
  qplot(height, p, data =.)


#install.packages('MASS')
library('MASS')
Sigma <- 9*matrix(c(1,0.5,0.5,1), 2, 2)
dat <- MASS::mvrnorm(n = 10000, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

head(dat)

ps <- seq(0, 1, 0.1)
dat %>% 
  mutate(g = cut(x, quantile(x, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(y = mean(y), x = mean(x)) %>%
  qplot(x, y, data =.)


# Case study: is it a 2 or a 7?
library(tidyverse)
library(dslabs)
data("mnist_27")
class(mnist_27)
names(mnist_27)
class(mnist_27$train)
View(mnist_27$train)

mnist_27$train %>% ggplot(aes(x_1, x_2, color = y)) + geom_point()
fit <- mnist_27$train %>%
  mutate(y = ifelse(y==7, 1, 0)) %>%
  lm(y ~ x_1 + x_2, data = .)

library(caret)
p_hat <- predict(fit, newdata = mnist_27$test)
y_hat <- factor(ifelse(p_hat > 0.5, 7, 2))
confusionMatrix(y_hat, mnist_27$test$y)$overall[["Accuracy"]]
#install.packages('e1071')
#library(e1071)

View(mnist_27$true_p)
mnist_27$true_p %>% ggplot(aes(x_1, x_2, z = p, fill = p)) +
  geom_raster() +
  scale_fill_gradientn(colors=c("#F8766D", "white", "#00BFC4")) +
  stat_contour(breaks=c(0.5), color="black")

# Linear Regression
#install.packages('HistData')
library(HistData)

set.seed(1983, sample.kind = 'Rounding')
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

head(galton_heights)

y <- galton_heights$son
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

train_set <- galton_heights %>% slice(-test_index)
test_set <- galton_heights %>% slice(test_index)

m <- mean(train_set$son)
m
mean((m - test_set$son)^2)
fit <- lm(son ~ father, data = train_set)
fit$coef
y_hat <- fit$coef[1] + fit$coef[2]*test_set$father
mean((y_hat - test_set$son)^2)
y_hat <- predict(fit, test_set)
mean((y_hat - test_set$son)^2)

# Labs
set.seed(1, sample.kind="Rounding")
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

y = dat$y
set.seed(1, sample.kind="Rounding")
lm_func = function(Y){
  test_index = createDataPartition(Y,times = 1, p=0.5,list = F )
  train_set = dat %>% slice(-test_index)
  test_set = dat %>% slice(test_index)
  fit = lm(y ~ x, data = train_set)
  y_hat = predict(fit, test_set)
  rmse = sqrt(mean((y_hat- test_set$y)^2))
  return(rmse)
}

result = replicate(100,lm_func(y))
mean(result)
sd(result)


lm_func = function(D){
  test_index = createDataPartition(D$y,times = 1, p=0.5,list = F )
  train_set = D %>% slice(-test_index)
  test_set = D %>% slice(test_index)
  fit = lm(y ~ x, data = train_set)
  y_hat = predict(fit, test_set)
  rmse = sqrt(mean((y_hat- test_set$y)^2))
  return(rmse)
}

func = function(N){
  dat <- MASS::mvrnorm(n = N, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  result = replicate(100,lm_func(dat))
  m = round(mean(result),3)
  s = round(sd(result),3)
  return(c(m,s))
}

set.seed(1, sample.kind="Rounding")
sapply(c(100, 500, 1000, 5000, 10000), func)




set.seed(1, sample.kind="Rounding")
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

y = dat$y
set.seed(1, sample.kind="Rounding")
lm_func = function(Y){
  test_index = createDataPartition(Y,times = 1, p=0.5,list = F )
  train_set = dat %>% slice(-test_index)
  test_set = dat %>% slice(test_index)
  fit = lm(y ~ x, data = train_set)
  y_hat = predict(fit, test_set)
  rmse = sqrt(mean((y_hat- test_set$y)^2))
  return(rmse)
}

result = replicate(100,lm_func(y))
round(mean(result),3)
round(sd(result),3)



set.seed(1, sample.kind="Rounding")
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

set.seed(1, sample.kind="Rounding")
test_index = createDataPartition(dat$y,times = 1, p=0.5,list = F )
train_set = dat %>% slice(-test_index)
test_set = dat %>% slice(test_index)

fit = lm(y ~ x_1, data = train_set)
y_hat = predict(fit, test_set)
rmse_x_1 = sqrt(mean((y_hat- test_set$y)^2))

fit = lm(y ~ x_2, data = train_set)
y_hat = predict(fit, test_set)
rmse_x_2 = sqrt(mean((y_hat- test_set$y)^2))

fit = lm(y ~ x_1 + x_2, data = train_set)
y_hat = predict(fit, test_set)
rmse_x_1_2 = sqrt(mean((y_hat- test_set$y)^2))

rmse_x_1
rmse_x_2
rmse_x_1_2



set.seed(1, sample.kind="Rounding")
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

set.seed(1, sample.kind="Rounding")
test_index = createDataPartition(dat$y,times = 1, p=0.5,list = F )
train_set = dat %>% slice(-test_index)
test_set = dat %>% slice(test_index)

fit = lm(y ~ x_1, data = train_set)
y_hat = predict(fit, test_set)
rmse_x_1 = sqrt(mean((y_hat- test_set$y)^2))

fit = lm(y ~ x_2, data = train_set)
y_hat = predict(fit, test_set)
rmse_x_2 = sqrt(mean((y_hat- test_set$y)^2))

fit = lm(y ~ x_1 + x_2, data = train_set)
y_hat = predict(fit, test_set)
rmse_x_1_2 = sqrt(mean((y_hat- test_set$y)^2))

rmse_x_1
rmse_x_2
rmse_x_1_2


# Logistic Regression
library(dslabs)
data("heights")
y <- heights$height

set.seed(2, sample.kind = "Rounding") #if you are using R 3.6 or later

test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- heights %>% slice(-test_index)
test_set <- heights %>% slice(test_index)

train_set %>% 
  filter(round(height)==66) %>%
  summarize(y_hat = mean(sex=="Female"))

heights %>% 
  mutate(x = round(height)) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarize(prop = mean(sex == "Female")) %>%
  ggplot(aes(x, prop)) +
  geom_point()
lm_fit <- mutate(train_set, y = as.numeric(sex == "Female")) %>% lm(y ~ height, data = .)
p_hat <- predict(lm_fit, test_set)
y_hat <- ifelse(p_hat > 0.5, "Female", "Male") %>% factor()
confusionMatrix(y_hat, test_set$sex)$overall["Accuracy"]  # 0.785

heights %>% 
  mutate(x = round(height)) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarize(prop = mean(sex == "Female")) %>%
  ggplot(aes(x, prop)) +
  geom_point() + 
  geom_abline(intercept = lm_fit$coef[1], slope = lm_fit$coef[2])
range(p_hat)

# fit logistic regression model
glm_fit <- train_set %>% 
  mutate(y = as.numeric(sex == "Female")) %>%
  glm(y ~ height, data=., family = "binomial")
p_hat_logit <- predict(glm_fit, newdata = test_set, type = "response")
y_hat_logit <- ifelse(p_hat_logit > 0.5, "Female", "Male") %>% factor
confusionMatrix(y_hat_logit, test_set$sex)$overall[["Accuracy"]] # 0.798

# CASE STUDY - 2 OR 7
mnist <- read_mnist()
is <- mnist_27$index_train[c(which.min(mnist_27$train$x_1), which.max(mnist_27$train$x_1))]
titles <- c("smallest","largest")
tmp <- lapply(1:2, function(i){
  expand.grid(Row=1:28, Column=1:28) %>%
    mutate(label=titles[i],
           value = mnist$train$images[is[i],])
})
tmp <- Reduce(rbind, tmp)
tmp %>% ggplot(aes(Row, Column, fill=value)) +
  geom_raster() +
  scale_y_reverse() +
  scale_fill_gradient(low="white", high="black") +
  facet_grid(.~label) +
  geom_vline(xintercept = 14.5) +
  geom_hline(yintercept = 14.5)

rm(mnist)

data("mnist_27")
mnist_27$train %>% ggplot(aes(x_1, x_2, color = y)) + geom_point()

is <- mnist_27$index_train[c(which.min(mnist_27$train$x_2), which.max(mnist_27$train$x_2))]
titles <- c("smallest","largest")
tmp <- lapply(1:2, function(i){
  expand.grid(Row=1:28, Column=1:28) %>%
    mutate(label=titles[i],
           value = mnist$train$images[is[i],])
})
tmp <- Reduce(rbind, tmp)
tmp %>% ggplot(aes(Row, Column, fill=value)) +
  geom_raster() +
  scale_y_reverse() +
  scale_fill_gradient(low="white", high="black") +
  facet_grid(.~label) +
  geom_vline(xintercept = 14.5) +
  geom_hline(yintercept = 14.5)

rm(mnist)

fit_glm <- glm(y ~ x_1 + x_2, data=mnist_27$train, family = "binomial")
p_hat_glm <- predict(fit_glm, mnist_27$test)
y_hat_glm <- factor(ifelse(p_hat_glm > 0.5, 7, 2))
confusionMatrix(data = y_hat_glm, reference = mnist_27$test$y)$overall["Accuracy"]

mnist_27$true_p %>% ggplot(aes(x_1, x_2, fill=p)) +
  geom_raster()

mnist_27$true_p %>% ggplot(aes(x_1, x_2, z=p, fill=p)) +
  geom_raster() +
  scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
  stat_contour(breaks=c(0.5), color="black") 

p_hat <- predict(fit_glm, newdata = mnist_27$true_p)
mnist_27$true_p %>%
  mutate(p_hat = p_hat) %>%
  ggplot(aes(x_1, x_2,  z=p_hat, fill=p_hat)) +
  geom_raster() +
  scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
  stat_contour(breaks=c(0.5),color="black") 

p_hat <- predict(fit_glm, newdata = mnist_27$true_p)
mnist_27$true_p %>%
  mutate(p_hat = p_hat) %>%
  ggplot() +
  stat_contour(aes(x_1, x_2, z=p_hat), breaks=c(0.5), color="black") +
  geom_point(mapping = aes(x_1, x_2, color=y), data = mnist_27$test)


# Labs
set.seed(2, sample.kind="Rounding") #if you are using R 3.6 or later
make_data <- function(n = 1000, p = 0.5, 
                      mu_0 = 0, mu_1 = 2, 
                      sigma_0 = 1,  sigma_1 = 1){
  
  y <- rbinom(n, 1, p)
  f_0 <- rnorm(n, mu_0, sigma_0)
  f_1 <- rnorm(n, mu_1, sigma_1)
  x <- ifelse(y == 1, f_1, f_0)
  
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  
  list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
       test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
}
dat <- make_data()

dat$train %>% ggplot(aes(x, color = y)) + geom_density()

mu_1 = 3

mu_1 <- seq(0, 3, len=25)
logistic_run <- function(mu_1){
  n = 1000
  p = 0.5
  mu_0 = 0
  sigma_0 = 1
  sigma_1 = 1
  y <- rbinom(n, 1, p)
  f_0 <- rnorm(n, mu_0, sigma_0)
  f_1 <- rnorm(n, mu_1, sigma_1)
  x <- ifelse(y == 1, f_1, f_0)
  
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index)
  test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index)
  fit = glm(y~x, data = train, family = "binomial")
  p = predict(fit, test, type='response')
  y_hat = ifelse(p>0.5,1,0)
  acc = mean(y==y_hat)
  return(acc)
}

set.seed(1, sample.kind="Rounding") 
res = sapply(mu_1, logistic_run)
plot(y=res, x=mu_1) # THE GRAPH IS NOT COMING CORRECT, IT SHOULD HAVE AN INCREASING TREND


# SMOOTHING or CURVE FITTING or LOW PASS FILTERING
library(tidyverse)
library(dslabs)
data("polls_2008")
qplot(day, margin, data = polls_2008)

span <- 7 
fit <- with(polls_2008, ksmooth(day, margin, kernel = "box", bandwidth = span))
class(fit)
names(fit)
polls_2008 %>% mutate(smooth = fit$y) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") + 
  geom_line(aes(day, smooth), color="red")


fit <- with(polls_2008, ksmooth(day, margin, kernel = "normal", bandwidth = span))
polls_2008 %>% mutate(smooth = fit$y) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") + 
  geom_line(aes(day, smooth), color="red")


total_days <- diff(range(polls_2008$day))
span <- 21/total_days

fit <- loess(margin ~ day, degree=1, span = span, data=polls_2008)
names(fit)
polls_2008 %>% mutate(smooth = fit$fitted) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") +
  geom_line(aes(day, smooth), color="red")


total_days <- diff(range(polls_2008$day))
span <- 28/total_days
fit_1 <- loess(margin ~ day, degree=1, span = span, data=polls_2008)
fit_2 <- loess(margin ~ day, span = span, data=polls_2008)
polls_2008 %>% mutate(smooth_1 = fit_1$fitted, smooth_2 = fit_2$fitted) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") +
  geom_line(aes(day, smooth_1), color="red", lty = 2) +
  geom_line(aes(day, smooth_2), color="orange", lty = 1) 

polls_2008 %>% ggplot(aes(day, margin)) +
  geom_point() + 
  geom_smooth()

polls_2008 %>% ggplot(aes(day, margin)) +
  geom_point() + 
  geom_smooth(span = 0.15, method.args = list(degree=1))


# Labs
library(tidyverse)
library(lubridate)
library(purrr)
install.packages('pdftools')
library(pdftools)

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
dat <- map_df(str_split(pdf_text(fn), "\n"), function(s){
  s <- str_trim(s)
  header_index <- str_which(s, "2015")[1]
  tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
  month <- tmp[1]
  header <- tmp[-1]
  tail_index  <- str_which(s, "Total")
  n <- str_count(s, "\\d+")
  out <- c(1:header_index, which(n==1), which(n>=28), tail_index:length(s))
  s[-out] %>%
    str_remove_all("[^\\d\\s]") %>%
    str_trim() %>%
    str_split_fixed("\\s+", n = 6) %>%
    .[,1:5] %>%
    as_data_frame() %>% 
    setNames(c("day", header)) %>%
    mutate(month = month,
           day = as.numeric(day)) %>%
    gather(year, deaths, -c(day, month)) %>%
    mutate(deaths = as.numeric(deaths))
}) %>%
  mutate(month = recode(month, "JAN" = 1, "FEB" = 2, "MAR" = 3, "APR" = 4, "MAY" = 5, "JUN" = 6, 
                        "JUL" = 7, "AGO" = 8, "SEP" = 9, "OCT" = 10, "NOV" = 11, "DEC" = 12)) %>%
  mutate(date = make_date(year, month, day)) %>%
  filter(date <= "2018-05-01")

head(dat)
span = 60/nrow(dat)
dat = dat %>% mutate(d = 1:nrow(dat))
fit = loess(deaths ~ d, data = dat, span = span, degree = 1)
dat[-1,]%>% mutate(smooth = fit$fitted) %>% ggplot(aes(date,smooth)) + geom_line(size = 2, color = 'red') + geom_point(aes(x=date, y = deaths), alpha = 0.5)
fit$fitted

# Labs
library(broom)
library(dslabs)
data(mnist_27)
mnist_27$train %>% glm(y ~ x_2, family = "binomial", data = .) %>% tidy()
span = 21/nrow(mnist_27$train)
fit = loess(as.numeric(y) ~ x_2, data = mnist_27$train, degree = 1, span = span)
mnist_27$train %>% mutate(smooth = fit$fitted) %>% ggplot(aes(x=x_2, y = smooth))+geom_line()



# MATRIX UNDERSTANDING
library(tidyverse)
library(dslabs)
if(!exists("mnist")) mnist <- read_mnist()

names(mnist)
names(mnist$train)
class(mnist$train)
class(mnist$train$images)
class(mnist$train$labels)
dim(mnist$train$images)

x <- mnist$train$images[1:1000,] 
y <- mnist$train$labels[1:1000]
rm(mnist)

y[3]
grid <- matrix(x[3,], 28, 28)
View(grid)
image(1:28, 1:28, grid)
image(1:28, 1:28, grid[, 28:1])

avg = rowMeans(x)
tibble(labels = as.factor(y), row_averages = avg) %>% 
  qplot(labels, row_averages, data = ., geom = "boxplot") 

install.packages('matrixStats')
library(matrixStats)
sds <- colSds(x)
sds  

qplot(sds, bins = "30", color = I("black"))
image(1:28, 1:28, matrix(sds, 28, 28)[, 28:1])
new_x <- x[ ,colSds(x) > 60]
dim(new_x)
#> [1] 1000  314

class(x[ , 1, drop=FALSE])
#> [1] "matrix"
dim(x[, 1, drop=FALSE])
#> [1] 1000    1

qplot(as.vector(x), bins = 30, color = I("black"))
qplot(as.vector(new_x), bins = 30, color = I("black"))
new_x <- x
new_x[new_x < 50] <- 0

bin_x <- x
bin_x[bin_x < 255/2] <- 0 
bin_x[bin_x > 255/2] <- 1

x_mean_0 <- sweep(x, 2, colMeans(x)) # Sweep is a matrix based function. applies a function on all the rows of a column
x_standardized <- sweep(x_mean_0, 2, colSds(x), FUN = "/") # it also has a function option that allows to put an expression

# matrix multiplication
# mat_x %*% mat_y
# cross product of any matrix 
# crossproduct(x)
# inverse of a matrix - 
# solve(mat_x)

x = as.vector(mnist$train$images)
sum(x[x>50 & x<205])/length(x)


# DISTANCE BASED ALGORITHMS
# CALCULATING DISTANCE BETWEEN ALL 784 PREDICTORS
library(tidyverse)
library(dslabs)
if(!exists("mnist")) mnist <- read_mnist()
set.seed(1995, sample.kind = 'Rounding') # if using R 3.6 or later
ind <- which(mnist$train$labels %in% c(2,7)) %>% sample(500)

#the predictors are in x and the labels in y
x <- mnist$train$images[ind,]
y <- mnist$train$labels[ind]

rm(mnist)

y[1:3]
x_1 <- x[1,]
x_2 <- x[2,]
x_3 <- x[3,]

#distance between two numbers
sqrt(sum((x_1 - x_2)^2))
sqrt(sum((x_1 - x_3)^2))
sqrt(sum((x_2 - x_3)^2))

#compute distance using matrix algebra
sqrt(crossprod(x_1 - x_2))
sqrt(crossprod(x_1 - x_3))
sqrt(crossprod(x_2 - x_3))

#compute distance between each row
d <- dist(x) # DIST FUNCTION CALCULATES DISTANCE ONLY BETWEEN ROWS AND NOT COLUMNS
class(d)
as.matrix(d)[1:3,1:3]

#visualize these distances
image(as.matrix(d))

#order the distance by labels
image(as.matrix(d)[order(y), order(y)])

#compute distance between predictors
d <- dist(t(x)) # using transpose to calculate distance between columns and not rows
dim(as.matrix(d))
d_492 <- as.matrix(d)[492,]
image(1:28, 1:28, matrix(d_492, 28, 28))

# Labs
library(dslabs)
data(tissue_gene_expression)
dim(tissue_gene_expression$x)
table(tissue_gene_expression$y)
d <- dist(tissue_gene_expression$x)
as.matrix(d)[c(1,2,39,40,73,74),c(1,2,39,40,73,74)]
image(as.matrix(d))

# KNN ALGORITHM
# knn works like bin smoothing
# it considers the K nearest points and takes the avg values as the estimate
# Lets say you want to predict Y and you have X1, X2, X3...Xn
# When we provide the algorithm a new set of values (x1,x2,x3...xn) for which we want to predict Y
# it finds the K nearest points from x1,x2,x3,....xn and then for those points 
# calculates the avg of Y (if Y is continous) or Proportion of diff levels of Y (if Y is categorical)
# larger Ks give more smoother estimates and smaller gives more wiggly estimates

#logistic regression
library(caret)
View(mnist_27$train)
fit_glm <- glm(y~., data=mnist_27$train, family="binomial")
p_hat_logistic <- predict(fit_glm, mnist_27$test)
y_hat_logistic <- factor(ifelse(p_hat_logistic > 0.5, 7, 2))
confusionMatrix(data = y_hat_logistic, reference = mnist_27$test$y)$overall[1]

#fit knn model
knn_fit <- knn3(y ~ ., data = mnist_27$train) # default value of k is 5
x <- as.matrix(mnist_27$train[,2:3])
y <- mnist_27$train$y
knn_fit <- knn3(x, y)
knn_fit <- knn3(y ~ ., data = mnist_27$train, k=5)
y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")
confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)$overall["Accuracy"]

knn_fit <- knn3(y ~ ., data = mnist_27$train) # default value of k is 5
y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")
confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)$overall["Accuracy"]


# Now lets see visually how well this model is able to separate the classes
View(mnist_27$true_p) # this dataset provides right probability
mnist_27$true_p %>% ggplot(aes(x_1, x_2, z=p, fill=p)) +
  geom_raster() +
  scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
  stat_contour(breaks=c(0.5), color="black") # this shows the real way the predicted prob should separate
# it is basically showing for diff values of x_1 & x_2 what are the predicted prob
# if the predicted prob is correct then basis the right cut off we will be able to see no overlaps of probabilitis (showns as blue for more than 0.5 and red for less than 0.5)
p_hat <- predict(knn_fit, newdata = mnist_27$true_p)
View(p_hat)
mnist_27$true_p %>%
  mutate(p_hat = p_hat[,2]) %>%
  ggplot(aes(x_1, x_2,  z=p_hat, fill=p_hat)) +
  geom_raster() +
  scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
  stat_contour(breaks=c(0.5),color="black") 

# We can see that the model is failing as there are islands of blue in red regions
# Lets see how this performs on the training data
nrow()
p_hat <- predict(knn_fit, newdata = mnist_27$train)
mnist_27$train %>%
  mutate(p_hat = p_hat[,2]) %>%
  ggplot(aes(x_1, x_2,  z=p_hat, fill=p_hat)) +
  geom_raster() +
  scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
  stat_contour(breaks=c(0.5),color="black") 

nrow(mnist_27$true_p)

#Tuning the KNN model to find the best K
knn_fit <- train(y ~ ., method = "knn", 
                 data = mnist_27$train,
                 tuneGrid = data.frame(k = seq(9, 71, 2))) # tune grid provides a sequence of odd numbers as K from 9 to 71 

knn_fit$bestTune
y_hat_knn <- predict(knn_fit, mnist_27$test, type = "raw")
confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)$overall["Accuracy"]


# UNDERSTANDING CROSS VALIDATION
# When we build any ML model there are model parameters that we need to decide
# For e.g. for KNN we need to decide on K
# If K is 1 than the training is done on each data point individually
# meaning it will be 100% accurate on training and very low accuracy on test
# if K is very high than it will be very less accurate on both train & test
# We need to find a optimal K. Remember that low value of K will lead to over training
# Wrong approach to find optimal K - Make test & train. Run different models on train for diff K 
# Check on test MSE and choose the K with lowest K. Problem is - You decided
# on K using Test dataset. You can not say that this K will hold true in 
# other population. You have no means to know if this will work and Test 
# dataset is already used.

# Right approach is - K-Fold Cross validation
# Here We make train and test. Then take out non overlapiong K samples from training
# So size of each sample will be N/K where N is total obs in Training set
# & K is the total number of cross validations you wish to do. Each such sample is called 
# Validation Set. Every time you take out the Validation Set from Training Set
# the rest of the data is used to train the model. So lets say you want to find
# out optimal k for KNN between 5 to 20. So for each value of k try the K-fold validation
# Meaning build K models and calculate the MSE on validation set each time. 
# You then calculate avg MSE for that value of k, lest say 5. Then do the same
# for k = 6, build K models calculate MSE on all K validation sets. Then calculate avg
# MSE. LIke this do this on all values of k, choose the k which gives min AVG MSE
# For the chosen k, the parameter estimates are avg of all K model runs on Training data
# samples. This gives you the final model on training set. Now run this on Test data finally
# to calculate the MSE expected on population. You can even do K-fold validation on 
# Test data, by  running the model on K different samples from test data and then take an
# avg of MSE to provide the expected value of random error.


library(tidyverse)
library(dslabs)
library(caret)
data("mnist_27")

knn_fit <- knn3(y ~ ., data = mnist_27$train) # default k = 5
y_hat_knn <- predict(knn_fit, mnist_27$train, type = "class") 
confusionMatrix(data = y_hat_knn, reference = mnist_27$train$y)$overall["Accuracy"]
y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")  
confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)$overall["Accuracy"]

#fit knn with k=1
knn_fit_1 <- knn3(y ~ ., data = mnist_27$train, k = 1)
y_hat_knn_1 <- predict(knn_fit_1, mnist_27$train, type = "class")
confusionMatrix(data=y_hat_knn_1, reference=mnist_27$train$y)$overall[["Accuracy"]]
y_hat_knn <- predict(knn_fit_1, mnist_27$test, type = "class")  
confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)$overall["Accuracy"]

#fit knn with k=401
knn_fit_401 <- knn3(y ~ ., data = mnist_27$train, k = 401)
y_hat_knn_401 <- predict(knn_fit_401, mnist_27$train, type = "class")
confusionMatrix(data=y_hat_knn_401, reference=mnist_27$train$y)$overall[["Accuracy"]]
y_hat_knn_401 <- predict(knn_fit_401, mnist_27$test, type = "class")
confusionMatrix(data=y_hat_knn_401, reference=mnist_27$test$y)$overall["Accuracy"]

#pick the k in knn
ks <- seq(3, 251, 2)
library(purrr)
accuracy <- map_df(ks, function(k){
  fit <- knn3(y ~ ., data = mnist_27$train, k = k)
  y_hat <- predict(fit, mnist_27$train, type = "class")
  cm_train <- confusionMatrix(data = y_hat, reference = mnist_27$train$y)
  train_error <- cm_train$overall["Accuracy"]
  y_hat <- predict(fit, mnist_27$test, type = "class")
  cm_test <- confusionMatrix(data = y_hat, reference = mnist_27$test$y)
  test_error <- cm_test$overall["Accuracy"]
  
  tibble(train = train_error, test = test_error)
})

#pick the k that maximizes accuracy using the estimates built on the test data
ks[which.max(accuracy$test)]
max(accuracy$test)


# Labs
library(caret)
library(dslabs)
set.seed(1, sample.kind = 'Rounding')
data(heights)
View(heights)
y <- heights$height
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- heights %>% slice(-test_index)
test_set <- heights %>% slice(test_index)

ks <- seq(1, 101, 3)
library(purrr)
accuracy <- map_df(ks, function(k){
  fit <- knn3(sex ~ height, data = train_set, k = k)
  y_hat <- predict(fit, test_set, type = "class")
  cm_test <- confusionMatrix(data = y_hat, reference = test_set$sex, mode='everything')
  f1 <- cm_test$byClass["F1"]
  tibble(test = f1)
})
max(accuracy$test)
ks[which.max(accuracy$test)]
max(accuracy$test)


ks <- seq(1, 101, 3)
library(purrr)
accuracy <- map_df(ks, function(k){
  fit <- knn3(sex ~ height, data = train_set, k = k)
  y_hat <- predict(fit, test_set, type = "class")
  cm_test <- confusionMatrix(data = y_hat, reference = test_set$sex, mode='everything')
  f1 <- cm_test$byClass["F1"]
  tibble(test = f1)
})
max(accuracy$test)
ks[which.max(accuracy$test)]
max(accuracy$test)

library(dslabs)
data(tissue_gene_expression)
x = tissue_gene_expression$x
y = tissue_gene_expression$y
ks = c(1, 3, 5, 7, 9, 11)

set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_x <- x[-test_index,]
train_y <- y[-test_index]
test_x <- x[test_index,]
test_y <- y[test_index]

library(purrr)
accuracy <- map_df(ks, function(k){
  fit <- knn3(y=train_y, x=train_x, k = k)
  y_hat <- predict(fit, test_x, type = "class")
  cm_test <- confusionMatrix(data = y_hat, reference = test_y, mode='everything')
  f1 <- cm_test$overall["Accuracy"]
  tibble(test = f1)
})
round(accuracy$test,3)
ks[which.max(accuracy$test)]
max(accuracy$test)


