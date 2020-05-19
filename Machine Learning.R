# KEY UNDERSTANDINGS









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




