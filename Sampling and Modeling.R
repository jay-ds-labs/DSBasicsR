# Function for taking a random draw from a specific urn
# The dslabs package includes a function for taking a random draw of size  n  from the urn described in the video:
  
library(tidyverse)
library(dslabs)
take_poll(25)    # draw 25 beads

# As N increases , se decreases
p <- seq(0, 1, length = 100)
sample_sizes <- c(25, 100, 1000)
par(mfrow=c(1,3))
for(i in sample_sizes){
  p = seq(0,1,length.out= 100)
  se = sqrt(p*(1-p)/i)
  plot(p,se, xlab=paste('N=',i), ylim = c(0,0.1), xlim = c(0,1))
  
}

# How to know if sample size is sufficient
# Calculate the spread - p-(1-p) = 2p-1
# Calculate the standard error = 2*sqrt[(p*(1-p)/N)]
# N is the sample size

# `N` represents the number of people polled
N <- 25

# `p` represents the proportion of Democratic voters
p <- 0.45

# Calculate the standard error of the spread. Print this value to the console.
se = 2*sqrt(p*(1-p)/N)
se

spread = 2*p -1
spread  # as se is more than spread, sample size (N) is not sufficient

# X_hat is the average value of random draws from a urn
# Computing the probability of  X_hat  being within .01 of  p 
N = 25 # number of draws
X_hat <- 0.48
se <- sqrt(X_hat*(1-X_hat)/N)
pnorm(0.01/se) - pnorm(-0.01/se)

# The margin of error is defined as 2 times the standard error of the estimate  X¯ .
# There is about a 95% chance that  X¯  will be within two standard errors of the actual parameter  p .


# MONTE CARLO SIMULATION OF CLT
# CLT - If we take mean of random samples from a population than the means will be distributed normally and avg of all sample means will be same as population mean and the std dev of the sample will be std dev of the population divided by sqrt(sample size)
# this can be proved by using monte carlo simulation
# Monte Carlo simulation using a set value of p
p <- 0.45    # unknown p to estimate
N <- 1000

# simulate one poll of size N and determine x_hat
x <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
x_hat <- mean(x)

# simulate B polls of size N and determine average x_hat
B <- 10000    # number of replicates
N <- 1000    # sample size per replicate
x_hat <- replicate(B, {
  x <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
  mean(x)
})

# Histogram and QQ-plot of Monte Carlo results
library(tidyverse)
# install.packages("gridExtra")
library(gridExtra)
p1 <- data.frame(x_hat = x_hat) %>%
  ggplot(aes(x_hat)) +
  geom_histogram(binwidth = 0.005, color = "black")
p2 <- data.frame(x_hat = x_hat) %>%
  ggplot(aes(sample = x_hat)) +
  stat_qq(dparams = list(mean = mean(x_hat), sd = sd(x_hat))) +
  geom_abline() +
  ylab("X_hat") +
  xlab("Theoretical normal")
grid.arrange(p1, p2, nrow=1)

# you can make QQ plot in 3 ways 
# Easy way - 
data.frame(x_hat = x_hat) %>% ggplot(aes(sample = x_hat)) + geom_qq() + geom_qq_line()
# Detailed way - 
data.frame(x_hat = x_hat) %>% ggplot(aes(sample = x_hat)) + geom_qq(dparams = list(mean = mean(x_hat), sd = sd(x_hat))) + geom_qq_line(dparams = list(mean = mean(x_hat), sd = sd(x_hat)))
# the case study uses stat_qq which is another way to specify the aes
# aes can be specifed in 3 ways - during the ggplot deifinition (this will impact all the layers). After the ggplot and bore the layers using stat (this will impact the layers after the stat definition). In the layer it self (like you see qhen dparams are defined in both geom_qqplot and geom_qqline)
# we can do the by adding the dparams in ggplot itself
data.frame(x_hat = x_hat) %>% ggplot(aes(sample = x_hat), dparams = list(mean = mean(x_hat), sd = sd(x_hat))) + geom_qq() + geom_qq_line()

# The spread between two outcomes with probabilities  p  and  1−p  is  2p−1 .
# The expected value of the spread is  2X¯−1 .
# The standard error of the spread is  2SE(X¯) .
# The margin of error of the spread is 2 times the margin of error of  X¯ (= 2E(X¯) )

# Bias in polls
# An extremely large poll would theoretically be able to predict election results almost perfectly.
# These sample sizes are not practical. In addition to cost concerns, polling doesn't reach everyone in the population (eventual voters) with equal probability, and it also may include data from outside our population (people who will not end up voting).
# These systematic errors in polling are called bias. We will learn more about bias in the future.

# Plotting margin of error in an extremely large poll over a range of values of p
library(tidyverse)
N <- 100000
p <- seq(0.35, 0.65, length = 100)
SE <- sapply(p, function(x) 2*sqrt(x*(1-x)/N))
data.frame(p = p, SE = SE) %>%
  ggplot(aes(p, SE)) +
  geom_line()


# Lab
# Write a function called `take_sample` that takes `p` and `N` as arguements and returns the average value of a randomly sampled population.
take_sample = function(p,N){
  s = sample(c(0,1), N, replace = T, prob = c((1-p), p))
  return(mean(s))
}

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)
# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45
# Define `N` as the number of people polled
N <- 100
# Call the `take_sample` function to determine the sample average of `N` randomly selected people from a population containing a proportion of Democrats equal to `p`. Print this value to the console.
take_sample(p,N)

B <- 10000

errors = replicate(B, {
  x_hat = take_sample(p,N)
  p-x_hat
})

mean(errors)

hist(errors)

mean(abs(errors))
sqrt(mean(errors^2))

qqnorm(errors)
qqline(errors)
se = sqrt(p*(1-p)/N)
se

X = sample(c(0,1), N, replace = T, prob = c((1-p), p))
X_bar = mean(X)
se = sqrt(X_bar*(1-X_bar)/N)
se

N <- seq(100, 5000, len = 100)
p <- 0.5
se <- sqrt(p*(1-p)/N)
plot(N,se)
N[se<=0.01]

p = 0.45
N = 100
m = p
s = sqrt(p*(1-p)/N)
1 - pnorm(0.5,m,s)


N = 100
X_hat = 0.51
se_hat = sqrt(X_hat*(1-X_hat)/N)
a = pnorm(-0.01,0,se_hat)
b = 1-pnorm(0.01,0,se_hat)
a+b

# CONFIDENCE INTERVAL
# geom_smooth confidence interval example
# The shaded area around the curve is related to the concept of confidence intervals.

data("nhtemp")
View(nhtemp)
data.frame(year = as.numeric(time(nhtemp)), temperature = as.numeric(nhtemp)) %>%
  ggplot(aes(year, temperature)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Average Yearly Temperatures in New Haven")

# Monte Carlo simulation of confidence intervals
# Note that to compute the exact 95% confidence interval, we would use qnorm(.975)*SE_hat instead of 2*SE_hat.

p <- 0.45
N <- 1000
X <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))    # generate N observations
X_hat <- mean(X)    # calculate X_hat
SE_hat <- sqrt(X_hat*(1-X_hat)/N)    # calculate SE_hat, SE of the mean of N observations
c(X_hat - 2*SE_hat, X_hat + 2*SE_hat)    # build interval of 2*SE above and below mean
# Code: Solving for  z  with qnorm
z <- qnorm(0.995)    # calculate z to solve for 99% confidence interval
pnorm(qnorm(0.995))    # demonstrating that qnorm gives the z value for a given probability
pnorm(qnorm(1-0.995))    # demonstrating symmetry of 1-qnorm
pnorm(z) - pnorm(-z)    # demonstrating that this z value gives correct probability for interval

# Monte Carlo simulation
# Note that to compute the exact 95% confidence interval, 
# we would use qnorm(.975)*SE_hat instead of 2*SE_hat.

B <- 10000
inside <- replicate(B, {
  X <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
  X_hat <- mean(X)
  SE_hat <- sqrt(X_hat*(1-X_hat)/N)
  between(p, X_hat - 2*SE_hat, X_hat + 2*SE_hat)    # TRUE if p in confidence interval
})
mean(inside)

# When using confidence intervals, a lot of times we make the mistake of saying 
# that p falls between the confidence interval 95% of the time. 
# that implies that p is random.
# but p is never random. p is the prob in the population. 
# we are basically taking out samples and looking at the 
# standard error in sample. So it is the confidence interval which is random
# we should say that there is 95% chance that confidence interval will fall on p
# while p is always same

# Confidence interval for the spread with sample size of 25
# Note that to compute the exact 95% confidence interval, we would use 
# c(-qnorm(.975), qnorm(.975)) instead of 1.96.

N <- 25
X_hat <- 0.48
(2*X_hat - 1) + c(-2, 2)*2*sqrt(X_hat*(1-X_hat)/N)

# Computing a p-value for observed spread of 0.02
N <- 100    # sample size
z <- sqrt(N) * 0.02/0.5    # spread of 0.02
1 - (pnorm(z) - pnorm(-z))
2*(1-pnorm(z)) # same as 2*(1-pnorm(z, mu, sigma)) 

# Poll result case study
library(dslabs)
data("polls_us_election_2016")
View(polls_us_election_2016)
str(polls_us_election_2016)
polls = polls_us_election_2016 %>% filter(enddate>='2016-10-31', state == 'U.S.')
nrow((polls))
View(polls)
N = polls[1,'samplesize']
N
X_hat = polls[1, 'rawpoll_clinton']/100 # converting to proportion
X_hat
se_hat <- sqrt(X_hat*(1-X_hat)/N)
se_hat
ci = c(X_hat - qnorm(.975)*se_hat, X_hat + qnorm(.975)*se_hat)
ci

pollster_results = polls %>% mutate(N=samplesize, X_hat = rawpoll_clinton/100, se_hat = sqrt(X_hat*(1-X_hat)/N), lower = X_hat - qnorm(.975)*se_hat, upper = X_hat + qnorm(.975)*se_hat) %>% select(pollster, enddate, rawpoll_clinton, X_hat, se_hat, lower, upper) 
p = 0.482
avg_hit = pollster_results %>% mutate(hit = ifelse(p>=lower & p<=upper, 1,0)) %>% summarize(mean(hit))
avg_hit

polls <- polls_us_election_2016 %>% filter(enddate >= "2016-10-31" & state == "U.S.")
polls <- polls %>% mutate(d_hat = rawpoll_clinton/100 - rawpoll_trump/100)
N = polls[1,'samplesize']
N
d_hat = polls[1,'d_hat']
p = (d_hat+1)/2
X_hat = p
X_hat
se_hat <- 2*sqrt(X_hat*(1-X_hat)/N) # The standard error of the spread d_hat is 2 times the standard error of X_hat
se_hat
ci = c(d_hat - qnorm(.975)*se_hat, d_hat + qnorm(.975)*se_hat)
ci

pollster_results = polls %>% mutate(N=samplesize, p = (d_hat+1)/2, X_hat = p, 
                                    se_hat = 2*sqrt(X_hat*(1-X_hat)/N), 
                                    lower = d_hat - qnorm(.975)*se_hat, 
                                    upper = d_hat + qnorm(.975)*se_hat) %>% select(pollster, enddate, d_hat, lower, upper) 

d = 0.021
avg_hit = pollster_results %>% mutate(hit = ifelse(d>=lower & d<=upper, 1,0)) %>% summarize(mean(hit))
avg_hit

polls %>% mutate(errors =d_hat-0.021) %>% ggplot(aes(x = pollster, y = errors)) +
  geom_point() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

polls %>% mutate(errors =d_hat-0.021) %>% group_by(pollster) %>% filter(n()>=5) %>% ungroup() %>% ggplot(aes(x = pollster, y = errors)) +
  geom_point() + theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Simulating polls
# Note that to compute the exact 95% confidence interval, 
# we would use qnorm(.975)*SE_hat instead of 2*SE_hat.

d <- 0.039
Ns <- c(1298, 533, 1342, 897, 774, 254, 812, 324, 1291, 1056, 2172, 516)
p <- (d+1)/2

# calculate confidence intervals of the spread
confidence_intervals <- sapply(Ns, function(N){
  X <- sample(c(0,1), size=N, replace=TRUE, prob = c(1-p, p))
  X_hat <- mean(X)
  SE_hat <- sqrt(X_hat*(1-X_hat)/N)
  2*c(X_hat, X_hat - 2*SE_hat, X_hat + 2*SE_hat) - 1
})

# generate a data frame storing results
polls <- data.frame(poll = 1:ncol(confidence_intervals),
                    t(confidence_intervals), sample_size = Ns)
names(polls) <- c("poll", "estimate", "low", "high", "sample_size")
polls

# Calculating the spread of combined polls
# Note that to compute the exact 95% confidence interval, 
# we would use qnorm(.975) instead of 1.96.

d_hat <- polls %>%
  summarize(avg = sum(estimate*sample_size) / sum(sample_size)) %>%
  .$avg

p_hat <- (1+d_hat)/2
moe <- 2*1.96*sqrt(p_hat*(1-p_hat)/sum(polls$sample_size))   
round(d_hat*100,1)
round(moe*100, 1)

# Generating simulated poll data
library(dslabs)
data(polls_us_election_2016)
names(polls_us_election_2016)

# keep only national polls from week before election with a grade considered reliable
polls <- polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-10-31" &
           (grade %in% c("A+", "A", "A-", "B+") | is.na(grade)))

# add spread estimate
polls <- polls %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# compute estimated spread for combined polls
d_hat <- polls %>%
  summarize(d_hat = sum(spread * samplesize) / sum(samplesize)) %>%
  .$d_hat

# compute margin of error
p_hat <- (d_hat+1)/2
moe <- 1.96 * 2 * sqrt(p_hat*(1-p_hat)/sum(polls$samplesize))

# histogram of the spread
polls %>%
  ggplot(aes(spread)) +
  geom_histogram(color="black", binwidth = .01)

# Code: Investigating poll data and pollster bias
# number of polls per pollster in week before election
polls %>% group_by(pollster) %>% summarize(n())

# plot results by pollsters with at least 6 polls
polls %>% group_by(pollster) %>%
  filter(n() >= 6) %>%
  ggplot(aes(pollster, spread)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# standard errors within each pollster
polls %>% group_by(pollster) %>%
  filter(n() >= 6) %>%
  summarize(se = 2 * sqrt(p_hat * (1-p_hat) / median(samplesize)))

# Note that to compute the exact 95% confidence interval, we would use qnorm(.975) instead of 1.96.
# collect last result before the election for each pollster
one_poll_per_pollster <- polls %>% group_by(pollster) %>%
  filter(enddate == max(enddate)) %>%      # keep latest poll
  ungroup()

# histogram of spread estimates
one_poll_per_pollster %>%
  ggplot(aes(spread)) + geom_histogram(binwidth = 0.01)

# construct 95% confidence interval
results <- one_poll_per_pollster %>%
  summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg - 1.96*se, end = avg + 1.96*se)
round(results*100, 1)

# Lab on statistical models
library(dslabs)
data(heights)

# Make a vector of heights from all males in the population
x <- heights %>% filter(sex == "Male") %>%
  .$height

mean(x)
sd(x)

# Call the population average computed above μ and the standard deviation σ. Now take a sample of size 50, with replacement, and construct an estimate for μ and σ.
head(x)
set.seed(1)
# Define `N` as the number of people measured
# Find out the error in the avg height of sample
# Find out the 95% confidence interval
N <- 50
X = sample(x,50, replace = T)
X_hat = mean(X)
X_sd = sd(X)
se = X_sd / sqrt(N) # the standard error in estimate of mean is this
ci = c(X_hat - qnorm(0.975)*se, X_hat + qnorm(0.975)*se) # this is the confidence interval of avg height of sample
ci

# Monte Carlo Simulation for Heights
# Now run a Monte Carlo simulation in which you compute 10,000 confidence intervals as you have just done. What proportion of these intervals include μ?
# Define `mu` as the population average
x <- heights %>% filter(sex == "Male") %>%
  .$height
mu <- mean(x)
# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)
# Define `N` as the number of people measured
N <- 50
# Define `B` as the number of times to run the model
B <- 10000
# Define an object `res` that contains a logical vector for simulated intervals that contain mu
res = replicate(B,{
  X = sample(x,N, replace = T)
  X_hat = mean(X)
  X_sd = sd(X)
  se = X_sd / sqrt(N) # the standard error in estimate of mean is this
  check = ifelse((X_hat - qnorm(0.975)*se) <= mu & mu <= (X_hat + qnorm(0.975)*se),1,0) # this is the confidence interval of avg height of sample
  check
})

# Calculate the proportion of results in `res` that include mu. Print this value to the console.
mean(res)


# Lab - Visualizing Polling Bias
# In this section, we used visualization to motivate the presence of pollster bias in election polls. Here we will examine that bias more rigorously. Lets consider two pollsters that conducted daily polls and look at national polls for the month before the election.
# Is there a poll bias? Make a plot of the spreads for each poll.

# Load the libraries and data you need for the following exercises
library(dslabs)
library(dplyr)
library(ggplot2)
data("polls_us_election_2016")

# These lines of code filter for the polls we want and calculate the spreads
polls <- polls_us_election_2016 %>% 
  filter(pollster %in% c("Rasmussen Reports/Pulse Opinion Research","The Times-Picayune/Lucid") &
           enddate >= "2016-10-15" &
           state == "U.S.") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) 

# Make a boxplot with points of the spread for each pollster
polls %>% ggplot(aes(x = pollster, y = spread)) + geom_boxplot() + geom_point()

# Compute the estimates of σ1 and σ2.
# Create an object called `sigma` that contains a column for `pollster` and a column for `s`, the standard deviation of the spread
sigma = polls %>% group_by(pollster) %>% summarize(s = sd(spread))
sigma

# Probability Distribution of the Spread - if we assume sample size of each pollster polls are large enough than as per CLT the difference between their expected values, i.e. spread will also be normally distributed

# Calculate the 95% Confidence Interval of the Spreads
# Create an object called `res` that summarizes the average, standard deviation, and number of polls for the two pollsters.
res = polls %>% group_by(pollster) %>% summarize(m = mean(spread), s = sd(spread), n = n())
res

# Store the difference between the larger average and the smaller in a variable called `estimate`. Print this value to the console.
estimate = abs(res$m[1]-res$m[2])

# Store the standard error of the estimates as a variable called `se_hat`. Print this value to the console.
se_hat = sqrt(((res$s[1])^2/res$n[1]) + ((res$s[2])^2/res$n[2]))

# Calculate the 95% confidence interval of the spreads. Save the lower and then the upper confidence interval to a variable called `ci`.
ci = c(estimate - qnorm(0.975)*se_hat , estimate + qnorm(0.975)*se_hat)
ci # this is the confidence interval of difference in bias for both polls. 
# we can see that this is quite high from 3% to 6%

# Calculate the P-value
# The confidence interval tells us there is relatively strong pollster effect resulting in a difference of about 5%. Random variability does not seem to explain it.
# Compute a p-value to relay the fact that chance does not explain the observed pollster effect.
# Use the pnorm function to calculate the probability that a random value is larger than the observed ratio of the estimate to the standard error.
# Multiply the probability by 2, because this is the two-tailed test.
q = estimate / se_hat  
2*(1-pnorm(q,0,1))

# Comparing Within-Poll and Between-Poll Variability
# Compute the average and standard deviation for each pollster and examine the variability across the averages and how it compares to the variability within the pollsters, summarized by the standard deviation.

# Group the polls data by pollster.
polls <- polls_us_election_2016 %>% 
  filter(enddate >= "2016-10-15" &
           state == "U.S.") %>%
  group_by(pollster) %>%
  filter(n() >= 5) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  ungroup()

# Summarize the average and standard deviation of the spreads for each pollster.
var = polls %>% group_by(pollster) %>% summarize(avg = mean(spread), s = sd(spread))
var


#
library(tidyverse)
library(dslabs)
d <- 0.039
Ns <- c(1298, 533, 1342, 897, 774, 254, 812, 324, 1291, 1056, 2172, 516)
p <- (d + 1) / 2

polls <- map_df(Ns, function(N) {
  x <- sample(c(0,1), size=N, replace=TRUE, prob=c(1-p, p))
  x_hat <- mean(x)
  se_hat <- sqrt(x_hat * (1 - x_hat) / N)
  list(estimate = 2 * x_hat - 1, 
       low = 2*(x_hat - 1.96*se_hat) - 1, 
       high = 2*(x_hat + 1.96*se_hat) - 1,
       sample_size = N)
}) %>% mutate(poll = seq_along(Ns))















































































































































































































