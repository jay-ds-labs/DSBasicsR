# 1. URN BASED MODELS
# In real life we see an event happening. We can then find the prob of that event happening
# Lot of times we extraplotae that learning to draw conclusions for future
# Event happenings that we consider for our study are actually samples from 
# a Population. But we do not have access to the population
# We want to draw inference from the sample about the population
# For e.g. Imagine there is an urn with a lot of beads. 
# They are all of 0 color or 1 color. 
# Now you take out a Sample of N beads. You calculate the proportion of 1 in the
# sample as p. Now 3 questions can arise in your mind - 
# A.To what extent does this p resemble the actual proportion of beads in urn. i.e.
#   What is the error in this p, what is the range of that we can expect to be
#   99% correct or 95% correct?
# B.What is the right sample size to draw any conclusion

# Answer to A. If you know p & N, you can find the Standard Error (se) in your 
# prediction -> se = sqrt(p*(1-p)/N). SE is same as the standard deviation of p
# Meaning on an avg the p can vary between p-se & p+se
# But we want to know the 95% confidence interval of p
# For that we will first use CLT to say that the expected value of a sample i.e.
# the mean or p is normally distributed. Now in a normal distribution if we want a 
# 95% confidence interval than it will mean all the values within the interval
# 2.5%ile to 97.5%ile value so that you have 95% in between and 2.5% left out in 
# both direction. We can find 97.5%ile value in a normal distribution using -
# qnorm(0.975). 
# Now in a normal distribution with mean 0 and sd 1 we can say that 
# for 95% confidence interval all values will be between 0-qnorm(0.975,0,1) & 0+qnorm(0.975,0,1)
# But for other mean & std dev, we will use mean - sd*qnorm(0.975,0,1) & mean + sd*qnorm(0.975,0,1)
# So in our case for 95% confidence interval of p we will use - 
# p - se*qnorm(0.975,0,1) to p + se*qnorm(0.975,0,1) Where se = sqrt(p*(1-p)/N)

# Answer to B. SE decreases as we increase N. To find if N is sufficient
# Calculate spread of p = p-(1-p) = 2p-1
# Then calculate SE of spread = 2*(SE of p) = 2*sqrt(p*(1-p)/N)
# If SE of Spread > Spread than we know sample size N is not sufficient

# When using confidence intervals, a lot of times we make the mistake of saying 
# that p falls between the confidence interval 95% of the time. 
# that implies that p is random.
# but p is never random. p is the prob in the population. 
# we are basically taking out samples and looking at the 
# standard error in sample. So it is the confidence interval which is random
# we should say that there is 95% chance that confidence interval will fall on p
# while p is always same


# LAB 1 - To show as N increases, SE decreases
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

# LAB 2 - To find out if N is sufficient
# Lets say - in a sample size N = 25, p is 0.45. Lets find out if N is sufficient to 
# say anything about p in population
N <- 25
p <- 0.45
# Calculate the standard error of the spread.
se = 2*sqrt(p*(1-p)/N)


spread = 2*p -1
spread  # as se is more than spread, sample size (N) is not sufficient


# Real Life Application of Urn Model on Polling Data
# Case 1 - In an election poll result, we can find the 95% confidence 
# interval of winning from the results of a SINGLE poll
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
X_hat # The poll result Hillary Cllinton will win 47% of votes
se_hat <- sqrt(X_hat*(1-X_hat)/N)
se_hat
ci = c(X_hat - qnorm(.975)*se_hat, X_hat + qnorm(.975)*se_hat)
ci # 95% chance that Hillary will win 45% to 49% of votes
# Here remember that Margin of Error (moe) = qnorm(.975)*se_hat
# so CI = X_hat +/- MOE

# Case 2 - Predicting the spread of winning and 95% confidence interval
# when multiple polls are happening

# lets choose the right set of polls to consider
library(dslabs)
data(polls_us_election_2016)
polls <- polls_us_election_2016 %>% 
  filter(state == "U.S." & enddate >= "2016-10-31" &
           (grade %in% c("A+","A","A-","B+") | is.na(grade)))
polls <- polls %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# Expected spread = d_hat = sum(spread * samplesize) / sum(samplesize)
d_hat <- polls %>% 
summarize(d_hat = sum(spread * samplesize) / sum(samplesize)) %>% 
  pull(d_hat)

# From the spread we can find out expected proportion of votes to be won
# spread = p-(1-p) = 2p-1
p_hat <- (d_hat+1)/2 
# Note for the se for multiple polls, N = su of all sample sizes
se = sqrt(p_hat * (1 - p_hat) / sum(polls$samplesize))
moe = qnorm(0.975)*se
# MOE for spread (d_hat) is 2*MOE for proportion (p_hat)
moe_spread <- 2*moe
# So what does it say
p_hat 
moe
d_hat
moe_spread
# 50.7% chance of winning with possible error of 0.33%
# Possible spread of 1.4% with error of 0.66%

# On election night, we discover that the actual percentage was 2.1%, 
# which is outside a 95% confidence interval. 

# Our predictions were proved wrong as the underlying polls stated the spread 
# which was not normally distributed and there was a very high bias in predictions
# across the polls

polls %>%
  ggplot(aes(spread)) +
  geom_histogram(color="black", binwidth = .01)
# We can see that spread is not normally distributed

# Plotting the spread predicted by all the pollsters
polls %>% ggplot(aes(x=spread, y = pollster)) + geom_point()

# We see that there are a lot of pollsters with less number of polls
# lets remove them
polls %>% group_by(pollster) %>% filter(n()>=6) %>% ggplot(aes(x=spread, y = pollster)) + geom_point()

# with so much difference in spread - USC predicted Clinton to loose by 5% 
# and Ipsos predicted a win for Clinton by 5%
# so our current URN BASED MODEL fails in current scenario where normal assumptions are failing

# Lets try a different method to find the expected proportion of votes, spread 
# and confidence interval in case the values by polls are not normally distributed
# For each pollster, let’s collect their last reported result before the election:

one_poll_per_pollster <- polls %>% group_by(pollster) %>% 
  filter(enddate == max(enddate)) %>%
  ungroup()

# Here is a histogram of the data for these 15 pollsters:
qplot(spread, data = one_poll_per_pollster, binwidth = 0.01)
# Now, if the values in the URN were just 0's & 1's we could have 
# calculated P_Hat and then D_Hat. But we have numbers between -1 to 1 
# as Values in the urn
# in such a case we use the Data Driven Model, which is nothing but just a 
# fancy name to saying that
# Expected value of p_hat (proportion of votes) or d_hat will be -
# mean(p of all polls). 
# And Standard Error (se) = Standard Deviation of all valules in polls / sqrt(Number of values)
# Finally 95% confidence interval = mean +/- qnorm(0.975)*se
# So, lets calculate these values in our case - 
results <- one_poll_per_pollster %>% 
  summarize(avg = mean(spread), 
            se = sd(spread) / sqrt(length(spread))) %>% 
  mutate(start = avg - 1.96 * se, 
         end = avg + 1.96 * se) 
round(results * 100, 1)
# Our confidence interval is wider now since it incorporates the pollster variability.
# It does include the election night result of 2.1%. 
# Also, note that it was small enough not to include 0, 
# which means we were confident Clinton would win the popular vote.

# SO THE MOST IMPORTANT THING ABOUT URN MODEL IS THAT - 
# IT IS SUITABLE ONLY FOR CASES WHEN THE URN HAS 0,1 VALUES
# I.E. THERE IS ONLY A SINGLE VALUE OF P
# WHEN WE SAY THAT THERE IS A 95% PROABILITY TO WIN WE MEAN THAT
# THERE IS A 95% CONFIDENCE INTERVAL THAT P>0.5
# THIS IS POSSIBLE ONLY IF P IS VARYING, MEANING
# IN THE URN THERE ARE MULTIPLE VALUES OF P RATHER THAN 0 & 1

# IN SUCH A CASE WE USE BAYESIAN STATISTICS AND HIERARCHICAL MODELS TO PREDICT 
# THE VARYING P ACROSS DIFFERENT LEVELS LIKE - 
# DIFFERENT POLLS (VALUES OF P CAPTURED BY DIFF POLLS), 
# DIFFERENT DAYS (VALUES OF P CAPTURED ACROSS DIFF DAYS), 
# DIFFERENT STATES (VALUES OF P CAPTURED ACROSS DIFF STATES), ETC.

# BAYES THEOREM
# P(A|B) = P(A & B) / P(B)  
# P(A|B) = P(B|A) * P(A) / P(B) THE NUMERATOR IS SPLIT USING MULTIPLICATIVE RULE 
# WHICH STATES THAT P(A & B) = P(B|A) * P(A) OR P(A|B) * P(B) 
# SO MULTIPLICATIVE RULE IS COMING FROM BAYES THEOREM ITSELF
# P(A|B) MEANS IF B HAPPENS, WHAT IS THE PROBABIILTY OF A
# FOR E.G - IF A TEST HAS 99% ACCURACY FOR PRESENCE OF DISEASE THAN IT MEANS THAT - 
# P(+ | D = 1) = 0.99 I.E IF SOMEONE HAS A DISEASE, THAN 99% TIMES THE TEST IS +
# SIMILARLY P(- | D = 0) = 0.99 I.E IF SOMEONE DOESNT HAVE DISEASE, THAN 99% TIMES THE TEST IS -
# NOW LETS USE BAYES THEOREM TO ANSWER A QUESTION - 
# IF SOMEONE IS TESTED POSITIVE WHAT IS THE PROB THAT HE OR SHE HAS THE DISEASE
# POSITIVELY I.E. WHAT IS THE VALUE OF P(D=1 | +) 
# P(D=1 | +) = P(+ | D=1) * P(D=1) / P(+)
# P(D=1) MEANS PROBABILITY OF ANYONE TO HAVE DISEASE, LETS SAY THAT IS = 1 IN 3900
# I.E. P(D = 1) = 0.00025
# P(+) = PROBABILITY OF TESTING POSITIVE
# P(+) = (PROB OF TESTING +ve WHEN YOU HAVE DISEASE) + (PROB OF TESTING +ve WHEN YOU DONT HAVE DISEASE)
# P(+) = P(+ & D=1) + P(+ & D=0)
# P(+) = P(+ | D=1) * P(D=1) + P(+ | D=0) * P(D=0)
# P(+) = 0.99 * 0.00025 + 0.01*0.99975
# P(+) = 0.010245
# SO, P(D=1 | +) = P(+ | D=1) * P(D=1) / P(+)
# P(D=1 | +) = 0.99 * 0.00025 / 0.010245
# P(D=1 | +) = 0.024
# THIS IS INTERESTING, AS WHAT IT MEANS IS - 
# Despite the test having 0.99 accuracy, 
# the probability of having the disease given a positive test is only 0.02
# This may appear counter-intuitive to some, 
# but the reason this is the case is because we have to factor in the very 
# rare probability that a person, chosen at random, has the disease. 
# To illustrate this, we run a Monte Carlo simulation.
N = 100000
set.seed(1,sample.kind = 'Rounding')
disease = sample(c(1,0), N, replace = T, prob = c(1/3900, 1-(1/3900)))
test = logical(length(disease))
test[disease==1] = sample(c(1,0), size = sum(disease), replace = T, prob = c(0.99,0.01))
test[disease==0] = sample(c(0,1), size = N-sum(disease), replace = T, prob = c(0.99,0.01))
# table(disease,test)
# p(D=1 | +) = sum(disease ==1 & test ==1) / sum(test==1)
p = sum(disease ==1 & test ==1) / sum(test==1)
p # 0.02, as expected by Bayes Theorem

# Now lets run this simulation several times and see the avg value of p
BayesTheoremFunc = function(){
  N = 100000
  set.seed(1,sample.kind = 'Rounding')
  disease = sample(c(1,0), N, replace = T, prob = c(1/3900, 1-(1/3900)))
  test = logical(length(disease))
  test[disease==1] = sample(c(1,0), size = sum(disease), replace = T, prob = c(0.99,0.01))
  test[disease==0] = sample(c(0,1), size = N-sum(disease), replace = T, prob = c(0.99,0.01))
  p = sum(disease ==1 & test ==1) / sum(test==1)
  return(p)
}

B = 10000
resultP = replicate(B, BayesTheoremFunc())
mean(resultP) # 0.019, again same as Bayes Theorem output


# Hierarchical Models (uses Bayes Theorem)
# Lets take the case of a Baseball player Jose Iglesias
# After 20 bats his avg is 0.450
# Every time he goes to bat it is 1 (if success) or 0 
# So Batting Avg means Avg success rate. So with 0.450 it means 45% of success rate
# The question is at the end of the season what is the prob that he will break the 
# record of best batting avg of all times of 0.406. A season has 500 bats
# At the end of 20 bats, the standard error in p = 0.45 is given by - 
n = 20
p = 0.45
se = sqrt(p*(1-p)/n)
se
ci = c(p-qnorm(0.975)*se , p+qnorm(0.975)*se)
ci
# This prediction has two problems. 
# First, it is very large, so not very useful. 
# Second, it is centered at .450, which implies that our best guess is that this 
# new player will break Ted Williams’ record.

# Now, The average player had an AVG of .275 
# and the standard deviation of the population of players was 0.027. 
# So we can see already that .450 would be quite an anomaly since it is over six standard deviations away from the mean.
# So is José lucky or is he the best batter seen in the last 50 years? Perhaps it’s a combination of both luck and talent. But how much of each? If we become convinced that he is lucky, we should trade him to a team that trusts the .450 observation and is maybe overestimating his potential.

# Hierarchical models considers 2 things - 
# Avg player performance usually & individual player performance in the recent times
# to predict what will happen at the end
# first it assigns a natural probability of success basis the performance of others
# i.e. the natural prob will be a normal distribution-  p ~ N(mean_population, sd_population) 
# Then it considers individual success rate as a normal distribution - y - N(mean_self, sd_self)
# Finally it says Expected p = mean_population + (1-B)*(mean_self - mean_population)
# and Expected SE or Expected SD = sqrt(1 / ((1/sd_self^2) + (1/sd_population^2)))
# Where, B = sd_self^2 / (sd_self^2 + sd_population^2)
# sd_self = sqrt(p_self * (1- p_self)/N)
# REMEMBER THAT MEAN IS ACTUALLY SUCCESS RATE SO P.
# So, lets look at in this case
n = 20
mean_self = 0.450
mean_population = 0.275
sd_self = sqrt(mean_self * (1-mean_self)/n)
sd_population = 0.027
B = sd_self^2 / (sd_self^2 + sd_population^2)
ExpectedP = mean_population + (1-B)*(mean_self - mean_population)
ExpectedSE = sqrt(1 / ((1/sd_self^2) + (1/sd_population^2)))
ExpectedP # 0.285
ExpectedSE # 0.026
ci = c(ExpectedP - qnorm(0.975)*ExpectedSE, ExpectedP + qnorm(0.975)*ExpectedSE)
ci

# Labs
# Define `Pr_1` as the probability of the first son dying of SIDS
Pr_1 <- 1/8500
# Define `Pr_2` as the probability of the second son dying of SIDS
Pr_2 <- 1/100
# Calculate the probability of both sons dying of SIDS. Print this value to the console.
Pr_1*Pr_2

# Define `Pr_B` as the probability of both sons dying of SIDS
Pr_B <- Pr_1*Pr_2

# Define Pr_A as the rate of mothers that are murderers
Pr_A <- 1/1000000

# Define Pr_BA as the probability that two children die without evidence of harm, given that their mother is a murderer
Pr_BA <- 0.50

# Define Pr_AB as the probability that a mother is a murderer, given that her two children died with no evidence of physical harm. Print this value to the console.
Pr_AB = Pr_BA * Pr_A / Pr_B
Pr_AB

# Labs
# Load the libraries and poll data
library(dplyr)
library(dslabs)
data(polls_us_election_2016)

# Create an object `polls` that contains the spread of predictions for each candidate in Florida during the last polling days
polls <- polls_us_election_2016 %>% 
  filter(state == "Florida" & enddate >= "2016-11-04" ) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# Examine the `polls` object using the `head` function
head(polls)

# Create an object called `results` that has two columns containing the 
# average spread (`avg`) and the standard error (`se`). Print the results to the console.
results = polls %>% summarize( avg = mean(spread), se = sd(spread)/sqrt(length(spread)))
results

# Define `mu` and `tau`
mu <- 0
tau <- 0.01

# Define a variable called `sigma` that contains the standard error in the object `results`
sigma = results$se

# Define a variable called `Y` that contains the average in the object `results`
Y = results$avg

# Define a variable `B` using `sigma` and `tau`. Print this value to the console.
B = sigma^2 / (sigma^2 + tau^2)

# Calculate the expected value of the posterior distribution
ev = mu + (1-B)*(Y-mu)
ev
# Compute the standard error of the posterior distribution. Print this value to the console.
se = sqrt(1/((1/sigma^2)+(1/tau^2))) # this is same as sd
se
# Construct the 95% credible interval. Save the lower and then the upper confidence interval to a variable called `ci`.
ci = c(ev - qnorm(0.975)*se, ev + qnorm(0.975)*se)
ci
  
# Using the `pnorm` function, calculate the probability that the 
# actual spread was less than 0 (in Trump's favor). Print this value to the console.
pnorm(0,mean=ev, sd = se)

# Define a variable `taus` as different values of tau
taus <- seq(0.005, 0.05, len = 100)

# Create a function called `p_calc` that generates `B` and 
# calculates the probability of the spread being less than 0
p_calc = function(tau){
  B = sigma^2 / (sigma^2 + tau^2)  
  ev = mu + (1-B)*(Y-mu)
  se = sqrt(1/((1/sigma^2)+(1/tau^2))) 
  return(pnorm(0,mean=ev, sd = se))
}

# Create a vector called `ps` by applying the function `p_calc` across values in `taus`
ps = sapply(taus, p_calc)
# Plot `taus` on the x-axis and `ps` on the y-axis  
plot(x = taus, y = ps)  
  
# ELECTION FORECASTING CASE STUDY ON HIERARCHICAL MODELING
library(tidyverse)
library(dslabs)
polls <- polls_us_election_2016 %>% 
  filter(state == "U.S." & enddate >= "2016-10-31" &
           (grade %in% c("A+","A","A-","B+") | is.na(grade))) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

one_poll_per_pollster <- polls %>% group_by(pollster) %>% 
  filter(enddate == max(enddate)) %>%
  ungroup()

results <- one_poll_per_pollster %>% 
  summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread))) %>% 
  mutate(start = avg - 1.96*se, end = avg + 1.96*se) 

mu <- 0 # spread is 0
tau <- 0.035
sigma <- results$se
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)

posterior_mean <- mu + (1-B)*(Y-mu)
posterior_se <- sqrt( 1/ (1/sigma^2 + 1/tau^2))

posterior_mean
posterior_se
posterior_mean + c(-1.96, 1.96)*posterior_se
1 - pnorm(0, posterior_mean, posterior_se) 
# 1 - probability of spread <0 i.e. probability of clinton winning

# Lab
# Load the libraries and data
library(dplyr)
library(dslabs)
data("polls_us_election_2016")

# Create a table called `polls` that filters by  state, date, and reports the spread
polls <- polls_us_election_2016 %>% 
  filter(state != "U.S." & enddate >= "2016-10-31") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# Create an object called `cis` that has the columns indicated in the instructions
cis <- polls %>% mutate(X_hat = (spread+1)/2, se = 2*sqrt(X_hat*(1-X_hat)/samplesize), 
                        lower = spread - qnorm(0.975)*se, upper = spread + qnorm(0.975)*se) %>%
  select(state, startdate, enddate, pollster, grade, spread, lower, upper)

# Add the actual results to the `cis` data set
add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)
ci_data <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")

# Create an object called `p_hits` that summarizes the proportion of confidence intervals that contain the actual value. Print this object to the console.
p_hits = ci_data %>% mutate(hits = ifelse(actual_spread>=lower & actual_spread<=upper,1,0)) %>% summarize(p_hits=mean(hits))
p_hits

# Create an object called `p_hits` that summarizes the proportion of hits for each pollster that has at least 5 polls.
p_hits = ci_data %>% mutate(hits = ifelse(actual_spread>=lower & actual_spread<=upper,1,0)) %>% group_by(pollster) %>% filter(n()>=5) %>% summarize(proportion_hits=mean(hits), n = n(), grade = grade[1]) %>% arrange(proportion_hits)
p_hits

# Create an object called `p_hits` that summarizes the proportion of hits for each state that has more than 5 polls.
p_hits = ci_data %>% mutate(hits = ifelse(actual_spread>=lower & actual_spread<=upper,1,0)) %>% group_by(state) %>% filter(n()>=5) %>% summarize(proportion_hits=mean(hits), n = n()) %>% arrange(proportion_hits)
p_hits

# Make a barplot of the proportion of hits for each state
p_hits %>% ggplot(aes(x=state, y=proportion_hits)) + geom_bar(stat = 'identity') + coord_flip()


cis = ci_data
errors = cis %>% mutate(error = spread - actual_spread, hit = ifelse(sign(spread) == sign(actual_spread), TRUE, FALSE))
tail(errors)
p_hits = errors %>% group_by(state) %>% filter(n()>=5) %>% summarize(proportion_hits=mean(hit), n = n()) %>% arrange(proportion_hits)
p_hits %>% ggplot(aes(x=state, y=proportion_hits)) + geom_bar(stat = 'identity') + coord_flip()
# Generate a histogram of the error
errors %>% ggplot(aes(x=error)) + geom_histogram(binwidth = 0.1)
# Calculate the median of the errors. Print this value to the console.
median.error = median(errors$error)
median.error

# Create a boxplot showing the errors by state for polls with grades B+ or higher
errors %>% filter(grade %in% c('A+', 'A', 'A-','B+') ) %>% arrange(error) %>% ggplot(aes(x=state, y= error)) + geom_boxplot() + geom_point()
# Create a boxplot showing the errors by state for states with at least 5 polls with grades B+ or higher
errors %>% filter(grade %in% c('A+', 'A', 'A-','B+') ) %>% group_by(state) %>%filter(n()>=5) %>% ungroup() %>% arrange(error) %>% ggplot(aes(x=state, y= error)) + geom_boxplot() + geom_point()

# T DISTRIBUTION
# IT IS THE NOMRAL DISTRIBUTION FOR SMALLER SAMPLE SIZES (N<=30)
# IT HAS 0 MEAN AND SD FOR N-1 
# SO Z SCORE = (SAMPLE MEAN - POPULATION MEAN )/(SAMPLE SD / SQRT(N))
# SAMPLE SD = SQRT(P*(1-P)/(N-1))
# IT BASICALLY ACCOUNTS FOR LARGER ERRORS POSSIBLE IN SMALL SAMPLE SIZE
# SO IT HAS FATTER TAILS, MEANING THERE ARE MORE POSSIBILITIES OF BEING AWAY FROM 0
# IN A NORMAL DISTRIBUTION Z SCORE FOR 95% CONFIDENCE IS 1.96
# IN A T DISTRIBUTION FOR HIGHER NUMBER OF OBS IT IS SAME
# BEYOND 30 OBS THE Z SCORE FOR T DISTRIBUTION DOESNT CHANGE MUCH,HENCE THE CUT OFF 30

x = 1:100
qt.value = function(x){
  return(qt(0.975,x))
}
qts = sapply(x,qt.value)
plot(x,qts)

x = 5:100
qts = sapply(x,qt.value)
plot(x,qts)

qt(0.975,29)
qt(0.975,30)
qnorm(0.975)
qt(0.975,31)
qt(0.975,40)
qt(0.975,50)
qt(0.975,1000)
qt(0.975,10000)


# Calculating 95% confidence intervals with the t-distribution
z <- qt(0.975, nrow(one_poll_per_pollster) - 1)
one_poll_per_pollster %>%
  summarize(avg = mean(spread), moe = z*sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg - moe, end = avg + moe)

# quantile from t-distribution versus normal distribution
qt(0.975, 14)    # 14 = nrow(one_poll_per_pollster) - 1
qnorm(0.975)

# Calculate the probability of seeing 
# t-distributed random variables being more than 2 in absolute value when 'df = 3'.
pt(2,3) # prob of being equal to lower than 2
1-pt(2,3) # prob of being more than 2
pt(-2,3) # prob of being less than -2
1-pt(2,3) + pt(-2,3) # prob of being more than abs(2)


# Generate a vector 'df' that contains a sequence of numbers from 3 to 50
df = seq(3,50)
# Make a function called 'pt_func' that calculates the probability that a value is more than |2| for any degrees of freedom 
pt_func = function(n){
  return(1-pt(2,n) + pt(-2,n))
}

# Generate a vector 'probs' that uses the `pt_func` function to calculate the probabilities
probs = sapply(df, pt_func)

# Plot 'df' on the x-axis and 'probs' on the y-axis
plot(df,probs)

# checking normal distribution for N less than 15 using monte carlo
# Load the neccessary libraries and data
library(dslabs)
library(dplyr)
data(heights)
# Use the sample code to generate 'x', a vector of male heights
x <- heights %>% filter(sex == "Male") %>%
  .$height
# Create variables for the mean height 'mu', the sample size 'N', and the number of times the simulation should run 'B'
mu <- mean(x)
N <- 15
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Generate a logical vector 'res' that contains the results of the simulations
res <- replicate(B, {
  X <- sample(x, N, replace=TRUE)
  interval <- mean(X) + c(-1,1)*qt(0.975,14)*sd(X)/sqrt(N)
  between(mu, interval[1], interval[2])
})
# Calculate the proportion of times the simulation produced values within the 95% confidence interval. Print this value to the console.
mean(res)

# ASSOCIATION TESTS
library(tidyverse)
library(dslabs)
data("research_funding_rates")
research_funding_rates %>% select(discipline, applications_total, 
                                  success_rates_total) %>% head()
totals <- research_funding_rates %>% 
  select(-discipline) %>% 
  summarize_all(sum) %>%
  summarize(yes_men = awards_men, 
            no_men = applications_men - awards_men, 
            yes_women = awards_women, 
            no_women = applications_women - awards_women) 
totals
totals %>% summarize(percent_men = yes_men/(yes_men+no_men),
                     percent_women = yes_women/(yes_women+no_women))
# But could this be due just to random variability? Here we learn how to perform inference for this type of data.
# Lady Tasting Tea
# 4 correct cups, 4 incorrect cups
# 4 choices from 8 cups can be made in following ways
nrow(combinations(8,4)) # 70
# 4 correct choices from 8 cups can be made in only 1 way 
# so the prob of all correct choices 
1/70
# 3 correct out of 4 choices from 8 cups
# 3 is correct out of 4 choices can happen in 
nrow(combinations(4,3)) # 4 ways
# 1 is incorrect out of 4 choices can happen in 
nrow(combinations(4,1)) # 4 ways
# so prob of having 3 correct out of 4 choices is 
4*4/70 
# simialrly prob of having 2 correct out of 4 choices is
nrow(combinations(4,2)) *nrow(combinations(4,2))/70
# THIS IS CALLED HYPERGEOMETRIC DISTRIBTION
# NUMBER OF WAYS YOU CAN MAKE K CORRECT CHOICES OUT OR N CHOICES IS 
# nrow(combinations(n,k))*nrow(combinations(n,n-k))

# FISHER EXACT TEST 
tab <- matrix(c(3,1,1,3),2,2)
rownames(tab)<-c("Poured Before","Poured After")
colnames(tab)<-c("Guessed before","Guessed after")
tab
# The function fisher.test performs the inference calculations above:
fisher.test(tab, alternative="greater")$p.value # 0.243

# IN FISHER'S TEST SCENARIO WE KNEW THAT THERE ARE 4 CORRECT & 4 INCORRECT CUPS
# BUT IN CASES WHERE WE HAVE APPLICANTS, SOME ARE MALE & FEMALE & SOME GOT FUNDED & NOT FUNDED
# I.E. 2 CATEGORICAL VARIABLES
# The first step is to create the two-by-two data table:

two_by_two <- data.frame(awarded = c("no", "yes"), 
                         men = c(totals$no_men, totals$yes_men),
                         women = c(totals$no_women, totals$yes_women))
two_by_two

# The general idea of the Chi-square test is to compare this two-by-two table to what you expect to see, which would be:
rate <- totals %>%
  summarize(percent_total = 
              (yes_men + yes_women)/
              (yes_men + no_men +yes_women + no_women)) %>%
  pull(percent_total)
rate

data.frame(awarded = c("no", "yes"), 
           men = (totals$no_men + totals$yes_men) * c(1 - rate, rate),
           women = (totals$no_women + totals$yes_women) * c(1 - rate, rate))

# We can see that more men than expected and fewer women than expected received funding. However, under the null hypothesis these observations are random variables. The Chi-square test tells us how likely it is to see a deviation this large or larger. 
chisq_test <- two_by_two %>% select(-awarded) %>% chisq.test()
chisq_test
chisq_test$p.value

# Odds ratio - ratio of odds between men & women 
# odds men = ratio of prob to get the funding to the prob of not getting the funding for men
odds_men <- with(two_by_two, (men[2]/sum(men)) / (men[1]/sum(men)))
odds_men
odds_women <- with(two_by_two, (women[2]/sum(women)) / (women[1]/sum(women)))
odds_women
odds_ratio <- odds_men / odds_women
odds_ratio
# Confidence intervals for the odds ratio
# if all numbers in two_by_two matrix is large enough
# than log(odds ratio) is normally distributed
# and se = sqrt(1/a + 1/b + 1/c + 1/d) # a,b,c,d are 4 numbers inside the two_by_two
log_or <- log(odds_men / odds_women)
se <- two_by_two %>% select(-awarded) %>%
  summarize(se = sqrt(sum(1/men) + sum(1/women))) %>%
  pull(se)
ci <- log_or + c(-1,1) * qnorm(0.975) * se
exp(ci) # convert it back to the odds ratio scale
# Note that 1 is not included in the confidence interval which must mean that the p-value is smaller than 0.05. We can confirm this using:
2*(1 - pnorm(log_or, 0, se))
 

# Small count correction
# Note that the log odds ratio is not defined if any of the cells of the two-by-two table is 0. This is because if  
# is either the log of 0 or has a 0 in the denominator. For this situation, it is common practice to avoid 0s by adding 0.5 to each cell. This is referred to as the Haldane–Anscombe correction and has been shown, both in practice and theory, to work well.

# Large samples, small p-values

# As mentioned earlier, reporting only p-values is not an appropriate way to report the results of data analysis. In scientific journals, for example, some studies seem to overemphasize p-values. Some of these studies have large sample sizes and report impressively small p-values. Yet when one looks closely at the results, we realize odds ratios are quite modest: barely bigger than 1. In this case the difference may not be practically significant or scientifically significant.

# Note that the relationship between odds ratio and p-value is not one-to-one. It depends on the sample size. So a very small p-value does not necessarily mean a very large odds ratio. Notice what happens to the p-value if we multiply our two-by-two table by 10, which does not change the odds ratio:
  
two_by_two %>% select(-awarded) %>%
mutate(men = men*10, women = women*10) %>%
chisq.test() %>% .$p.value

  
# Lab
head(errors)
# Generate an object called 'totals' that contains the numbers of good and bad predictions for polls rated A- and C-
two_by_two = errors %>% filter(grade %in% c('A-','C-')) %>% group_by(grade,hit) %>% summarize(n = n())
totals = two_by_two %>% spread(grade,n)
totals = as.data.frame(totals)
# Print the proportion of hits for grade A- polls to the console
totals$`A-`[2]/sum(totals$`A-`)
# Print the proportion of hits for grade C- polls to the console
totals$`C-`[2]/sum(totals$`C-`)

# Perform a chi-squared test on the hit data. Save the results as an object called 'chisq_test'.
chisq_test <- totals %>% select(-hit) %>% chisq.test()
chisq_test
chisq_test$p.value

# Generate a variable called `odds_C` that contains the odds of getting the prediction right for grade C- polls
odds_C <- with(totals, (`C-`[2]/sum(`C-`)) / (`C-`[1]/sum(`C-`)))
# Generate a variable called `odds_A` that contains the odds of getting the prediction right for grade A- polls
odds_A <- with(totals, (`A-`[2]/sum(`A-`)) / (`A-`[1]/sum(`A-`)))
# Calculate the odds ratio to determine how many times larger the odds ratio is for grade A- polls than grade C- polls
odds_ratio <- odds_A/odds_C
odds_ratio 
# INTERPRETATION - 
# P VALUE IS NOT SIGNIFICANT & ODDS RATIO IS CLOSE TO 1, WE CAN NOT SAY THEY ARE SIGNIFICANTLY DIFF







