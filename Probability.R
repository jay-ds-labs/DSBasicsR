#  Introducing paste() and expand.grid()
# joining strings with paste
number <- "Three"
suit <- "Hearts"
paste(number, suit)

# joining vectors element-wise with paste
paste(letters[1:5], as.character(1:5))

# generating combinations of 2 vectors with expand.grid
expand.grid(pants = c("blue", "black"), shirt = c("white", "grey", "plaid"))

# Generating a deck of cards
suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
numbers <- c("Ace", "Deuce", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
deck <- expand.grid(number = numbers, suit = suits)
deck <- paste(deck$number, deck$suit)

# probability of drawing a king
kings <- paste("King", suits)
mean(deck %in% kings)

# Permutations and combinations
# combination - choose p items from n items where order doesnt matter
# permutation - choose p items from n items where order matters
# install.packages('gtools')
library(gtools)
permutations(5,2)    # ways to choose 2 numbers in order from 1:5
permutations(3,2)    # order matters
combinations(3,2)    # order does not matter

all_phone_numbers <- permutations(10, 7, v = 0:9)
n <- nrow(all_phone_numbers)
all_phone_numbers <- permutations(10, 7, v = 0:9, repeats.allowed = T)
n <- nrow(all_phone_numbers)
index <- sample(n, 5)
all_phone_numbers[index,]

# probability of drawing a second king given that one king is drawn
hands <- permutations(52,2, v = deck)
hands
first_card <- hands[,1]
second_card <- hands[,2]
sum(first_card %in% kings)

sum(first_card %in% kings & second_card %in% kings) / sum(first_card %in% kings)

mean(first_card %in% kings & second_card %in% kings) # same as (1/13)*(3/51)

# Probability of a natural 21 in blackjack
aces <- paste("Ace", suits)
facecard <- c("King", "Queen", "Jack", "Ten")
facecard <- expand.grid(number = facecard, suit = suits)
facecard <- paste(facecard$number, facecard$suit)

hands <- combinations(52, 2, v=deck) # all possible hands

# probability of a natural 21 given that the ace is listed first in `combinations`
mean(hands[,1] %in% aces & hands[,2] %in% facecard)

# probability of a natural 21 checking for both ace first and ace second
mean((hands[,1] %in% aces & hands[,2] %in% facecard)|(hands[,2] %in% aces & hands[,1] %in% facecard))



# Monte Carlo simulation of natural 21 in blackjack
# Note that your exact values will differ because the process is random and the seed is not set.

# code for one hand of blackjack
hand <- sample(deck, 2)
hand

# code for B=10,000 hands of blackjack
B <- 10000
results <- replicate(B, {
  hand <- sample(deck, 2)
  (hand[1] %in% aces & hand[2] %in% facecard) | (hand[2] %in% aces & hand[1] %in% facecard)
})
mean(results)  # very close to the probability that we got earlier


# The birthday problem
# We can compute the probability of shared birthdays in a group of people by modeling birthdays as random draws from the numbers 1 through 365. We can then use this sampling model of birthdays to run a Monte Carlo simulation to estimate the probability of shared birthdays.
# checking for duplicated bdays in one 50 person group
n <- 50
bdays <- sample(1:365, n, replace = TRUE)    # generate n random birthdays
any(duplicated(bdays))    # check if any birthdays are duplicated

# Monte Carlo simulation with B=10000 replicates
B <- 10000
results <- replicate(B, {    # returns vector of B logical values
  bdays <- sample(1:365, n, replace = TRUE)
  any(duplicated(bdays))
})
mean(results) 

# function to calculate probability of shared bdays across n people
compute_prob <- function(n, B = 10000) {
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace = TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}

n <- seq(1, 60)
prob <- sapply(n, compute_prob)    # element-wise application of compute_prob to n
plot(n, prob)

# Computing birthday problem probabilities with sapply
# function for computing exact probability of shared birthdays for any n
exact_prob <- function(n){
  prob_unique <- seq(365, 365-n+1)/365   # vector of fractions for mult. rule
  1 - prod(prob_unique)    # calculate prob of no shared birthdays and subtract from 1
}

# applying function element-wise to vector of n values
eprob <- sapply(n, exact_prob)

# plotting Monte Carlo results and exact probabilities on same graph
plot(n, prob)    # plot Monte Carlo results
lines(n, eprob, col = "red")    # add line for exact prob

# Estimating a practical value of B
# This code runs Monte Carlo simulations to estimate the probability of shared birthdays using several B values and plots the results. When B is large enough that the estimated probability stays stable, then we have selected a useful value of B.

B <- 10^seq(1, 5, len = 100)    # defines vector of many B values
compute_prob <- function(B, n = 22){    # function to run Monte Carlo simulation with each B
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace = TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}

prob <- sapply(B, compute_prob)    # apply compute_prob to many values of B
plot(log10(B), prob, type = "l") # after B = 10^4 the graph is stable. We can see that it is a good number of iterations

# Lab
cyan <- 3
magenta <- 5
yellow <- 7
p_yellow <- yellow / (cyan + magenta + yellow)
p_yellow^6

p_no6 = 1-1/6
p_no6^6

simulated_games <- sample(c("lose","win"), 4, replace = TRUE, prob = c(0.6, 0.4))
B <- 10000
set.seed(1)
celtic_wins <- replicate(B, {
  simulated_games <- sample(c("lose","win"), 4, replace = TRUE, prob = c(0.6, 0.4))
  any(simulated_games == "win")
})
mean(celtic_wins)

# Monte Carlo simulations can be used to simulate random outcomes, which makes them useful when exploring ambiguous or less intuitive problems like the Monty Hall problem.
# In the Monty Hall problem, contestants choose one of three doors that may contain a prize. Then, one of the doors that was not chosen by the contestant and does not contain a prize is revealed. The contestant can then choose whether to stick with the original choice or switch to the remaining unopened door.
# Although it may seem intuitively like the contestant has a 1 in 2 chance of winning regardless of whether they stick or switch, Monte Carlo simulations demonstrate that the actual probability of winning is 1 in 3 with the stick strategy and 2 in 3 with the switch strategy.

# Monte Carlo simulation of stick strategy
B = 10000
set.seed(1)
stick = replicate(B, {
  doors = c(1:3)
  selected.door = sample(doors, 1)
  reward.door = sample(doors, 1)
  selected.door == reward.door
})
mean(stick)

# Monte Carlo simulation of change door strategy
B <- 10000
set.seed(1)
non.stick <- replicate(B, {
  doors <- c(1:3)
  selected.door <- sample(doors, 1)
  remaining.doors <- doors[!(doors == selected.door)]
  reward.door <- sample(doors, 1)
  non.reward.door <- doors[!(doors == reward.door)]
  open.door.choice <- remaining.doors[!(remaining.doors == reward.door)]
  if(length(open.door.choice) == 1){
    open.door = open.door.choice
  }else{
    open.door <- sample(open.door.choice,1)
  }
  switch.door <- doors[!(doors==open.door | doors==selected.door)]
  switch.door == reward.door
})
mean(non.stick)


# The Cavs and the Warriors Lab
# Assign a variable 'n' as the number of remaining games.
n <- 6

# Assign a variable `outcomes` as a vector of possible game outcomes, where 0 indicates a loss and 1 indicates a win for the Cavs.
outcomes <- c(0,1)

# Assign a variable `l` to a list of all possible outcomes in all remaining games. Use the `rep` function on `list(outcomes)` to create list of length `n`. 
l <- rep(list(outcomes), n)

# Create a data frame named 'possibilities' that contains all combinations of possible outcomes for the remaining games.
possibilities <- expand.grid(l)

# Create a vector named 'results' that indicates whether each row in the data frame 'possibilities' contains enough wins for the Cavs to win the series.
results <- rowSums(possibilities)>=4

# Calculate the proportion of 'results' in which the Cavs win the series. Print the outcome to the console.
mean(results)

# Solving the same with MonteCarlo
B = 10000
set.seed(1)
results <- replicate(B, {
  outcomes = c(1,0)
  sum(sample(outcomes,6, replace = T, prob = c(0.5,0.5)))>=4
})
mean(results)



# Two teams, A and B, are playing a seven series game series. Team A is better than team B and has a p>0.5 chance of winning each game.
# Use the function sapply to compute the probability, call it Pr of winning for p <- seq(0.5, 0.95, 0.025).
# Then plot the result plot(p, Pr).

# Let's assign the variable 'p' as the vector of probabilities that team A will win.
p <- seq(0.5, 0.95, 0.025)

# Given a value 'p', the probability of winning the series for the underdog team B can be computed with the following function based on a Monte Carlo simulation:
prob_win <- function(p){
  B <- 10000
  result <- replicate(B, {
    b_win <- sample(c(1,0), 7, replace = TRUE, prob = c(1-p, p))
    sum(b_win)>=4
  })
  mean(result)
}

# Apply the 'prob_win' function across the vector of probabilities that team A will win to determine the probability that team B will win. Call this object 'Pr'.
Pr <- sapply(p,prob_win)

# Plot the probability 'p' on the x-axis and 'Pr' on the y-axis.
plot(p,Pr)

# doing the same thing as above with N (best of N games) changing and prob of A to win as 0.75
N = seq(1,25,2)
prob_win <- function(N, p=0.75){
  B <- 10000
  result <- replicate(B, {
    b_win <- sample(c(1,0), N, replace = TRUE, prob = c(1-p, p))
    sum(b_win)>=(N+1)/2
  })
  mean(result)
}

Pr <- sapply(N,prob_win)
plot(N,Pr)


# Labs
library(gtools)
nrow(permutations(8,3))
nrow(permutations(3,3))
6/336

# Monte carlo simulation can all the winners be from Jamaica out of 8 runners
runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
runners <- c(1:8)
B = 10000
set.seed(1)
result = replicate(B, {
  gold = sample(runners, 1)
  left = runners[!(runners == gold)]
  silver = sample(left,1)
  left2 = left[!(left == silver)]
  bronze = sample(left2,1)
  gold <= 3 & silver <=3 & bronze <= 3
})
mean(result)

####
nrow(combinations(6,1)) * nrow(combinations(6,2)) * nrow(combinations(2,1))
nrow(combinations(6,1)) * nrow(combinations(6,2)) * nrow(combinations(3,1))
nrow(combinations(6,1)) * nrow(combinations(6,3)) * nrow(combinations(3,1))

n <- c(1:12)
func = function(n){
  return(nrow(combinations(n,1)) * nrow(combinations(6,2)) * nrow(combinations(3,1))
)
}
result = sapply(n,func)
names(result) = n
result

n <- c(2:12)
func = function(n){
  return(nrow(combinations(6,1)) * nrow(combinations(n,2)) * nrow(combinations(3,1))
  )
}
result = sapply(n,func)
names(result) = n
result

# Lab Easophagel cancer

head(esoph)
nrow(esoph)
all_cases = sum(esoph$ncases)
all_cases
all_controls = sum(esoph$ncontrols)
all_controls

install.packages('tidyverse')

library('tidyverse')
summary <- esoph %>% group_by(alcgp) %>% summarize(sum_cases = sum(ncases), sum_control = sum(ncontrols)) %>% mutate(prob = sum_cases/(sum_control+ sum_cases))
View(summary)

a = esoph[esoph$alcgp == '120+',]
View(a)
sum(a$ncases)/sum(a$ncontrols)
unique(esoph$alcgp)

summary <- esoph %>% group_by(tobgp) %>% summarize(sum_cases = sum(ncases), sum_control = sum(ncontrols)) %>% mutate(prob = sum_cases/(sum_control+ sum_cases))
View(summary)

1- (78/all_cases)
1 - (525/all_controls)
45/all_cases
31/all_cases


summary <- esoph %>% mutate(grp = paste(alcgp,tobgp)) %>% group_by(grp) %>% summarize(sum_cases = sum(ncases), sum_control = sum(ncontrols)) %>% mutate(prob = sum_cases/(sum_control+ sum_cases))
View(summary)
10/all_cases

45+31-10
66/all_cases

67/all_controls
(45/all_cases) / (67/all_controls)

82/all_controls
13/all_controls

67+82-13
136/all_controls

(66/all_cases) / (136/all_controls)

# Cumulative distribution function
# Define x as male heights from the dslabs heights dataset:
  
library(tidyverse)
# install.packages('dslabs')
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)
# Given a vector x, we can define a function for computing the CDF of x using:
  
F <- function(a) mean(x <= a)
1 - F(70)    # probability of male taller than 70 inches

# Using pnorm() to calculate probabilities
# pnorm gives the cumm probability to have a value less than a particular value (70.5)
# in a distribution x, assuming it is normally distributed with mean(x) and sd(x)
pnorm(70.5, mean(x), sd(x))  # prob to have a height less than 70.5
1 - pnorm(70.5, mean(x), sd(x)) # prob to have a heigh more than or equal to 70.5

# Discretization and the normal approximation
# plot distribution of exact heights in data
plot(prop.table(table(x)), xlab = "a = Height in inches", ylab = "Pr(x = a)")

# probabilities in actual data over length 1 ranges containing an integer
mean(x <= 68.5) - mean(x <= 67.5)
mean(x <= 69.5) - mean(x <= 68.5)
mean(x <= 70.5) - mean(x <= 69.5)

# probabilities in normal approximation match well
pnorm(68.5, mean(x), sd(x)) - pnorm(67.5, mean(x), sd(x))
pnorm(69.5, mean(x), sd(x)) - pnorm(68.5, mean(x), sd(x))
pnorm(70.5, mean(x), sd(x)) - pnorm(69.5, mean(x), sd(x))

# probabilities in actual data over other ranges don't match normal approx as well
mean(x <= 70.9) - mean(x <= 70.1)
pnorm(70.9, mean(x), sd(x)) - pnorm(70.1, mean(x), sd(x))


# qnorm provides the reverse of pnorm
qnorm(0.99,69,3) # value of 99%ile of a distribution with mean 69 & sd 3


# Plotting the probability density for the normal distribution
library(tidyverse)
x <- seq(-4, 4, length = 100)
data.frame(x, f = dnorm(x)) %>%
  ggplot(aes(x, f)) +
  geom_line()

# Understanding the diff between pnorm & dnorm
# dnorm - gives the prob of a particular guy selected randomly to have a particular height
# pnorm - gives the cumm prob of all the guys to have height less than a particular height
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)
data.frame(x, f = dnorm(x, mean(x), sd(x))) %>%
  ggplot(aes(x, f)) +
  geom_line()


data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)
data.frame(x, density.f = dnorm(x, mean(x), sd(x)), prob.f = pnorm(x, mean(x), sd(x))) %>%
  ggplot(aes(x, density.f)) +geom_line(color='blue') + geom_line(aes(x,prob.f), color='red') + geom_text(aes(x=80, y = 1.1, label = 'prob (pnorm'), size = 3, color = 'red') + geom_text(aes(x=80, y = 0.1, label = 'density (dnorm'), size = 3, color = 'blue') + ggtitle('Difference between dnorm & pnorm') + ylab('Probability') + xlab('x - Height')

# Generating normally distributed random numbers
# define x as male heights from dslabs data
library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)

# generate simulated height data using normal distribution - both datasets should have n observations
n <- length(x)
avg <- mean(x)
s <- sd(x)
simulated_heights <- rnorm(n, avg, s)

# plot distribution of simulated_heights
data.frame(simulated_heights = simulated_heights) %>%
  ggplot(aes(simulated_heights)) +
  geom_histogram(color="black", binwidth = 2)

# Monte Carlo simulation of tallest person over 7 feet
B <- 10000
tallest <- replicate(B, {
  simulated_data <- rnorm(800, avg, s)    # generate 800 normally distributed random heights
  max(simulated_data)    # determine the tallest height
})
mean(tallest >= 7*12) 

# You may encounter other continuous distributions (Student t, chi-squared, exponential, gamma, beta, etc.).
# R provides functions for density (d), quantile (q), probability distribution (p) and random number generation (r) for many of these distributions.
# Each distribution has a matching abbreviation (for example, norm() or t()) that is paired with the related function abbreviations (d, p, q, r) to create appropriate functions.
# For example, use rt() to generate random numbers for a Monte Carlo simulation using the Student t distribution.


# Lab
female_avg <- 64
female_sd <- 3
pnorm(5*12, female_avg, female_sd)
1 - pnorm(6*12, female_avg, female_sd)

female_avg <- 64*2.54
female_sd <- 3*2.54
pnorm(67*2.54, female_avg, female_sd) - pnorm(61*2.54, female_avg, female_sd)

female_avg <- 64
female_sd <- 3
pnorm(67, female_avg, female_sd) - pnorm(61, female_avg, female_sd)

# Lab - The distribution of IQ scores is approximately normally distributed. The average is 100 and the standard deviation is 15. Suppose you want to know the distribution of the person with the highest IQ in your school district, where 10,000 people are born each year.
# Generate 10,000 IQ scores 1,000 times using a Monte Carlo simulation. Make a histogram of the highest IQ scores.
# The variable `B` specifies the number of times we want the simulation to run.
B <- 1000
set.seed(1)
highestIQ <- replicate(B, {
  max(rnorm(10000,100,15))
})
hist(highestIQ)

# Lab
set.seed(16,sample.kind = 'Rounding')
act_scores = rnorm(10000, 20.9, 5.7)
mean(act_scores)
sd(act_scores)
sum(act_scores>=36)
mean(act_scores>30)
mean(act_scores<=10)

x = seq(1,36)
f_x = dnorm(x,20.9,5.7)
plot(x,f_x)
z = scale(act_scores)
mean(z>2)
min(act_scores[z>2])
qnorm(0.975,20.9,5.7)

fun = function(n){
  return(dnorm(n,20.9,5.7))
}
y = sapply(x, fun)
plot(x,y)

x <- seq(1,36)
data.frame(x, density.f = dnorm(x, mean(x), sd(x)), prob.f = pnorm(x, mean(x), sd(x))) %>%
  ggplot(aes(x, density.f)) +geom_line(color='blue') + geom_line(aes(x,prob.f), color='red')

df = data.frame(x, density.f = dnorm(x, mean(x), sd(x)), prob.f = pnorm(x, mean(x), sd(x)))
View(df)

qnorm(0.95,20.9,5.7)

p <- seq(0.01, 0.99, 0.01)
sample_quantiles <- quantile(act_scores,p)
sample_quantiles
theoretical_quantiles <- qnorm(p,20.9,5.7)
plot(theoretical_quantiles,sample_quantiles)

# Modeling a random variable
# define random variable x to be 1 if blue, 0 otherwise
beads <- rep(c("red", "blue"), times = c(2, 3))
x <- ifelse(sample(beads, 1) == "blue", 1, 0)

# demonstrate that the random variable is different every time
ifelse(sample(beads, 1) == "blue", 1, 0)
ifelse(sample(beads, 1) == "blue", 1, 0)
ifelse(sample(beads, 1) == "blue", 1, 0)

# Monte Carlo simulation: Chance of casino losing money on roulette
# We build a sampling model for the random variable  S  that represents the casino's total winnings. 

# sampling model 1: define urn, then sample
color <- rep(c("Black", "Red", "Green"), c(18, 18, 2)) # define the urn for the sampling model
n <- 1000
X <- sample(ifelse(color == "Red", -1, 1), n, replace = TRUE)
X[1:10]

# sampling model 2: define urn inside sample function by noting probabilities
x <- sample(c(-1, 1), n, replace = TRUE, prob = c(18/38, 20/38))    # 1000 independent draws
S <- sum(x)    # total winnings = sum of draws
S

# We use the sampling model to run a Monte Carlo simulation and use the results to estimate the probability of the casino losing money.
n <- 1000    # number of roulette players
B <- 10000    # number of Monte Carlo experiments
S <- replicate(B, {
  X <- sample(c(-1,1), n, replace = TRUE, prob = c(9/19, 10/19))    # simulate 1000 spins
  sum(X)    # determine total profit
})

mean(S < 0)  # probability of the casino losing money

# We can plot a histogram of the observed values of S as well as the normal density curve based on the mean and standard deviation of S.

library(tidyverse)
s <- seq(min(S), max(S), length = 100)    # sequence of 100 values across range of S
normal_density <- data.frame(s = s, f = dnorm(s, mean(S), sd(S))) # generate normal density for S
data.frame (S = S) %>%    # make data frame of S for histogram
  ggplot(aes(S, ..density..)) +
  geom_histogram(color = "black", binwidth = 10) +
  ylab("Probability") +
  geom_line(data = normal_density, mapping = aes(s, f), color = "blue")


# CLT
# The Central Limit Theorem (CLT) says that the distribution of the sum of a random variable is approximated by a normal distribution.

# The expected value of a random variable,  E[X]=μ , is the average of the values in the urn. This represents the expectation of one draw. 
# The standard error of one draw of a random variable is the standard deviation of the values in the urn.
# The expected value of the sum of draws is the number of draws times the expected value of the random variable. 
# The standard error of the sum of independent draws of a random variable is the square root of the number of draws times the standard deviation of the urn. 
# Equations
# These equations apply to the case where there are only two outcomes,  a  and  b  with proportions  p  and  1−p  respectively. The general principles above also apply to random variables with more than two outcomes.

# Expected value of a random variable: 
#  ap+b(1−p) 
# Expected value of the sum of n draws of a random variable: 
#  n×(ap+b(1−p)) 
# Standard deviation of an urn with two values: |b-a| * sqrt(p*(1-p))
# Standard error of the sum of n draws of a random variable: sqrt(n) * |b-a| * sqrt(p*(1-p))

# Lab American Roulette probabilities
# An American roulette wheel has 18 red, 18 black, and 2 green pockets. Each red and black pocket is associated with a number from 1 to 36. The two remaining green slots feature "0" and "00". Players place bets on which pocket they think a ball will land in after the wheel is spun. Players can bet on a specific number (0, 00, 1-36) or color (red, black, or green).

# What are the chances that the ball lands in a green pocket?
green <- 2
black <- 18
red <- 18
p_green = green/(green + black + red)
p_green

# In American roulette, the payout for winning on green is $17. This means that if you bet $1 and it lands on green, you get $17 as a prize.
# Create a model to predict your winnings from betting on green one time.
green <- 2
black <- 18
red <- 18

set.seed(1)
X = ifelse(sample(c('g','b','r'),1,prob=c(2/38,18/38,18/38)) == 'g',17,-1)

p_green = green/(green + black + red)
p_not_green = 1-p_green
expected_outcome = (17*p_green) + (-1*p_not_green)
expected_outcome

# Now, compute the standard error of that random variable, which represents a single outcome after one spin of the roulette wheel.
standard_err = abs(17-(-1))*sqrt(p_green * p_not_green)
standard_err

# Now create a random variable S that sums your winnings after betting on green 1,000 times.
set.seed(1)
n = 1000
X = ifelse(sample(c('g','b','r'),n,replace = T,prob=c(2/38,18/38,18/38)) == 'g',17,-1)
S = sum(X)
S

# In the previous exercise, you generated a vector of random outcomes, S, after betting on green 1,000 times.
# What is the expected value of S?
expected_outcome_n = n*expected_outcome
expected_outcome_n

# You generated the expected value of S, the outcomes of 1,000 bets that the ball lands in the green pocket, in the previous exercise.
# What is the standard error of S?
standard_err = abs(17-(-1))*sqrt(p_green * p_not_green)
standard_err_n = sqrt(n) * standard_err
standard_err_n

# Lab what is the prob of winning in green in 100 bets
# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- 2 / 38

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1-p_green

# Define the number of bets using the variable 'n'
n <- 100

# Calculate 'avg', the expected outcome of 100 spins if you win $17 when the ball lands on green and you lose $1 when the ball doesn't land on green
avg <- n * (17*p_green + -1*p_not_green)

# Compute 'se', the standard error of the sum of 100 outcomes
se <- sqrt(n) * (17 - -1)*sqrt(p_green*p_not_green)

# Using the expected value 'avg' and standard error 'se', compute the probability that you win money betting on green 100 times.
1-pnorm(0,avg,se)


# Lab
# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- 2 / 38

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1-p_green

# Define the number of bets using the variable 'n'
n <- 100

# The variable `B` specifies the number of times we want the simulation to run. Let's run the Monte Carlo simulation 10,000 times.
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling.
set.seed(1)

# Create an object called `S` that replicates the sample code for `B` iterations and sums the outcomes.
S = replicate(B,{
  s = sample(c(17,-1),n,replace = T, prob = c(p_green, p_not_green))
  sum(s)
  })

# Compute the average value for 'S'
mean(S)
sd(S)
              
mean(S>0)


# SAT Lab
p = 1/5
expected_val = p*1+ (-0.25*(1-p))
std_err = abs(1-(-0.25))*sqrt(p*(1-p))
std_err_all = std_err*sqrt(44)
std_err_all

set.seed(21, sample.kind = 'Rounding')
S = replicate(10000,{
  s = sample(c(1,-0.25),44,replace=T, prob = c(0.2,0.8))
  sum(s)
})

mean(S>=8)

0.25*44

p <- seq(0.25, 0.95, 0.05)
fun = function(p){
  S = replicate(10000,{
    s = sample(c(1,0),44,replace=T, prob = c(p,1-p))
    sum(s)
  })
  
  m = mean(S>=35)
  return(m>=0.8)
}
output = sapply(p, fun)
names(output) = p
output

# Case Study Special Roulette
# A casino offers a House Special bet on roulette, which is a bet on five pockets (00, 0, 1, 2, 3) out of 38 total pockets. The bet pays out 6 to 1. In other words, a losing bet yields -$1 and a successful bet yields $6. A gambler wants to know the chance of losing money if he places 500 bets on the roulette House Special.
p.w = 5/38
p.l = 1-p.w
eo = 6*p.w + (-1)*p.l
eo
se = abs(6-(-1))*sqrt(p.w*p.l)
se
eo*500
se/sqrt(500)
s = sample(c(6,-1),500,replace=T,prob=c(p.w,p.l))
sum(s)
500*eo
se*sqrt(500)
pnorm(0,500*eo,se*sqrt(500))

# BIG SHORT
# Interest rates for loans are set using the probability of loan defaults to calculate a rate that minimizes the probability of losing money.
# We can define the outcome of loans as a random variable. We can also define the sum of outcomes of many loans as a random variable.
# The Central Limit Theorem can be applied to fit a normal distribution to the sum of profits over many loans. We can use properties of the normal distribution to calculate the interest rate needed to ensure a certain probability of losing money for a given probability of default.

# Interest rate sampling model
n <- 1000
loss_per_foreclosure <- -200000
p <- 0.02
defaults <- sample( c(0,1), n, prob=c(1-p, p), replace = TRUE)
sum(defaults * loss_per_foreclosure)

# Interest rate Monte Carlo simulation
B <- 10000
losses <- replicate(B, {
  defaults <- sample( c(0,1), n, prob=c(1-p, p), replace = TRUE) 
  sum(defaults * loss_per_foreclosure)
})

# Plotting expected losses
library(tidyverse)
data.frame(losses_in_millions = losses/10^6) %>%
  ggplot(aes(losses_in_millions)) +
  geom_histogram(binwidth = 0.6, col = "black")

# Expected value and standard error of the sum of 1,000 loans
n*(p*loss_per_foreclosure + (1-p)*0)    # expected value 
sqrt(n)*abs(loss_per_foreclosure)*sqrt(p*(1-p))    # standard error

# Calculating interest rates for expected value of 0
# We can calculate the amount  x  to add to each loan so that the expected value is 0 using the equation  lp+x(1−p)=0 . Note that this equation is the definition of expected value given a loss per foreclosure  l  with foreclosure probability  p  and profit  x  if there is no foreclosure (probability  1−p ).
x = - loss_per_foreclosure*p/(1-p)
x
# On a $180,000 loan, this equals an interest rate of:
x/180000

# Calculating interest rate for 1% probability of losing money
l <- loss_per_foreclosure
z <- qnorm(0.01)
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))
x/180000    # interest rate
loss_per_foreclosure*p + x*(1-p)    # expected value of the profit per loan
n*(loss_per_foreclosure*p + x*(1-p)) # expected value of the profit over n loans

# Monte Carlo simulation for 1% probability of losing money
B <- 100000
profit <- replicate(B, {
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-p, p), replace = TRUE) 
  sum(draws)
})
mean(profit)    # expected value of the profit over n loans
mean(profit<0)    # probability of losing money

# Expected value with higher default rate and interest rate
p <- .04
loss_per_foreclosure <- -200000
r <- 0.05
x <- r*180000
loss_per_foreclosure*p + x*(1-p)

# Calculating number of loans for desired probability of losing money
# The number of loans required is:
z <- qnorm(0.01)
l <- loss_per_foreclosure
n <- ceiling((z^2*(x-l)^2*p*(1-p))/(l*p + x*(1-p))^2)
n    # number of loans required
n*(loss_per_foreclosure*p + x * (1-p))    # expected profit over n loans

# Monte Carlo simulation with known default probability
# This Monte Carlo simulation estimates the expected profit given a known probability of default  p=0.04 . Note that your results will differ from the video because the seed is not set.

B <- 10000
p <- 0.04
x <- 0.05 * 180000
profit <- replicate(B, {
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-p, p), replace = TRUE) 
  sum(draws)
})
mean(profit)

# Monte Carlo simulation with unknown default probability
# This Monte Carlo simulation estimates the expected profit given an unknown probability of default  0.03≤p≤0.05 , modeling the situation where an event changes the probability of default for all borrowers simultaneously. Note that your results will differ from the video because the seed is not set.

p <- 0.04
x <- 0.05*180000
profit <- replicate(B, {
  new_p <- 0.04 + sample(seq(-0.01, 0.01, length = 100), 1)
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-new_p, new_p), replace = TRUE)
  sum(draws)
})
mean(profit)    # expected profit
mean(profit < 0)    # probability of losing money
mean(profit < -10000000)    # probability of losing over $10 million

