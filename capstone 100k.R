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
required.packages.data.manipulation <- c('data.table','tidyverse','DT', 'Hmisc','pander')
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
# PART 3 - Data Preparation for Model Building
# 1. Creating train & test datasets from edx dataset for model parameter estimation
#    so that Validation set is used only for final model validation      
###################################################################################

# Creating train_set & test_set on edxB (larger dataset, used for final model train & validation using selected modelling technique)
test.and.train.big <- create.test.and.train.from.edx(edxB)
train_setB <- test.and.train.big$train_set                # will be used in PART 5
test_setB <- test.and.train.big$test_set                  # will be used in PART 5
rm(test.and.train.big)

# Creating train_set & test_set on edxS (smaller dataset, used for selecting modeling technique with lowest RMSE )
test.and.train.small <- create.test.and.train.from.edx(edxS)
train_setS <- test.and.train.small$train_set              # will be used in PART 4
test_setS <- test.and.train.small$test_set                # will be used in PART 4
rm(test.and.train.small)

###################################################################################
# PART 3 - Data Preparation for Model Building
# 2. Create a blank dataset called rmse.results to store the RMSE of all  modeling 
#    techniques applied on smaller dataset for final modeling technique selection
###################################################################################

rmse.results <- data.frame(SNo = integer(), ModelType = character(), Algorithm = character(), RMSE = double(), stringsAsFactors=F)

###################################################################################
# PART 4 - Model Selection on 10K Movie Dataset
# Type 1 - Random Prediction
# Model 1 - Mean Rating
###################################################################################

model.param.meanRating <- mean(edxS$rating)
predicted.rating <- rep(model.param.meanRating,nrow(validationS))
model.rmse <- rmse(validationS$rating,predicted.rating)
rmse.results[nrow(rmse.results)+1,] <- c(1, 'Random prediction', 'Mean rating', model.rmse)
view.rmse()


###################################################################################
# PART 4 - Model Selection on 10K Movie Dataset
# Type 1 - Random Prediction
# Model 2 - Random Rating with equal probability
###################################################################################

# 0.1 rating is allocated to each possible 10 ratings between 0.5 to 5
model.param.rating.prob <- rep(1/length(unique(edxS$rating)),10)
predicted.rating <- sample(seq(0.5,5,0.5), size = nrow(validationS), replace = T, prob = model.param.rating.prob)
model.rmse <- rmse(validationS$rating,predicted.rating)
rmse.results[nrow(rmse.results)+1,] <- c(2, 'Random prediction', 'Random rating with 0.1 prob', model.rmse)
view.rmse()

# 6 Types of models to be built
# Type 1 - Random Prediction
#   Mean Rating
#   Random Rating - Same prob 
#   Random Rating - Existing prob
# Type 2 - Linear Models
#   Movie Effect
#   Movie & User Effect
#   Movie, User & Genre Effect
#   Movie, User, Genre & Time Effect
#   Movie Effect with Regularization
#   Movie & User Effect with Regularization
#   Movie, User & Genre Effect with Regularization
#   Movie, User, Genre & Time Effect with Regularization
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















