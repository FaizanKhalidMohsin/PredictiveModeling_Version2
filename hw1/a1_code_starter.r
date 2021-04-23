

rm(list=ls())

#Load the required libraries
library(missForest)
library(mice)
library(doParallel)

#Load the helper functios and data frame
source('a1_functions_train.r')
load('a1_simulated_data.RData')


#specify the data in imputation parameters
train.prop <- 0.25
outcome.var <- "Y"
rf.iter <- 5
mice.iter <- 20

#specify the proportion and type of missingness
miss.prop.list <- c(0.15, 0.40)
miss.type.list <- c('mcar', 'mnar') #mcar = missing completely at random; mnar = missing not at random

#set the data, missingness type and missingness proportion for an iteration
data.idx <- 1
miss.prop.idx <- 1
miss.type.idx <- 1

data <- data.list[[data.idx]]
miss.prop <- miss.prop.list[miss.prop.idx]
miss.type <- miss.type.list[miss.type.idx]

#specify the training and test sets
train.set <- data[1:as.integer(dim(data)[1]*train.prop),]; test.set <- data[(as.integer(dim(data)[1]*train.prop)+1):dim(data)[1],]
pred.var <- colnames(data)[-which(colnames(data) == outcome.var)] #list of predictors

##########################
## Set the missing data ##
train.set.miss <- train.set
set.seed(4587)
train.set.miss[,pred.var] <- miss.set.df(train.set.miss[,pred.var], miss.prop, miss.type)

################
#MICE Imputation
mice.data <- train.set.miss

#####################
#Impute with the mean
mean.data <- train.set.miss

#########################
#Random Forest Imputation
rf.data <- train.set.miss




