
rm(list=ls())

#library(doParallel)
#library(doMC)
#library(tools)
library(pls)
library(MASS)
library(glmnet)

split.data <- function(data, train.prop, set.seed=4583) {
	set.seed(set.seed)
	data <- data[sample(1:dim(data)[1]),]
	train.set <- data[1:as.integer(train.prop*dim(data)[1]),]
	test.set <- data[(as.integer(train.prop*dim(data)[1])+1):dim(data)[1],]
	return(list(train=train.set, test=test.set))
}

load('assignment2_data.RData')

train.prop <- 0.50
train.split.seed <- 7294
outcome.var <- "Y"

split <- split.data(data, train.prop, set.seed=train.split.seed)
train.set <- split$train; test.set <- split$test
pred.var <- colnames(data)[-which(colnames(data) == outcome.var)]

set.seed(8572)

n.fold <- 3
n.obs <- dim(train.set)[1]
cv.folds <- rep(1:n.fold, length=n.obs)[sample(1:n.obs, n.obs, replace=F)]

