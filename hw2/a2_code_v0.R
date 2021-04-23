
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

head(data$Y)


# Step 1: Split data into train.set(1/2) and test.set(1/2)
split <- split.data(data, train.prop, set.seed=train.split.seed)
train.set <- split$train; test.set <- split$test
pred.var <- colnames(data)[-which(colnames(data) == outcome.var)]
head(train.set$Y)


set.seed(8572)

# Step2: Split the train.set into training.set (2/3) and validation.set (1/3)
n.fold <- 3 
n.obs <- dim(train.set)[1]
cv.folds <- rep(1:n.fold, length=n.obs)[sample(1:n.obs, n.obs, replace=F)]
table(cv.folds)
print(cv.folds)


training.set = train.set[cv.folds != 1, ] #Set the training set to be cv.folds == 2 and 3
validation.set = train.set[cv.folds == 1, ] #Set the validation set to be cv.folds = 1

head(training.set$Y)
head(validation.set$Y)


# Step 3: CV Step: Split training set into 5 chunks
n.fold.cv <- 5
n.obs.cv <- dim(training.set)[1]
cv.folds.cv <- rep(1:n.fold.cv, length=n.obs.cv)[sample(1:n.obs.cv, n.obs.cv, replace=F)]
table(cv.folds.cv)

#Create the RMSE.df

#RMSE.df = NULL

RMSE.df = train.set[1:5,]
dim(RMSE.df)
STCOEF.df = train.set[1:5,]
dim(STCOEF.df)

# Doing the cross-validation step as a for loop.

for(i in 1:n.fold.cv){ # This is cross validation loop
  
  train.chunk = training.set[cv.folds.cv != i,]
  validation.chunk = training.set[cv.folds.cv == i,]
  
  # MSE null model
  modlnull = lm(Y~1, data = train.chunk)
  prednull = predict(modlnull, newdata = subset(validation.chunk, select = -Y))
  mse_null = mean(prednull - validation.chunk$Y)
  
  for(var in pred.var ){
    
    #var = "Var1"
    formula_var = paste("Y ~", var)
    frml = formula(formula_var)
    #class(frml)
    
    # MSE full model
    modl = lm(frml, data = train.chunk)
    
    # GET STANDIZED cOEFFICIENTS
    #st.codf = coef(modl)[2]
    
    # GET MSE
    pred = predict(modl, newdata = subset(validation.chunk, select = -Y))
    mse = mean(pred - validation.chunk$Y)
    
  
    # Residualv1
    residual = mse/mse_null
    
    RMSE.df[i, var ] = residual
    
  }

}

RMSE.df.mean = as.vector(colmeans(RMSE.df))
RMSE.df.mean.ordered = sort(RMSE.df.mean)
RMSE.df.mean.ordered = RMSE.df.mean[-order(RMSE.df.mean)]

top100var = names(head(RMSE.df.mean.ordered, 100))

shortlist = names(RMSE.df.mean.ordered[1:100])
longlist = names(RMSE.df.mean[RMSE.df.mean<0.925])


########### QUEST 2 PCR


# mETHOD 0

components = c( 5, 6, 7, 8, 10, 11, 13 , 16)



# Method 1

paste("asdf", "asdfasd", "asdfas", sep = "+")

formula_var_pcr = paste("Y ~", top100var, sep = "+")
frml_pcr = formula(formula_var)


RMSE.df.pcr.meth1 = rep(0, length(components))

for(j in 1:length(components)){
  #modl = pcr.cv(frml_pcr, k= components[j],  data = yourdata, cv.k = 5)
  
  mod = pcr(frml_pcr, data = yourdata,ncomp =  components[j],  scale = TRUE, validation = "CV")
  mse = modl$MSEP
  RMSE.df.pcr.meth1[j] = mse
}

pcr_pred = predict(pcr_fit2, x_test, ncomp=7)
mean((pcr_pred-y_test)^2)
# Generate scores from the best PCR model for training and validation sets.

# Method 2

RMSE.df.pcr = matrix(0, nrow = 5, ncol = length(components))

# Doing the cross-validation step as a for loop.

for(i in 1:n.fold.cv){ # Cross validation loop
  
  train.chunk = training.set[cv.folds.cv != i,]
  validation.chunk = training.set[cv.folds.cv == i,]
  
  # MSE null model
  modlnull = lm(Y~1, data = train.chunk)
  prednull = predict(modlnull, newdata = subset(validation.chunk, select = -Y))
  mse_null = mean(prednull - validation.chunk$Y)
  
  for(j in 1:length(components) ){
  
    
    modl = pcr.cv(frml_pcr, k=components[j] , data=train.chunk)
    
    # GET MSE
    pred = predict(modl, newdata = subset(validation.chunk, select = -Y))
    mse = mean(pred - validation.chunk$Y)
    
    # Residualv1
    residual = mse/mse_null
    RMSE.df.pcr[i, j ] = residual
    
  }
  
}



# Model building

varall
varset1
varpcr

modelAIC.ALL.null = lm.AIC(Y~1, data = training.set)

# ALL variables
modelAIC.ALL = lm.AIC(Y~varall, data = training.set)
#top100
modelAIC.SET1vAR = lm.AIC(Y~varset1, data = training.set)
#Short list
modelAIC.Shortlist = lm.AIC(Y~shortlist, data = training.set)
#PCR
modelAIC.VARPCR = lm.AIC(Y~varsetpcr, data = training.set)

pred.AIC.ALL = pred(modelAIC.ALL, newdata= subset(validation.set, select = -Y))
pred.AIC.null = pred(modelAIC.ALL.null, newdata= subset(validation.set, select = -Y))
mse.AIC.ALL = mean(pred.AIC.ALL - validation.set$Y)
mse.AIC.ALL.null = mean(pred.AIC.null - validation.set$Y)

residual.AIC.ALL = mse.AIC.ALL/mse.AIC.ALL.null
residual.modelAIC.SET1vAR
residaul.modelAIC.VARPCR




