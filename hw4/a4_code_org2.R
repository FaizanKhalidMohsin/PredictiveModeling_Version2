
# Load Packages
rm(list=ls())
library(randomForest)
library()
library(dplyr)

# Create Functions

# Function to split data into train and test set
split.data <- function(data, train.prop, set.seed=4583) {
  set.seed(set.seed)
  data <- data[sample(1:dim(data)[1]),]
  train.set <- data[1:as.integer(train.prop*dim(data)[1]),]
  test.set <- data[(as.integer(train.prop*dim(data)[1])+1):dim(data)[1],]
  return(list(train=train.set, test=test.set))
}


# Create function to obtain model MSE 
model.MSE = function(frml,data1,data2,msenull){
  model = lm(frml, data = data1)
  pred = predict(model, newdata= subset(data2, select = -Y)) 
  mse = mean((pred - data2$Y)^2)
  residual = mse/msenull
  return(residual)
}


# Load data 
load('assignment2_data.RData')
train.prop <- 0.50
train.split.seed <- 7294
outcome.var <- "Y"


# Step 1: Split data into train.set(1/2) and test.set(1/2)
split <- split.data(data, train.prop, set.seed=train.split.seed)
train.set <- split$train; test.set <- split$test
pred.var <- colnames(data)[-which(colnames(data) == outcome.var)]




t0 = Sys.time()


t1 = Sys.time() - t0
mtry0 = sqrt(dim(train.set)[2]-1)

vec = c( 0.10, 0.25, 0.50, 0.75, 1.0, 1.25)

#oob_error = rep(0,length(vec))

oob_error_df = data.frame(NULL)

for(i in 1:length(vec)){
  
  #i = 1
  print(paste0("i : ", i))
  # 
  # 
  fit = randomForest(Y~. , ntree = 1200, mtry0 = mtry0,  mtry = mtry0*vec[i], data = train.set)
  # oob_error[i] = fit$mse
  
  df_current = data.frame(i = i, mtry = mtry0*vec[i], oob_err = mean(fit$mse), vec = vec[i])
  
  oob_error_df = rbind(df_current, oob_error_df)
  
  
}

t_rf_loop_time = Sys.time() - t1

#str(fit)
plot(oob_error_df$vec, oob_error_df$oob_error)

mtry_opt = mtry0*vec[which.min(oob_error_df$oob_error)]

rf_opt_fit = randomForest(Y~. , sampsize = 1200,  mtry = mtry_opt, data = train.set)

y_rf_pred = predict(rf_opt_fit, newdata = test.set)

mse_rf = mean((test.y - y_rf_pred)^2)


#####################################################################################################
#                                          PART THREE                                                #
#                                       G B                                          #
#####################################################################################################


# Step 2: Split the train.set into training.set (2/3) and validation.set (1/3)
set.seed(8572)
n.fold <- 10
n.obs <- dim(train.set)[1]
cv.folds <- rep(1:n.fold, length=n.obs)[sample(1:n.obs, n.obs, replace=F)]

table(cv.folds)

# Fit a gradient boosting model using a package of your choice and try tree depths of 1 (stump), 2, 3 and 4 and 
# shrinkage values of 0.05, 0.10, 0.25 and 0.50. Determine the optimal number of trees and the best combination 
# of parameters using 10 fold cross-validation from the training set. Pick the best model using CV and estimate 
# the error on the test set. For each tree depth, plot the test set errors vs. number of trees for different 
# shrinkage values. 

shrinkage_vec = c(0.05, .1, .25, 0.5)
depth_vec = c(1, 2, 3, 4)

matrix(0, nrow= length(shrinkage_vec), ncol = depth_vec)

for (i in 1:length(shrinkage_vec)){
  
  
  for(j in 1:length(depth_vec)) {
    
      gbm.fit <- gbm(
          formula = Y ~ . ,
          distribution = "gaussian",
          data = ames_train,
          n.trees = 10000,
          interaction.depth = depth_vec[j],
          shrinkage = shrinkage_vec[i],
          cv.folds = 10,
          n.cores = NULL, # will use all cores by default
          verbose = FALSE)
      
      
    
  }
 
  
}


# print results
print(gbm.fit)


# Step 4: Obtain the Residual Error and Standard Coefficient Values 

RMSE.gb = train.set[1:10,]
#STCOEF.df = train.set[1:10,]

# Cross-validation FOR loop.
sleep_for_a_minute_uni <- function() {
  
for(i in 1:n.fold){ 
  
  # Specify train and validation chunks
  training.set = train.set[cv.folds != 1, ] #Set the training set to be cv.folds == 2 and 3
  validation.set = train.set[cv.folds == 1, ] #Set the validation set to be cv.folds = 1

  #train.chunk = training.set[cv.folds.cv != i,]
  #validation.chunk = training.set[cv.folds.cv == i,]
  
  # MSE null model
  # modlnull = lm(Y~1, data = train.chunk)
  # prednull = predict(modlnull, newdata = subset(validation.chunk, select = -Y))
  # mse_null = mean(prednull - validation.chunk$Y)
  
  # GBoosting Model. 
  
  
  # Create a for loop to perform linear regression with each individual variable
  # for(var in pred.var ){
  #   
  #   formula_var = paste("Y ~", var)
  #   frml = formula(formula_var)
  #   
  #   # MSE full model
  #   modl = lm(frml, data = train.chunk)
  #   
  #   # Get standardized coefficients
  #   st.codf <- abs((coefficients(modl)*sqrt(diag(vcov(modl))))[-1])
  #   
  #   # GET MSE
  #   pred = predict(modl, newdata = subset(validation.chunk, select = -Y))
  #   mse = mean(pred - validation.chunk$Y)
  #   
  #   # Residual vs Null 
  #   residual = mse/mse_null
  #   
  #   RMSE.df[i, var ] = residual
  #   STCOEF.df[i, var] = st.codf
  #   
  # }
  
}}

start_time_uni <- Sys.time()

sleep_for_a_minute_uni()

end_time_uni <- Sys.time()

comptime_uni <- end_time_uni - start_time_uni

# Obtain and sort means of cross-validated RMSE
RMSE.df.mean = as.vector(colMeans(RMSE.df))
names(RMSE.df.mean) = c(pred.var, "Y")
RMSE.df.mean.ordered = sort(RMSE.df.mean) 
RMSE.df.mean.ordered[1:100]

# Obtain and sort means of cross-validated Standardized Coefficients 
STCOEF.df.mean = as.vector(colMeans(STCOEF.df))
names(STCOEF.df.mean) = c(pred.var, "Y")
STCOEF.df.mean.ordered = sort(STCOEF.df.mean) 
STCOEF.df.mean.ordered[1:100]

# Create Shortlist of top 100 variables and a longer list with CV RE <0.925
shortlist = names(RMSE.df.mean.ordered[1:100])
longlist = names(RMSE.df.mean[RMSE.df.mean<0.925])

# Plot RMSE and Standardized Coefficients 
plot(RMSE.df.mean.ordered)
plot(STCOEF.df.mean.ordered[1:2722]) 



#####################################################################################################
#                                          PART THREE                                               #
#                                       MODEL BUILDING                                              #
#####################################################################################################


# Get shortlist and PCR list Formula
frml_shortlist = LRformula(shortlist[-1])
frml_pcrlist = LRformula(colnames(df.optimal.scores.T)[-length(colnames(df.optimal.scores.T))])

# Forward Selection on AIC main effects  
AIC.shortlist = AIC.function(frml_shortlist, training.set, 2)
AIC.longlist = AIC.function(frml_longlist, training.set, 3)
AIC.pcrlist = AIC.function(frml_pcrlist, df.optimal.scores.T, 2)

length(coef(AIC.shortlist));summary(AIC.shortlist)
length(coef(AIC.longlist));summary(AIC.longlist)
length(coef(AIC.pcrlist));summary(AIC.pcrlist) 

# Forward Selection on AIC main effects + interactions
AIC.shortlist.int = AIC.function(interaction.terms(AIC.shortlist), training.set, 2)
AIC.longlist.int = AIC.function(interaction.terms(AIC.longlist), training.set, 4)
AIC.pcrlist.int = AIC.function(interaction.terms(AIC.pcrlist), df.optimal.scores.T, 2)

length(coef(AIC.shortlist.int));summary(AIC.shortlist.int)
length(coef(AIC.longlist.int));summary(AIC.longlist.int) 
length(coef(AIC.pcrlist.int));summary(AIC.pcrlist.int) 

# Fitting Linear Regression using three lists and computing MSE on validation set

# MSE for null model for longlist and shortlist
null.model = lm(Y~1, data = training.set)
pred.null = predict(null.model, newdata= subset(validation.set, select = -Y))
mse.null = mean((pred.null - validation.set$Y)^2)

# Shortlist
residual.shortlist.initial = model.MSE(frml_shortlist,training.set,validation.set,mse.null)
residual.shortlist.AIC = model.MSE(LRformula(names(AIC.shortlist$coefficients)[-1]),
                                     training.set,validation.set,mse.null)
residual.shortlist.AIC.int = model.MSE(interaction.terms(AIC.shortlist),
                                         training.set,validation.set,mse.null)

# Longlist
residual.longlist.initial = model.MSE(frml_longlist,training.set,validation.set,mse.null)
residual.longlist.AIC = model.MSE(LRformula(names(AIC.longlist$coefficients)[-1]),
                                    training.set,validation.set,mse.null)
residual.longlist.AIC.int = model.MSE(interaction.terms(AIC.longlist),
                                        training.set,validation.set,mse.null)


# PCR list
residual.PCR.initial = model.MSE(frml_pcrlist,df.optimal.scores.T,df.optimal.scores.V,mse.null)
residual.PCR.AIC = model.MSE(LRformula(names(AIC.pcrlist$coefficients)[-1]),
                               df.optimal.scores.T,df.optimal.scores.V,mse.null)
residual.PCR.AIC.int = model.MSE(interaction.terms(AIC.pcrlist),
                                   df.optimal.scores.T,df.optimal.scores.V,mse.null)


# Residual error from shortlist, longlist and PCR list
residualval <- cbind(residual.shortlist.initial, residual.shortlist.AIC, residual.shortlist.AIC.int, 
                     residual.longlist.initial, residual.longlist.AIC, residual.longlist.AIC.int, 
                     residual.PCR.initial, residual.PCR.AIC, residual.PCR.AIC.int) 

print(residualval)


#####################################################################################################
#                                          PART FOUR                                               #
#                                       SELECT A FINAL MDOEL                                       #
#####################################################################################################



# Part 1: Retrain the whole model

# Shortlist
residual.shortlist.initial.final = model.MSE(frml_shortlist,train.set,test.set,mse.null)
residual.shortlist.AIC.final = model.MSE(LRformula(names(AIC.shortlist$coefficients)[-1]),train.set, test.set,mse.null)
residual.shortlist.AIC.int.final = model.MSE(interaction.terms(AIC.shortlist),
                                               train.set, test.set, mse.null)

# Longlist
residual.longlist.initial.final = model.MSE(frml_longlist,train.set,test.set,mse.null)
residual.longlist.AIC.final = model.MSE(LRformula(names(AIC.longlist$coefficients)[-1]),train.set,test.set,mse.null)
residual.longlist.AIC.int.final = model.MSE(interaction.terms(AIC.longlist),
                                              train.set,test.set,mse.null)

# PCR list
residual.PCR.initial.final = model.MSE(frml_pcrlist,df.optimal.scores.Train,df.optimal.scores.Test,mse.null)
residual.PCR.AIC.final = model.MSE(LRformula(names(AIC.pcrlist$coefficients)[-1]),df.optimal.scores.Train,df.optimal.scores.Test,mse.null)
residual.PCR.AIC.int.final = model.MSE(interaction.terms(AIC.pcrlist),
                                         df.optimal.scores.Train,df.optimal.scores.Test,mse.null)


# Residual error for the final model
residualval.final <- as.numeric(c(residual.shortlist.initial.final, residual.shortlist.AIC.final, residual.shortlist.AIC.int.final, 
                                  residual.longlist.initial.final, residual.longlist.AIC.final, residual.longlist.AIC.int.final, 
                                  residual.PCR.initial.final, residual.PCR.AIC.final, residual.PCR.AIC.int.final))

residualname.final <- c("SI", "SA", "SAIN", 
                        "LI", "LA", "LAIN", 
                        "PI", "PA", "PIN")

residual.final <- data.frame(residualname.final, residualval.final)

# Part 2: Comparison of the selected model with other models
plot(residual.final$residualval.final, xaxt = "n",  xlab = "Model", ylab= "RE", ylim=c(0,1.0), type = "o")
axis(1, at = seq_len(nrow(residual.final)), residualname.final)
