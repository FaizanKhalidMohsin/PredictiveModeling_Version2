
# Load Packages
rm(list=ls())
library(pls)
library(MASS)
library(glmnet)
library(rpart)

# Create Functions

# Function to split data into train and test set
split.data <- function(data, train.prop, set.seed=4583) {
  set.seed(set.seed)
  data <- data[sample(1:dim(data)[1]),]
  train.set <- data[1:as.integer(train.prop*dim(data)[1]),]
  test.set <- data[(as.integer(train.prop*dim(data)[1])+1):dim(data)[1],]
  return(list(train=train.set, test=test.set))
}

# Function to create LR formula using a elements from a list
LRformula = function(lst){
  frm = formula(paste("Y ~", paste(lst, collapse="+")))
  return(frm)
}

# Function to change scores into dataframe structure and add Y variable
scoretodf = function(frmula, df, error){
  optimal.mod = pcr(frml_longlist, data = df, ncomp=components[which.min(error)],
                    scale = TRUE, validation = "CV")
  optimal.scores = as.data.frame(optimal.mod$scores[,])
  optimal.scores$Y = df$Y
  colnames(optimal.scores) <- gsub(" ", "_", colnames(optimal.scores))
  return(optimal.scores)
}

# Function to perform Forward Selection
AIC.function = function(frmla, df, k){
  
  intercept <- formula('Y~1')
  intercept.model <- lm(Y ~ 1, data=df)
  AICmodel <- stepAIC(intercept.model, scope=list(lower=intercept, upper=frmla),
                      direction='forward', k=k)
  return(AICmodel)
}  

# Function to create formula including the first-order interaction with selected main effects
interaction.terms = function(AICmodel){
  vars = names(AICmodel$coefficients)[2:length(AICmodel$coefficients)]
  interactions = combn(vars, 2, FUN = paste, collapse="*")
  vars.interaction = c(vars,interactions)
  frml_interaction <- paste("Y~", paste(vars.interaction, collapse = "+"))
  return(frml_interaction)
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
load('C:/Users/di_ni/OneDrive/Desktop/CHL5212/Assignments/A22/assignment2_data.RData')
train.prop <- 0.50
train.split.seed <- 7294
outcome.var <- "Y"

#####################################################################################################
#                                          PART ONE                                                 #
#                                       VARIABLE SELECTION                                          #
#####################################################################################################


# Step 1: Split data into train.set(1/2) and test.set(1/2)
split <- split.data(data, train.prop, set.seed=train.split.seed)
train.set <- split$train; test.set <- split$test
pred.var <- colnames(data)[-which(colnames(data) == outcome.var)]


# Step 2: Split the train.set into training.set (2/3) and validation.set (1/3)
set.seed(8572)
n.fold <- 3 
n.obs <- dim(train.set)[1]
cv.folds <- rep(1:n.fold, length=n.obs)[sample(1:n.obs, n.obs, replace=F)]

training.set = train.set[cv.folds != 1, ] #Set the training set to be cv.folds == 2 and 3
validation.set = train.set[cv.folds == 1, ] #Set the validation set to be cv.folds = 1


# Step 3: CV Step: Split training set into 5 chunks
#n.fold.cv <- 5
#n.obs.cv <- dim(training.set)[1]
#cv.folds.cv <- rep(1:n.fold.cv, length=n.obs.cv)[sample(1:n.obs.cv, n.obs.cv, replace=F)]
#table(cv.folds.cv)

# Step 4: Obtain the Residual Error and Standard Coefficient Values 

RMSE.df = train.set[1:5,]
STCOEF.df = train.set[1:5,]

# GLMNET to do lasso regression, (lasso is the default regerssion).
?cv.glmnet
?glmnet

# Lasso
cv.glmnet(x  = subset(training.set, select = -Y), y = training.set$Y)

# Lasso + inter


# Tree
rpart(Y~., data=training.set, method="anova",control=rpart.control(minsplit=80, cp=0.01) )

?rpart
# Model building: 

## Trees and Regularized Regression


### Regularized Regression 4X4: Three models x 4: variables lists + all variables. 

# Do 10 fold CV. 

?cv.glmnet

lasso_regression_main_effect_errors = rep(0, 4)
mix_regression_main_effect_errors = rep(0, 4)
ridge_regression_main_effect_errors = rep(0, 4)
tree_regression_main_effect_errors = rep(0, 4)


for(i in 1:4) {
# Model 1 Lasso: alpha= 1
lasso = cv.glmnet(x  =  subset(training.set, select = -Y), y = training.set$Y)

lasso_pred = predict(lasso, newdata = subset(validation.set, select = -Y))
lasso_valid_mse = mean((lasso_pred - validation.set$Y )^2)

lasso_regression_main_effect_errors[i] = lasso_valid_mse

# Final result to store all the lasso_regression_main_effect_errors = lasso_valid_mse_vec = c(list1_mse, list2_mse, list3_mse, list4_mse)

# Model 2 Lasso + Ridge: alpha= 0.5
mixed = cv.glmnet(x  =  subset(training.set, select = -Y), y = training.set$Y, alpha = 0.5, nfold = 10)

mixed_valid_mse = mean((lasso_pred - validation.set$Y )^2)

mixed_regression_main_effect_errors[i] = mixed_valid_mse



# Model 3 Ridge: alpha= 0
ridge = cv.glmnet(x  =  subset(training.set, select = -Y), y = training.set$Y, alpha = 0)



# Tree
tree_fit = rpart(Y~., data=training.set, method="anova",control=rpart.control(minsplit=80, cp=0.01) )
## prune the tree
pfit<- prune(tree_fit, cp=  tree_fit$cptable[which.min(tree_fit$cptable[,"xerror"]),"CP"])

tree_pred = predict(pfit, newdata = subset(validation.set, select = -Y))

tree_valid_mse = mean((tree_pred - validation.set$Y )^2)

tree_regression_main_effect_errors[i] = tree_valid_mse

}

# plot the pruned tree
plot(pfit, uniform=TRUE,
     main="Pruned Classification Tree for Kyphosis")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)
post(pfit, file = "c:/ptree.ps",
     title = "Pruned Classification Tree for Kyphosis")


# Cross-validation FOR loop.
sleep_for_a_minute_uni <- function() {
  
for(i in 1:n.fold.cv){ 
  
  # Specify train and validation chunks
  train.chunk = training.set[cv.folds.cv != i,]
  validation.chunk = training.set[cv.folds.cv == i,]
  
  # MSE null model
  modlnull = lm(Y~1, data = train.chunk)
  prednull = predict(modlnull, newdata = subset(validation.chunk, select = -Y))
  mse_null = mean(prednull - validation.chunk$Y)
  
  # Create a for loop to perform linear regression with each individual variable
  for(var in pred.var ){
    
    formula_var = paste("Y ~", var)
    frml = formula(formula_var)
    
    # MSE full model
    modl = lm(frml, data = train.chunk)
    
    # Get standardized coefficients
    st.codf <- abs((coefficients(modl)*sqrt(diag(vcov(modl))))[-1])
    
    # GET MSE
    pred = predict(modl, newdata = subset(validation.chunk, select = -Y))
    mse = mean(pred - validation.chunk$Y)
    
    # Residual vs Null 
    residual = mse/mse_null
    
    RMSE.df[i, var ] = residual
    STCOEF.df[i, var] = st.codf
    
  }
  
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
#                                          PART TWO                                                 #
#                                 DIMENSIONALITY REDUCTION                                          #
#####################################################################################################


# Specify components
components = c(5, 6, 7, 8, 10, 11, 13 , 16)

# Create a formula using all variables included in Longlist
frml_longlist = LRformula(longlist[-length(longlist)])

MSE.training.pcr = matrix(data=0, nrow=5, ncol=8)

# FOR loop to obtain MSE values for Training and Validation Set 
for(i in 1:n.fold.cv){
  
  train.chunk = training.set[cv.folds.cv != i,]
  test.chunk = training.set[cv.folds.cv == i,]
  
  for(j in 1:length(components)){
    
    # PCR regression 
    mod.training = pcr(Y~., data = train.chunk, ncomp =components[j],  scale = TRUE, validation = "CV")
    pcr.training = as.data.frame(mod.training$scores[,])
    pcr.training$Y = train.chunk$Y
    
    lm.null = lm(Y~1, data = pcr.training)
    lm.mod = lm(Y~., data = pcr.training)
    
    pcr.testing = as.data.frame(predict(mod.training, newdata = test.chunk[,!(names(test.chunk) %in% c("Y"))],type="scores"))
    
    pred = predict(lm.mod, newdata = pcr.testing)
    predicting.null = predict(lm.null, newdata = pcr.testing)
    
    mse = mean((pred - test.chunk$Y)^2)
    mse.predicting.null = mean((predicting.null - test.chunk$Y)^2)
    
    MSE.training.pcr[i,j] = mse/mse.predicting.null
  }
}

print(MSE.training.pcr)

MSE.training.pcr = as.vector(colMeans(MSE.training.pcr))

# Get best scores from the best PCR model:
df.optimal.scores.T = scoretodf(frml_longlist,training.set,MSE.training.pcr)
df.optimal.scores.V = scoretodf(frml_longlist,validation.set,MSE.training.pcr)
df.optimal.scores.Train = scoretodf(frml_longlist,train.set,MSE.training.pcr)
df.optimal.scores.Test = scoretodf(frml_longlist,test.set,MSE.training.pcr)


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
