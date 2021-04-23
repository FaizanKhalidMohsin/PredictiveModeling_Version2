

# Load Packages

library(randomForest)
library(gbm)
library(dplyr)

# Function to split data into train and test set
split.data <- function(data, train.prop, set.seed=4583) {
  set.seed(set.seed)
  data <- data[sample(1:dim(data)[1]),]
  train.set <- data[1:as.integer(train.prop*dim(data)[1]),]
  test.set <- data[(as.integer(train.prop*dim(data)[1])+1):dim(data)[1],]
  return(list(train=train.set, test=test.set))
}

# Step 1: Split data into train.set(1/2) and test.set(1/2)
split <- split.data(data, train.prop, set.seed=train.split.seed)
train.set <- split$train; test.set <- split$test
pred.var <- colnames(data)[-which(colnames(data) == outcome.var)]


### RF

#reference time
t0 = Sys.time()


mtry0 = sqrt(dim(train.set)[2]-1)

vec = c( 0.10, 0.25, 0.50, 0.75, 1.0, 1.25)

#oob_error = rep(0,length(vec))

gb_error_df = data.frame(NULL)

for(i in 1:length(vec)){
  
  #i = 1
  print(paste0("i : ", i))
  # 
  # 
  fit = randomForest(Y~. , ntree = 1200, mtry = mtry0*vec[i], data = train.set)
  # oob_error[i] = fit$mse
  
  df_current = data.frame(i = i, mtry0 = mtry0,  mtry = mtry0*vec[i], oob_err = mean(fit$mse), vec = vec[i])
  
  gb_error_df = rbind(gb_error_df, df_current)
  
  print(gb_error_df)
  
}

t_rf_loop_time = Sys.time() - t0

#str(fit)
plot( x = gb_error_df$mtry, y =  gb_error_df$oob_err)

mtry_opt = mtry0*vec[which.min(gb_error_df$oob_err)]
print(paste("mtry vec optimum:" , gb_error_df$vec[which.min(gb_error_df$oob_err)]))
mtry_opt 

rf_opt_fit = randomForest(Y~. , sampsize = 1200,  mtry = mtry_opt, data = train.set) 

y_rf_pred = predict(rf_opt_fit, newdata = test.set) 

mse_rf = mean((test.y - y_rf_pred)^2) 


## GB

shrinkage_vec = c(0.05, .1, .25, 0.5)
depth_vec = c(1, 2, 3, 4)

gb_error=matrix(0, nrow= length(shrinkage_vec), ncol = depth_vec)

gb_error_df = data.frame(NULL)

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
    
    gb_error[i, j] = gbm.fit$valid.error
    
  }
  
  
}


# print results
#print(gbm.fit)




