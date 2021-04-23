

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


# Load data 
load('assignment2_data.RData')
train.prop <- 0.50
train.split.seed <- 7294
outcome.var <- "Y"

# Step 1: Split data into train.set(1/2) and test.set(1/2)
split <- split.data(data, train.prop, set.seed=train.split.seed)
train.set <- split$train; test.set <- split$test
pred.var <- colnames(data)[-which(colnames(data) == outcome.var)]



#reference time
t0 = Sys.time()


mtry0 = sqrt(dim(train.set)[2]-1)

vec = c( 0.10, 0.25, 0.50, 0.75, 1.0, 1.25)

#oob_error = rep(0,length(vec))

rf_error_df = data.frame(NULL)

for(i in 1:length(vec)){
  
  #i = 1
  print(paste0("i : ", i))
  # 
  # 
  fit = randomForest(Y~. , ntree = 1200, mtry = mtry0*vec[i], data = train.set)
  # oob_error[i] = fit$mse
  
  df_current = data.frame(i = i, mtry0 = mtry0,  mtry = mtry0*vec[i], oob_err = mean(fit$mse), vec = vec[i])
  
  rf_error_df = rbind(rf_error_df, df_current)
  
  print(rf_error_df)
  
}

t_rf_loop_time = Sys.time() - t0

#str(fit)
plot( x = rf_error_df$mtry, y =  rf_error_df$oob_err)

mtry_opt = mtry0*vec[which.min(rf_error_df$oob_err)]
print(paste("mtry vec optimum:" , rf_error_df$vec[which.min(rf_error_df$oob_err)]))
mtry_opt 

rf_opt_fit = randomForest(Y~. , sampsize = 1200,  mtry = mtry_opt, data = train.set) 

y_rf_pred = predict(rf_opt_fit, newdata = test.set) 

mse_rf = mean((test.set$y - y_rf_pred)^2) 


# end of rf
#########################################################




#### First part of the gbm question


shrinkage_vec = c(0.05, .1, .25, 0.5)
depth_vec = c(1, 2, 3, 4)

gb_error=matrix(0, nrow= length(shrinkage_vec), ncol = length(depth_vec))
gb_numb_trees=matrix(0, nrow= length(shrinkage_vec), ncol = length(depth_vec))

opt.tree <- matrix(0, nrow= length(shrinkage_vec), ncol = length(depth_vec)) #take the minimum         
opt.tree.mse <- matrix(0, nrow= length(shrinkage_vec), ncol = length(depth_vec))


gbm.fit1 <- gbm(
  formula = Y ~ . ,
  distribution = "gaussian",
  data = train.set,
  n.trees = 10000,
  interaction.depth = 1,
  shrinkage = 0.05,
  cv.folds = 5,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE)

which.min(gbm.fit1$cv.error) #take the minimum         
min(gbm.fit1$cv.error)

which.min(gbm.fit1$valid.error)
min(gbm.fit$valid.error) 

gbm.perf(gbm.fit1,  method = "cv")

##train.set = #split it into 10 chuncks as in his code

##for( i.fold in 1:10) {  

for (i in 1:length(shrinkage_vec)){
  
  print("i")
  print(i)
  
  for(j in 1:length(depth_vec)) {
    
    # i = 1
    # j = 1
    
    print("j")
    print(j)
    gbm.fit <- gbm(
      formula = Y ~ . ,
      distribution = "gaussian",
      data = train.set, #train.set[i.fold] 
      n.trees = 100,
      interaction.depth = depth_vec[j],
      shrinkage = shrinkage_vec[i],
      cv.folds = 5,
      n.cores = NULL, # will use all cores by default
      verbose = FALSE)
    
    
    print("gbm.fit$valid.error")
    print(gbm.fit$valid.error)
    
    opt.tree[i,j] <- which.min(gbm.fit$cv.error) #take the minimum         
    opt.tree.mse[i,j] <- min(gbm.fit$cv.error)
    
    #gb_numb_trees[i, j] = which.min(gbm.fit$valid.error)
    #gb_error[i, j] = min(gbm.fit$valid.error)#  #gbm.fit$valid.error
  }
  
}


#mse.ifold = predict(gbm.fit, newdata = train.set[-i.fold])
#}

# print results
#print(gbm.fit)
index_2d = which.min(gb_error, arr.ind = T)

opt.shrinkage = shrinkage_vec[index_2d[1]]
opt.depth = depth_vec[index_2d[2]]
opt.trees = gb_numb_trees[index_2d]


gbm.bestfit = gbm.fit(formula = Y ~ . ,
                      distribution = "gaussian",
                      data = train.set,
                      n.trees = opt.trees,
                      interaction.depth = opt.depth,
                      shrinkage = opt.shrinkage,
                      cv.folds = 10,
                      n.cores = NULL, # will use all cores by default
                      verbose = FALSE)

gbm.perf(gbm.bestfit, method = "cv")

gbm.pred.y = predict(gbm.bestfit, newdata = subset(test.set, select = -Y))
gbm.test.mse = mean((gbm.pred.y - test.set$Y)^2)



######  last part of the gbm question



shrinkage_vec = c(0.05, .1, .25, 0.5)
depth_vec = c(1, 2, 3, 4)

numb.trees = c(50, 100, 150)
gbm.fit.list = list()
gbm.test.mse.df = matrix(0, nrow= length(shrinkage_vec), ncol = length(depth_vec))
gbm.test.mse.df = data.frame(NULL)


#count = 1

for (i in 1:length(shrinkage_vec)){
  
  print("i")
  print(i)
  
  for(j in 1:length(depth_vec)) {
    
    # i = 1
    # j = 1
    
    print("j")
    print(j)
    
    
    for(k in 1:length(numb.trees)) {
      
      # i = 1
      # j = 1
      # k = 1
      
      print("k")
      print(k)
      
      
      gbm.fit <- gbm(
        formula = Y ~ . ,
        distribution = "gaussian",
        data = train.set, #train.set[i.fold] 
        n.trees = numb.trees[k] ,
        interaction.depth = depth_vec[j],
        shrinkage = shrinkage_vec[i],
        cv.folds = 5,
        n.cores = NULL, # will use all cores by default
        verbose = FALSE)
      
      
      gbm.pred.y = predict(gbm.fit, newdata = subset(test.set, select = -Y))
      gbm.test.mse = mean((gbm.pred.y - test.set$Y)^2)
      
      
      gbm.test.mse.df.current = data.frame(shrinkage =  shrinkage_vec[i], tree_depth =  depth_vec[j], 
                                           numb_trees = numb.trees[k] , gbm_test_mse = gbm.test.mse,
                                           opt_tree_from_train_set = which.min(gbm.fit$cv.error), 
                                           opt_tree_mse_from_train_set = min(gbm.fit$cv.error))
      
      gbm.test.mse.df = rbind(gbm.test.mse.df, gbm.test.mse.df.current)
      
      
    }
    
  }
  
}


write.csv(gbm.test.mse.df, "gbm_results.csv", row.names = F)




