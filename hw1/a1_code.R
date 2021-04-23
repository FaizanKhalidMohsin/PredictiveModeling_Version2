

#rm(list=ls())

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
data.idx <-1
miss.prop.idx <- 1
miss.type.idx <- 1

# fOR LOOP
mse_data = data.frame(mse = c(0,0,0), 
                      data = c(1,1,1) , 
                      miss.type.idx = c(1,1,1), 
                      miss.prop.idx = c(1,1,1), 
                      imp_method = c("mice", "mean", "rf"))


# for (data.idx in 1:3) {
#   
#   for (miss.prop.idx in 1:2){
#     
#     for (miss.type.idx in 1:2) {
#       
#       
#       # # # # # # Start code
#       
#       
#       # # # # # # END code
#       
#       mse_data[data.idx, miss.prop.idx, miss.type.idx, 1 ] = mse_test_final_mice_data1_per1_type1
#       mse_data[data.idx, miss.prop.idx, miss.type.idx, 2 ] = mse_test_final_mean_data1_per1_type1
#       mse_data[data.idx, miss.prop.idx, miss.type.idx, 3 ] = mse_test_final_rf_data1_per1_type1 
#       
#     }
#     
#   }
#   
# }


data <- data.list[[data.idx]]
miss.prop <- miss.prop.list[miss.prop.idx]
miss.type <- miss.type.list[miss.type.idx]

# # # # # # Start code

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
#mice.data <- as.data.frame(train.set.miss)

mice.data <- train.set.miss
micedatasets = mice(mice.data,m=5,maxit=50,meth='pmm',seed=500)
completedData1 <- complete(micedatasets,1)
completedData2 <- complete(micedatasets,2)
completedData3 <- complete(micedatasets,3)
completedData4 <- complete(micedatasets,4)
completedData5 <- complete(micedatasets,5)

mice_dataset_final_data1_per1_type1 = (completedData1 + completedData2 + completedData3 + completedData4 + completedData5) / 5
# imputation is finished

#MSE for Null Model using test data.
modelNull = lm(Y~ 1, data = mice_dataset_final_data1_per1_type1) # full model trained on the trainset
predicted_Y_usingTestdata = predict(modelNull, newdata = test[,-"Y"] ) #  mse using test data step 1
predict = predicted_Y_usingTestdata
mse_null = mean((predicted - test$Y)^2) #  mse using test data step 2.
#mse_test_final_mice_data1_per1_type1 = mse_test / mse_null #= 1/5 if 5/1



#MSE for Full model using full model trained on the trainset and mse using test data
modelFull = lm(Y~ ., data = mice_dataset_final_data1_per1_type1) # full model trained on the trainset
predicted_Y_usingTestdata = predict(modelFull, newdata = test[,-"Y"] ) #  mse using test data step 1
predict = predicted_Y_usingTestdata
mse_test = mean((predicted - test$Y)^2) #  mse using test data step 2.

mse_test_final_mice_data1_per1_type1 = mse_test / mse_null #= 1/5 if 5/1

#mse<1 #your model has predictive power compared to the null model
#mse>1 #no predictive power


#####################
#Impute with the mean
mean.data <- train.set.miss

for(i in 1:ncol(mean.data)){
  mean.data[is.na(mean.data[,i]), i] <- mean(mean.data[,i], na.rm = TRUE)
}

mean_dataset_final_data1_per1_type1 = mean.data


#MSE for Full model using full model trained on the trainset and mse using test data
modelFull = lm(Y~ ., data = mean_dataset_final_data1_per1_type1) # full model trained on the trainset
predicted_Y_usingTestdata = predict(modelFull, newdata = test[,-"Y"] ) #  mse using test data step 1
predict = predicted_Y_usingTestdata
mse_test = mean((predicted - test$Y)^2) #  mse using test data step 2.
mse_test_final_mean_data1_per1_type1 = mse_test / mse_null #= 1/5 if 5/1

#########################
#Random Forest Imputation
rf.data <- train.set.miss
iris.imp <- missForest(rf.data, ntree = 500 , maxiter = 5, seed = 500)
#check imputed values
rf_dataset_final_data1_per1_type1 = iris.imp$ximp
#IMputation finished


#MSE for Full model using full model trained on the trainset and mse using test data
modelFull = lm(Y~ ., data = rf_dataset_final_data1_per1_type1) # full model trained on the trainset
predicted_Y_usingTestdata = predict(modelFull, newdata = test[,-"Y"] ) #  mse using test data step 1
predict = predicted_Y_usingTestdata
mse_test = mean((predicted - test$Y)^2) #  mse using test data step 2.
mse_test_final_rf_data1_per1_type1 = mse_test / mse_null #= 1/5 if 5/1

#mse<1 #your model has predictive power
#mse>1 #no predictive power


# # # # # # END code




