## Problem 4: Classify 2's and 3's 
## Compare the classification performance of linear regression and k-nearest 
## neighbor classification on the zipcode data.


####### Reading Data from zip file #######
train_zipcode_data <- read.table("zip.train.gz")
train_zipcode_data <- as.data.frame(train_zipcode_data) # We do not have cbind and rbind
train_data_2_3_only <- train_zipcode_data[train_zipcode_data[,1] %in% c(2,3), ]
test_zipcode_data <-  read.table('zip.test.gz')
test_zipcode_data <- as.data.frame(test_zipcode_data) # We do not have cbind and rbind
test_data_2_3_only <- test_zipcode_data[test_zipcode_data[,1] %in% c(2,3), ]

###############################################################################################

###### Library ###################################################################################
library(MASS)
library(class) #Used for KNN
#################################################################################################

####### Linear Regression #####################################################################
## Training using linear regression Model
lin_Reg_Model <- lm(V1 ~ ., data = train_data_2_3_only)
# summary(lin_Reg_Model)
# intercept <- lin_Reg_Model$coef[1]
# slope <- lin_Reg_Model$coef[-1]

##### function for Predicting between 2's and 3's ####
two_three_func <- function(x){
  ifelse((x>=2.5), '3','2')
}

## Training Error
lin_pred_train <- two_three_func(predict(lin_Reg_Model, train_data_2_3_only)) #sapply( ,two_three_func)
lin_pred_train_error <- mean(lin_pred_train != train_data_2_3_only[,1])
print("=================== Predicting 2's & 3's by Linear Regression ==================================")
print("Training Error Percent - Linear Regression")
print(lin_pred_train_error*100)

## Testing Error
lin_pred_test <-  two_three_func(predict(lin_Reg_Model, test_data_2_3_only)) #sapply( ,two_three_func)
lin_pred_test_error <- mean(lin_pred_test != test_data_2_3_only[,1])
print("Testing Error Percent - Linear Regression")
print(lin_pred_test_error*100)
print("======================= End of Linear Regression Prediction or Performance ============================")


###### K-Nearest Neighbour Classification ###################################################################

## Normalize function ##
data_norm <- function(x){
  return ((x-min(x)) / (max(x)-min(x)))
}

## Normailizing train data and test data ##
norm_train_data_2_3 <- sapply(train_data_2_3_only[,-1], data_norm)
norm_test_data_2_3 <- sapply(test_data_2_3_only[,-1], data_norm)
# summary(train_zipcode_data[1:5,2:5])
# summary(norm_train_zipcode_data[1:5,2:5])

## KNN function applying and predicting training error ##
knn_func_training <- function(train, test, k) {
  knn_fit <- knn(train, test, cl=train_data_2_3_only[,1], k=k, prob=F)
  knn_error_train <- mean(knn_fit != train_data_2_3_only[,1])
}
## KNN function applying and predicting testing error ##
knn_func_testing <- function(train, test, k) {
  knn_fit <- knn(train, test, cl=train_data_2_3_only[,1], k=k, prob=F)
  knn_error_test <- mean(knn_fit != test_data_2_3_only[,1])
}


## Analyzing KNN Algorithm with K = 1, 3, 5, 7, 15 ##
for (i in c(1, 3, 5, 7, 15)) {
  knn_train_error <- knn_func_training(norm_train_data_2_3[,-1], norm_train_data_2_3[,-1], i)
  knn_test_error <- knn_func_testing(norm_train_data_2_3[,-1], norm_test_data_2_3[,-1], i)
  train_test_error <- sprintf(paste('For k=%2d: ', 'train error percent is %.3f &', 'test error percent is %.3f'),
                          i, knn_train_error*100, knn_test_error*100)

  print(train_test_error)
 
}
print("====================== End of KNN Algorithm Prediction or Performance ===============================")

#############################################################################################################

