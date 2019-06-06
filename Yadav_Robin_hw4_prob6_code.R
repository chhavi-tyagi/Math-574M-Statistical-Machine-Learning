## Problem 6: Classify the 1's, 2's, 3's for the zip code data.

####### Reading Data from zip file #######
train_zipcode_data = read.table("zip.train.gz")
train_zipcode_data = as.data.frame(train_zipcode_data) # We do not have cbind and rbind
train_data_1_2_3_only = train_zipcode_data[train_zipcode_data[,1] %in% c(1,2,3), ]
test_zipcode_data =  read.table('zip.test.gz')
test_zipcode_data = as.data.frame(test_zipcode_data) # We do not have cbind and rbind
test_data_1_2_3_only = test_zipcode_data[test_zipcode_data[,1] %in% c(1,2,3), ]

#################################################################################################

###### Library ###################################################################################
#install.packages('class')
library(MASS)
library(class) #Used for KNN
#################################################################################################


#### (a) Use the k-nearest neighbor classification with k = 1, 3, 5, 7, 15. Show both the training and
# test error for each choice.
## Normalize function ##
data_norm <- function(x){
  return ((x-min(x)) / (max(x)-min(x)))
}

## Normailizing train data and test data ##
norm_train_data_1_2_3 <- sapply(train_data_1_2_3_only[,-1], data_norm)
norm_test_data_1_2_3 <- sapply(test_data_1_2_3_only[,-1], data_norm)
# summary(train_zipcode_data[1:5,2:5])
# summary(norm_train_zipcode_data[1:5,2:5])

## KNN function applying and predicting training error ##
knn_func_training <- function(train, test, k) {
  knn_fit <- knn(train, test, cl=train_data_1_2_3_only[,1], k=k, prob=F)
  knn_error_train <- mean(knn_fit != train_data_1_2_3_only[,1])
}
## KNN function applying and predicting testing error ##
knn_func_testing <- function(train, test, k) {
  knn_fit <- knn(train, test, cl=train_data_1_2_3_only[,1], k=k, prob=F)
  knn_error_test <- mean(knn_fit != test_data_1_2_3_only[,1])
}

## Analyzing KNN Algorithm with K = 1, 3, 5, 7, 15 ##
for (i in c(1, 3, 5, 7, 15)) {
  knn_train_error <- knn_func_training(norm_train_data_1_2_3[,-1], norm_train_data_1_2_3[,-1], i)
  knn_test_error <- knn_func_testing(norm_train_data_1_2_3[,-1], norm_test_data_1_2_3[,-1], i)
  train_test_error <- sprintf(paste('For k=%2d: ', 'train error percent is %.2f &', 'test error percent is %.2f'),
                              i, knn_train_error*100, knn_test_error*100)
  print(train_test_error)
}

#############################################################################################################


#### (b) Implement the LDA method and report its training and testing errors.

####### Removing Column coulmn 1 to 17 from training and testing datasets #######
library(MASS)
train_out_data_1_2_3_only = train_data_1_2_3_only[, 18:257]
test_out_data_1_2_3_only= test_data_1_2_3_only[, 18:257]

### Training LDA model with training data
lda_model <- lda(train_out_data_1_2_3_only, grouping=train_data_1_2_3_only[,1])
#lda_model <- lda(V1~., data=train_out_data_1_2_3_only)
summary(lda_model)


## Training Errors ##
y_pred_lda_train <- predict(lda_model, train_out_data_1_2_3_only)$class
#table(y_pred_lda_train, train_out16_data_2_3_only[,1])
lda_training_erros <- mean(y_pred_lda_train != train_data_1_2_3_only[,1])
print("LDA Train Error Percent: Classify 1's, 2's, 3's")
lda_train_error <- sprintf(paste('LDA train error percent is %.2f'),lda_training_erros*100)
print(lda_train_error)


## Testing Errors ##
y_pred_lda_test <- predict(lda_model, test_out_data_1_2_3_only)$class
#table(y_pred_lda_test, test_out16_data_2_3_only[,1])
lda_testing_erros <- mean(y_pred_lda_test != test_data_1_2_3_only[,1])
print("LDA Test Error Percent: Classify 1's, 2's, 3's")
lda_test_error <- sprintf(paste('LDA test error percent is %.2f'),lda_testing_erros*100)
print(lda_test_error)

##############################################################################################################

