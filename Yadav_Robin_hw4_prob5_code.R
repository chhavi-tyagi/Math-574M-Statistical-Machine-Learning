# Problem 5: K-Nearest neigbhour for Classification


###### Library ###################################################################################
library(MASS)
library(class) #Use for KNN
#################################################################################################

#### (a) Fit k-nearest neighbor classifier with a range of values k for the training data in Scenario 1 ####
## loading Datasets for Scenario 1 ##
load("training_two_class.RData")
load("testing_two_class.RData")

## Arranging datasets for training and testing
ytrain <- c(rep(1,100), rep(0,100))
train_set_scen1 <- cbind(training_two_class, ytrain)

ytest <- c(rep(1,500), rep(0,500))
test_set_scen1 <- cbind(testing_two_class, ytest)

## KNN function applying and predicting training error ##
knn_func_training <- function(train, test, k) {
  knn_fit <- knn(train, test, cl=train_set_scen1[,3], k=k, prob=F)
  knn_error <- mean(knn_fit != train_set_scen1[,3])
}
## KNN function applying and predicting testing error ##
knn_func_testing <- function(train, test, k) {
  knn_fit <- knn(train, test, cl=train_set_scen1[,3], k=k, prob=F)
  knn_error <- mean(knn_fit != test_set_scen1[,3])
}

## Analyzing KNN Algorithm with K = 1, 4, 7, 10, 13, 16, 30, 45, 60, 80, 100, 150, 200 ##
error_vector <- matrix(NA,ncol=3, nrow=13) # creating empty matrix
j=0
for (i in c(1, 4, 7, 10, 13, 16, 30, 45, 60, 80, 100, 150, 200)) {
  knn_train_error <- knn_func_training(train_set_scen1[,-3], train_set_scen1[,-3], i)
  knn_test_error <- knn_func_testing(train_set_scen1[,-3], test_set_scen1[,-3], i)
  train_test_error <- sprintf(paste('For k=%2d: ', 'train error percent is %.1f & ', 'test error percent is %.1f'),
                              i, knn_train_error*100, knn_test_error*100)
  print(train_test_error)
  j=j+1;
  error_vector[j, 1] = i
  error_vector[j, 2] = knn_train_error
  error_vector[j, 3] = knn_test_error
}

### Plot two curves: the training error vs k-Number of Nearest Neighbours, and the testing error vs k-Number of Nearest Neighbours, in same figure
# Plot for Scenario 1: only with k
plot(error_vector[, 1], error_vector[, 2], type="b", lwd=2, col="red", xlim=c(0,200), ylim=c(0,0.6), xlab = "k-Number of Nearest Neighbours", ylab = "Errors", main="Scenario 1")
lines(error_vector[,1], error_vector[,3], type="b", lwd=2, col="blue")
legend(x=150,y=0.07,legend=c("Train Error","Test Error"),cex=1,col=c("red","blue"),pch=c(1,1), lwd=2)

# Plot for Scenario 1: with n/k
plot(200/error_vector[, 1], error_vector[, 2], type="b", lwd=2, col="red", xlim=c(0,200), ylim=c(0,0.6), xlab = "n/k-Degrees of Freedom", ylab = "Errors", main="Scenario 1")
lines(200/error_vector[,1], error_vector[,3], type="b", lwd=2, col="blue")
legend(x=150,y=0.55,legend=c("Train Error","Test Error"),cex=1,col=c("red","blue"),pch=c(1,1), lwd=2)

print("======================== End of Scenario 1 ===================================")



################################################################################################################

#### (b) Fit k-nearest neighbor classifier with a range of values k for the training data in Scenario 2 ####
## loading Datasets for Scenario 2 ##
load("trainSet_Scen2_two_class.RData")
load("testSet_Scen2_two_class.RData")

## KNN function applying and predicting training error ##
knn_func_scen2_training <- function(train, test, k) {
  knn_fit <- knn(train, test, cl=trainSet_Scen2_two_class[,3], k=k, prob=F)
  knn_error <- mean(knn_fit != trainSet_Scen2_two_class[,3])
}
## KNN function applying and predicting testing error ##
knn_func_scen2_testing <- function(train, test, k) {
  knn_fit <- knn(train, test, cl=trainSet_Scen2_two_class[,3], k=k, prob=F)
  knn_error <- mean(knn_fit != testSet_Scen2_two_class[,3])
}

## Analyzing KNN Algorithm with K = 1, 4, 7, 10, 13, 16, 30, 45, 60, 80, 100, 150, 200 ##
error_vector_scen2 <- matrix(NA, ncol=3, nrow=13) # creating empty matrix
k=0
for (i in c(1, 4, 7, 10, 13, 16, 30, 45, 60, 80, 100, 150, 200)) {
  knn_train_error_scen2 <- knn_func_scen2_training(trainSet_Scen2_two_class[,-3], trainSet_Scen2_two_class[,-3], i)
  knn_test_error_scen2 <- knn_func_scen2_testing(trainSet_Scen2_two_class[,-3], testSet_Scen2_two_class[,-3], i)
  train_test_error_scen2 <- sprintf(paste('For k=%2d: ', 'train error percent: %.1f  & ', 'test error percent: %.1f'),
                                    i, knn_train_error_scen2*100, knn_test_error_scen2*100)

  print(train_test_error_scen2)
  k=k+1;
  error_vector_scen2[k, 1] = i
  error_vector_scen2[k, 2] = knn_train_error_scen2
  error_vector_scen2[k, 3] = knn_test_error_scen2
}

# Plot for Scenario 2
plot(error_vector_scen2[, 1], error_vector_scen2[, 2], type="b", col="red", lwd=2, xlim=c(0,200), ylim=c(0,0.55), xlab = "k-Number of Nearest Neighbours", ylab = "Errors", main="Scenario 2")
points(error_vector_scen2[,1], error_vector_scen2[,3], type="b", col="blue", lwd=2)
legend(x=150,y=0.07,legend=c("Train Error","Test Error"),cex=1,col=c("red","blue"),pch=c(1,1), lwd=2)

plot(200/error_vector_scen2[, 1], error_vector_scen2[, 2], type="b", col="red", lwd=2, xlim=c(0,200), ylim=c(0,0.55), xlab = "n/k-Degrees of Freedom", ylab = "Errors", main="Scenario 2")
points(200/error_vector_scen2[,1], error_vector_scen2[,3], type="b", col="blue", lwd=2)
legend(x=150,y=0.55,legend=c("Train Error","Test Error"),cex=1,col=c("red","blue"),pch=c(1,1), lwd=2)

 print("======================== End of Scenario 2 ===================================")


###############################################################################################################