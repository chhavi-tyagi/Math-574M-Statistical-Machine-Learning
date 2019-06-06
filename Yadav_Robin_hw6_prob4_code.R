# Problem 4: Classify 2's and 3's for zip code using the linear SVM, Gaussian kernel SVM, and polynomial kernel
# SVM. Fit the training data using a sequence of tuning parameters and find the one which gives the best
# error on the test set. You may consider using the R package e1071.

# Installing package e1071
# install.packages('e1071')
# Needed Libraries
library(e1071)


## Normalize function ##
data_norm <- function(x){
  return ((x-min(x)) / (max(x)-min(x)))
}

####### Reading Data from zip file #######
train_zipcode_data <- read.table("zip.train.gz")
train_data_2_3_only <- train_zipcode_data[train_zipcode_data[,1] %in% c(2,3), ]
X_train <- sapply(train_data_2_3_only[,-1], data_norm) # Normalizing training data
train_data <- cbind(train_data_2_3_only[,1], X_train)
train_data <- as.data.frame(train_data)
train_data$V1 <- as.factor(train_data$V1)


test_zipcode_data <-  read.table('zip.test.gz')
test_data_2_3_only <- test_zipcode_data[test_zipcode_data[,1] %in% c(2,3), ]
X_test <- sapply(test_data_2_3_only[,-1], data_norm) # Normalizing testing data
test_data <- cbind(test_data_2_3_only[,1], X_test)
test_data <- as.data.frame(test_data)
test_data$V1 <- as.factor(test_data$V1)



#### Linear SVM ####
## Tuning ##
set.seed(123)
linearSVM_tune <- tune(svm, V1~., data=train_data, kernel="linear",
                       ranges =list(cost=2^(2:7)))
info_linearSVM_tune <- summary(linearSVM_tune)
# print(info_linearSVM_tune)
best_linear_model <- linearSVM_tune$best.model
# print(best_linear_model)

## Training ##
linearSVM <- svm(V1~., data=train_data, kernel="linear",
                 gamma = best_linear_model$gamma, 
                 cost = best_linear_model$cost)
info_linearSVM <- summary(linearSVM)
print(info_linearSVM)

linearSVM_pred <- predict(linearSVM, newdata=test_data[,-1])
linearSVM_error <- mean(linearSVM_pred != test_data[,1])
cat('\n')
print('Linear SVM: Classifiying 2 and 3; Test Error: ')
print(linearSVM_error)

cat('\n')
print('###################################################################')



#### Gaussian Kernel SVM ####
## Tuning ##
set.seed(123)
gaussSVM_tune <- tune(svm, V1~., data=train_data, kernel="radial",
                       ranges =list(cost=2^(2:7)))
info_gaussSVM_tune <- summary(gaussSVM_tune)
# print(info_gaussSVM_tune)
best_gauss_model <- gaussSVM_tune$best.model
# print(best_gauss_model)

## Training ##
gaussSVM <- svm(V1~., data=train_data, kernel="radial",
                 gamma = best_gauss_model$gamma, 
                 cost = best_gauss_model$cost)
info_gaussSVM <- summary(gaussSVM)
print(info_gaussSVM)

gaussSVM_pred <- predict(gaussSVM, newdata=test_data[,-1])
gaussSVM_error <- mean(gaussSVM_pred != test_data[,1])
cat('\n')
print('Gaussian SVM: Classifiying 2 and 3; Test Error: ')
print(gaussSVM_error)

cat('\n')
print('###################################################################')



#### Polynomial Kernel SVM ####
## Tuning ##
set.seed(123)
polySVM_tune <- tune(svm, V1~., data=train_data, kernel="polynomial",
                      ranges = list(degree = c(1,2,3,4)))
info_polySVM_tune <- summary(polySVM_tune)
# print(info_polySVM_tune)
best_poly_model <- polySVM_tune$best.model
# print(best_poly_model)

## Training ##
polySVM <- svm(V1~., data=train_data, kernel="poly",
                gamma = best_poly_model$gamma, 
                cost = best_poly_model$cost,
                degree = best_poly_model$degree )
info_polySVM <- summary(polySVM)
print(info_polySVM)

polySVM_pred <- predict(polySVM, newdata=test_data[,-1])
polySVM_error <- mean(polySVM_pred != test_data[,1])
cat('\n')
print('Polynomial SVM: Classifiying 2 and 3; Test Error: ')
print(polySVM_error)

cat('\n')
print('###################################################################')


