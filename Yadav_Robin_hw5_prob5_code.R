# Problem 5: Fit the LASSO regression for the prostate cancer data set. 

library(lars)
library(MASS)
## Read the information of the prostate cancer datasets
data_info <- read.delim("prostate.info.txt")
## Read prostate cancer datasets 
data_prostate <- read.delim("prostate.data.txt")
training_data_prostrate <- data_prostate[data_prostate$train==T, ]# Creating training datasets (train=True)
testing_data_prostrate <- data_prostate[data_prostate$train==F, ] # Creating testing datasets (train=False)
train_data_prostate <- training_data_prostrate[,-c(1,11)] # Removing 1st and 11th columns from traing datasets
test_data_prostate <- testing_data_prostrate[,-c(1,11)] # Removing 1st and 11th columns from testing datasets

####################################################################################################
##### (a) Select the parameter with 5-fold CV, using the minimum CV rule.

lasso_step <- seq(0,1, length=100)
lasso_cv <- cv.lars(as.matrix(train_data_prostate[,-9]), train_data_prostate[,9], K=5, index=lasso_step, mode="fraction")
# plot(lasso.cv)
# summary(lasso.cv)

lasso_min_cv <- which.min(lasso_cv$cv)

## minimum CV rule
bests1 <- lasso_step[lasso_min_cv]
print("Best Lambda: ")
print(bests1)

lasso_model <- lars(as.matrix(train_data_prostate[,-9]), train_data_prostate[,9])
lasso_coef1 <- predict.lars(lasso_model, s=bests1, type="coef", mode="fraction")
print("Regression Coefficeint: ")
print(lasso_coef1)

predict_test_error <- predict.lars(lasso_model, newx=as.matrix(test_data_prostate[,-9]), s=bests1, type="fit", mode="fraction")
test_error_minCV<- mean((predict_test_error$fit - test_data_prostate[,9])^2)
print("Test Error using Minimum CV: ")
print(test_error_minCV)

cat("\n")

##### (b) Select the parameter with 5-fold CV, using the one-standard deviation rule for CV.
bound <- lasso_cv$cv[lasso_min_cv] + lasso_cv$cv.error[lasso_min_cv]

# print(bound)

bests2 <- lasso_step[min(which(lasso_cv$cv<bound))]
print("Best Lambda: ")
print(bests2)

lasso_coef2 <- predict.lars(lasso_model, s=bests2, type="coef", mode="fraction")
print("Regression Coefficient:")
print(lasso_coef2)

predict_test_error_std <- predict.lars(lasso_model, newx=as.matrix(test_data_prostate[,-9]), s=bests2, type="fit", mode="fraction")
test_error_stdCV<- mean((predict_test_error_std$fit - test_data_prostate[,9])^2)
print("Test Error using one-standard deviation CV: ")
print(test_error_stdCV)
