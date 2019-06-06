# Problem 3: 

library(MASS)
library(lars)
## Read the information of the prostate cancer datasets
data_info <- read.delim("prostate.info.txt")
## Read prostate cancer datasets 
data_prostate <- read.delim("prostate.data.txt")
training_data_prostrate <- data_prostate[data_prostate$train==T, ]# Creating training datasets (train=True)
testing_data_prostrate <- data_prostate[data_prostate$train==F, ] # Creating testing datasets (train=False)
train_data_prostate <- training_data_prostrate[,-c(1,11)] # Removing 1st and 11th columns from traing datasets
test_data_prostate <- testing_data_prostrate[,-c(1,11)] # Removing 1st and 11th columns from testing datasets



# lin_Reg_Model = lm(lpsa ~ lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45, data=train_data_prostate)
lin_Reg_Model <- lm(lpsa ~., data=train_data_prostate)
summary_out <- summary(lin_Reg_Model)
print('Summary of Linear Regresson Model: ')
print(summary_out)

beta_OLS <- lin_Reg_Model$coefficients
beta_OLS_new <- beta_OLS[-1]

## compute the training error
# lin_pred <- as.vector(1*((inter2+train_data_prostate[,-9]%*%slope2)>0.5)) #
lin_pred_train <- predict(lin_Reg_Model, train_data_prostate)
n_train <- 67
train_error <- sum(((train_data_prostate[,9]) - lin_pred_train)^2) / n_train
print('Training Error for the Linear Regression: ')
print(train_error)

## compute the testing error
lin_pred_test <- predict(lin_Reg_Model, testing_data_prostrate[,-c(1,11)])
n_test <- 30
test_error <- sum(((test_data_prostate[,9]) - lin_pred_test)^2) / n_test
print('Testing Error for the Linear Regression: ')
print(test_error)

# (a) Select the parameter with 5-fold CV, using the minimum CV rule. Report the best tuning parameter,
# the selected model, the estimated regression coefficients, and the TestErr.

X_train <- as.matrix(train_data_prostate[,-9]) %*% diag(beta_OLS_new) # Weight = 1/beta_OLS and Xnew = X/Weight
lasso_step <- seq(0,1, length=100)
lasso_cv <- cv.lars(as.matrix(X_train), train_data_prostate[,9], K=5, index=lasso_step, mode="fraction")
summary(lasso_cv)

## Selected Model
print('Selected Model: ')
lasso_min_cv <- which.min(lasso_cv$cv)
print(lasso_min_cv)


## minimum CV rule
best_lambda1 <- lasso_step[lasso_min_cv]
print("Tunning parameter Minimum CV, Best Lambda: ")
print(best_lambda1)


new_lasso_model <- lars(as.matrix(X_train), train_data_prostate[,9])
new_lasso_coef1 <- predict.lars(new_lasso_model, s=best_lambda1, type="coef", mode="fraction")
beta_LASSO_coef1 <- as.vector(new_lasso_coef1$coefficients) * as.vector(beta_OLS_new)
print("Adaptive Lasso Estimated Regression Coefficeint from Minimum CV:  ")
print(beta_LASSO_coef1)

predict_test_error <- as.matrix(test_data_prostate[,-9]) %*% beta_LASSO_coef1
test_error_minCV <- mean((predict_test_error - test_data_prostate[,9])^2)
print("Test Error using Minimum CV: ")
print(test_error_minCV)

cat("\n")


# (b) Select the parameter with 5-fold CV, using the one-standard rule. Report the best tuning parameter,
# the selected model, the estimated regression coefficients, and the TestErr.

bound <- lasso_cv$cv[lasso_min_cv] + lasso_cv$cv.error[lasso_min_cv]
best_lambda2 <- lasso_step[min(which(lasso_cv$cv<bound))]
print("Tunning parameter for One Standard, Best Lambda: ")
print(best_lambda2)

new_lasso_coef2 <- predict.lars(new_lasso_model, s=best_lambda2, type="coef", mode="fraction")
beta_LASSO_coef2 <- as.vector(new_lasso_coef2$coefficients) * as.vector(beta_OLS_new)
print("Adaptive Lasso Estimated Regression Coefficient from One Standard:")
print(beta_LASSO_coef2)


predict_test_error_std <- as.matrix(test_data_prostate[,-9]) %*% beta_LASSO_coef2
test_error_stdCV <- mean((predict_test_error_std - test_data_prostate[,9])^2)
print("Test Error using one-standard deviation CV: ")
print(test_error_stdCV)






