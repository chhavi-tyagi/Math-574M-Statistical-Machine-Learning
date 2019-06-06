# Problem 4: Prostate Cancer: Linear Regression

library(MASS)
## Read the information of the prostate cancer datasets
data_info <- read.delim("prostate.info.txt")
## Read prostate cancer datasets 
data_prostate <- read.delim("prostate.data.txt")
training_data_prostrate <- data_prostate[data_prostate$train==T, ]# Creating training datasets (train=True)
testing_data_prostrate <- data_prostate[data_prostate$train==F, ] # Creating testing datasets (train=False)
train_data_prostate <- training_data_prostrate[,-c(1,11)] # Removing 1st and 11th columns from traing datasets
test_data_prostate <- testing_data_prostrate[,-c(1,11)] # Removing 1st and 11th columns from testing datasets

# train_data_prostate <- as.data.frame(train_data_prostate) # Converted datasets into matrix form
# names(train_data_prostate) <- c("lcavol","lweight","age","lbph", "svi", "lcp", "gleason", "pgg45", "y")

#### (a) Fit the standard linear regression model using the ordinary least squares.
# lin_Reg_Model = lm(lpsa ~ lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45, data=train_data_prostate)
lin_Reg_Model <- lm(lpsa ~., data=train_data_prostate)
summary_out <- summary(lin_Reg_Model)
print('Summary of Linear Regresson Model: ')
print(summary_out)


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



#####################################################################################################
#### (b) BIC Apply forward selection to select variables
library(leaps)

# M_j = regsubsets(lpsa ~ lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45, data=train_data_prostate)
fwd_model = regsubsets(train_data_prostate[,-9], train_data_prostate[,9], method="forward")
fwd_summary <- summary(fwd_model)
# fwd_summary <- summary(fwd_summary, matrix.logical=TRUE)
# print(fwd_summary)

coefficient <- coef(fwd_model, id=1:8) # this is regresssion coefficent
print('Regression Coefficeint: ')
print(coefficient)
cat('\n')

### Checking model structure
rss_out <- str(fwd_model)
# print(rss_out)

## computer training error
cat("\n")
print('Training Error for the Variable Selection: ')
train_error_reg <- (fwd_model$rss[-1]) / 67
print(train_error_reg)
print("==========================================================================")
cat("\n")

# BIC using the formula
bic <- rep(0,8)
for (i in 1:8){
  bic[i] = 67*log(train_error_reg[i]) + log(67)*(1+i)
}
print("BIC from 8 different models: ")
print(bic)

# find the optimal model
optimal_bic <- which.min(bic)
print("Optimal BIC: ")
print(optimal_bic)
cat("\n")

# report the set of important variables selected by BIC
imp_var_bic <- coefficient[which.min(bic)]
print('Report of Important Variable selected by BIC')
print(imp_var_bic)
cat("\n")

# use the selected variables to refit the OLS and report TestErr
refit_OLS_bic <- lm(lpsa ~ lcavol+lweight, data=train_data_prostate)

# compute the testing error
lin_pred_test_refit <- predict(refit_OLS_bic, test_data_prostate)
n_test <- 30
test_error_refit_bic <- sum(((test_data_prostate[,9]) - lin_pred_test_refit)^2) / n_test

print('Testing Error for the Refitted OLS BIC: ')
print(test_error_refit_bic)
print("=====================================================================================")
cat("\n")

# #####################################################################################################
# #### (c) AIC Apply forward selection to select variables
# AIC using the formula
aic <- rep(0,8)
for (i in 1:8){
  aic[i] = 67*log(train_error_reg[i]) + 2*(i+1)
}
print("AIC from 8 different models: ")
print(aic)
cat("\n")

# find the optimal model
optimal_aic <- which.min(aic)
print("Optimal AIC: ")
print(optimal_aic)
cat("\n")

# report the set of important variables selected by AIC
imp_var_aic <- coefficient[which.min(aic)]
print('Report of Important Variable selected by AIC')
print(imp_var_aic)
cat("\n")

# use the selected variables to refit the OLS and report TestErr
refit_OLS_aic <- lm(lpsa ~ lcavol+lweight+age+lbph+svi+lcp+pgg45, data=train_data_prostate)

# compute the testing error
lin_pred_test_refit <- predict(refit_OLS_aic, test_data_prostate)
n_test <- 30
test_error_refit_aic <- sum(((test_data_prostate[,9]) - lin_pred_test_refit)^2) / n_test
print('Testing Error for the Refitted OLS AIC: ')
print(test_error_refit_aic)
print("=====================================================================================")
