
########Libraries
library(MASS)

# Restoring Data
load("training_two_class.RData")
load("testing_two_class.RData")

# #########################################################################################################################
#### (a) LDA Model for scenario 1
y_lda_train = c(rep(1, 100), rep(0, 100))
lda_training_data = cbind(training_two_class, y_lda_train)
lda_training_data = as.data.frame(lda_training_data)
y_lda_test = c(rep(1, 500), rep(0, 500))
lda_testing_data = cbind(testing_two_class, y_lda_test)
lda_testing_data = as.data.frame(lda_testing_data)


lda_model = lda(lda_training_data[,3] ~ lda_training_data[,1]+lda_training_data[,2], data=lda_training_data)
summary(lda_model)


# Training Errors
table(lda_training_data[,3])

y_pred_lda_train = predict(lda_model, lda_training_data)$class
table(y_pred_lda_train, lda_training_data[,3])
lda_training_erros = mean(y_pred_lda_train != lda_training_data[,3])
print("LDA Train Error: Scenario 1")
print(lda_training_erros)


# Testing Errors
table(lda_testing_data[,3])

y_pred_lda_test = predict(lda_model, lda_testing_data)$class
# y_pred_lda_test is just 200 rows, I do not know. It suppose to be 1000 rows,
# I am just following slides where you have equal number of rows for both training
# and testing

#table(y_pred_lda_test, lda_testing_data[,3])
lda_testing_erros = mean(y_pred_lda_test != lda_testing_data[,3])
print("LDA Test Error: Scenario 1")
print(lda_testing_erros)
