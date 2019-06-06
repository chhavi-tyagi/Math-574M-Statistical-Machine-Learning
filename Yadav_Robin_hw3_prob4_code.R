# Problem 4: LDA and Logistic Regresssion for Binary Classification: Scenario 1


########Libraries
library(MASS)
library(dplyr)


# Restoring Data
load("training_two_class.RData")
load("testing_two_class.RData")


# # Plot for Scenario 1
# plot(training_two_class[1:100, ], col="green", xlim=c(-2,5), ylim=c(-2,6), xlab = "x", ylab = "y", main="Two-Class Classification Problem Plot")
# points(training_two_class[101:200, ], col="red") # points() used to plot on the existing plot
# legend(x=3.5,y=6,legend=c("Class Green","Class Red"),cex=.8,col=c("green","red"),pch=c(1,1))
# 
##################################################################################################################
#### Bayes Decision Boundary x = y found by the derviation check homework#2 problem#5
abline(a=0,b=1, col=5) #y=bx+a
legend(x=3.2,y=-1.2,legend=c("Bayes Decision","Linear Decision"),cex=.8,col=c(col=5,col=4),pch=c(20,20))

## Training Error : Class 1=green, Class 2=red
ybay_train = c(rep(1,100), rep(0,100))
train_set_scen1 = cbind(training_two_class, ybay_train)
ybay_train_pred = 1*(train_set_scen1[,1]>train_set_scen1[,2])
ybay_train_erros = mean(ybay_train_pred!=train_set_scen1[,3])
print("Bayes Train Error: Scenario 1")
print(ybay_train_erros)

## Testing Error : Class 1=green, Class 2=red
ybay_test = c(rep(1,500), rep(0,500))
test_set_scen1 = cbind(testing_two_class, ybay_test)
ybay_test_pred = 1*(test_set_scen1[,1]>test_set_scen1[,2])
ybay_test_erros = mean(ybay_test_pred!=test_set_scen1[,3])
print("Bayes Test Error: Scenario 1")
print(ybay_test_erros)

# ####################################################################################################################
###### Linear Model fit Training linear regression model with training data: Class 1=green, Class 2=red ############
y_lin_train = c(rep(1, 100), rep(0, 100))
train_lin_set_scen1 = training_two_class # contain data without label

lin_model_scen1 = lm(y_lin_train ~ train_lin_set_scen1)
summary(lin_model_scen1)
intercept_lin_scen1 = lin_model_scen1$coef[1]
slope_lin_scen1 = lin_model_scen1$coef[-1]
# # X2 = (-c1/c3) + (-c2/c3)*X1
abline(coef=c((intercept_lin_scen1-0.5)/(-slope_lin_scen1[2]),slope_lin_scen1[1]/(-slope_lin_scen1[2])), col=4)


### Training Error : Linear Classifier greater than 0.5
lin_pred_train_scen1 =  as.vector(1*((intercept_lin_scen1 + train_lin_set_scen1%*%slope_lin_scen1)>0.5))
lin_pred_train_error = mean(lin_pred_train_scen1 != y_lin_train)
print("Linear Model Train Error: Scenario 1")
print(lin_pred_train_error)

### Testinng Error:Class 1=green, Class 2=red
y_lin_test = c(rep(1,500), rep(0,500))
test_lin_set_scen1 = testing_two_class
lin_pred_test_scen1 =  as.vector(1*((intercept_lin_scen1 + test_lin_set_scen1%*%slope_lin_scen1)>0.5))
lin_pred_test_error = mean(lin_pred_test_scen1 != y_lin_test)
print("Linear Model Test Error: Scenario 1")
print(lin_pred_test_error)



# # #########################################################################################################################
#### (a) LDA Model for scenario 1
y_lda_train <- c(rep(1, 100), rep(0, 100))
lda_training_data <- cbind(training_two_class, y_lda_train)
lda_training_data <- as.data.frame(lda_training_data)
names(lda_training_data) <- c("x1","x2","y")
y_lda_test = c(rep(1, 500), rep(0, 500))
lda_testing_data <- cbind(testing_two_class, y_lda_test)
lda_testing_data <- as.data.frame(lda_testing_data)
names(lda_testing_data) <- c("x1","x2","y")

### Training LDA model with training data
lda_model <- lda(y ~ x1+x2, data=lda_training_data)
summary(lda_model)


# Training Errors
table(lda_training_data[,3])

y_pred_lda_train <- predict(lda_model, lda_training_data)$class
table(y_pred_lda_train, lda_training_data[,3])
lda_training_erros <- mean(y_pred_lda_train != lda_training_data[,3])
print("LDA Train Error: Scenario 1")
print(lda_training_erros)


# Testing Errors
table(lda_testing_data[,3])

y_pred_lda_test <- predict(lda_model, lda_testing_data)$class

table(y_pred_lda_test, lda_testing_data[,3])
lda_testing_erros = mean(y_pred_lda_test != lda_testing_data[,3])
print("LDA Test Error: Scenario 1")
print(lda_testing_erros)




#############################################################################################################################
# (b) Logitic Regression for scenario 1
y_log_train = c(rep(1,100), rep(0,100)) # Green Class:1 and Red Class: 0
log_training_data = cbind(training_two_class, y_log_train)
log_training_data = as.data.frame(log_training_data)
names(log_training_data) = c("x1","x2", "y")
#table(log_training_data[,3])

y_log_test = c(rep(1,500), rep(0,500)) # Green Class:1 and Red Class: 0
log_testing_data = cbind(testing_two_class, y_log_test)
log_testing_data = as.data.frame(log_testing_data)
names(log_testing_data) = c("x1","x2", "y")
#table(log_testing_data[,3])

#Logistic Regression Model
log_model = glm(y ~ x1+x2, data=log_training_data, family='binomial')
summary(log_model)
# X2 = (-c1/c3) + (-c2/c3)*X1
# slope = (-coef(log_model)[2])/(coef(log_model)[3])
# intercept = (-coef(log_model)[1])/(coef(log_model)[3])
# abline(a=intercept,b=slope,col=6)

# Training Error
y_pred_log_train = predict(log_model, log_training_data)
#log_training_data = log_training_data %>% mutate(log_v = ifelse(y_pred_log_train > 0, '1','0'))
log_v = ifelse(y_pred_log_train > 0, '1','0')
y_new = cbind(log_training_data, log_v)
log_training_error = mean(y_new[,4] != log_training_data[,3])
#log_training_error = nrow(subset(log_training_data,log_v != log_training_data[,3]))/nrow(log_training_data)
print("Logistics Regression Train Error: Scenario 1")
print(log_training_error)

# # Testing Error
y_pred_log_test = predict(log_model, log_testing_data)
log_testing_data = log_testing_data %>% mutate(log_v = ifelse(y_pred_log_test > 0, '1','0'))
log_testing_error = nrow(subset(log_testing_data,log_v!=log_testing_data[,3]))/nrow(log_testing_data)
print("Logistics Regression Test Error: Scenario 1")
print(log_testing_error)





