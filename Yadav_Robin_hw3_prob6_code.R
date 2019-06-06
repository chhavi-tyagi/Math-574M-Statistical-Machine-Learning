# Problem 6: Linear Method for Classification: Scenario 2

library(MASS)
load("trainSet_Scen2_two_class.RData")
load("testSet_Scen2_two_class.RData")

# Scatter Plot for Scenario 2
plot(trainSet_Scen2_two_class[1:100, 1:2], col="green", xlim=c(-2,4), ylim=c(-3,4.5), xlab = "X1", ylab = "X2", main="Two-Class Classifcatio: Scenario#2")
points(trainSet_Scen2_two_class[101:200, 1:2], col="red") # points() used to plot on the existing plot
legend(x=2.8,y=4.5,legend=c("Class Green","Class Red"),cex=.8,col=c("green","red"),pch=c(1,1))

# Training linear regression model with training data: Class 1=green, Class 2=red
train_lin_set_scen2_X = trainSet_Scen2_two_class[,1:2]
train_lin_set_scen2_Y = trainSet_Scen2_two_class[,3]
#train_lin_set_scen2 = as.data.frame(train_lin_set_scen2)
#names(train_lin_set_scen2) = c("x1","x2","y")

lin_model_scen2 = lm(train_lin_set_scen2_Y ~ train_lin_set_scen2_X)
summary(lin_model_scen2)
intercept_lin_scen2 = lin_model_scen2$coef[1]
slope_lin_scen2 = lin_model_scen2$coef[-1]
# # X2 = (-c1/c3) + (-c2/c3)*X1
abline(coef=c((intercept_lin_scen2-0.5)/(-slope_lin_scen2[2]),slope_lin_scen2[1]/(-slope_lin_scen2[2])), col=4,lwd=3)

### Training Error : Linear Classifier greater than 0.5, Class 1=green, Class 2=red
lin_pred_train_scen2 =  ifelse(as.vector(1*((intercept_lin_scen2 + train_lin_set_scen2_X %*% slope_lin_scen2)>0.5)),'1','0')
lin_pred_train_error = mean(lin_pred_train_scen2 != train_lin_set_scen2_Y)
print("Scenario 2 - Training Error")
print(lin_pred_train_error)

### Testinng Error: Linear Classifier greater than 0.5, Class 1=green, Class 2=red
#y_lin_test = c(rep(1,500), rep(0,500))
test_lin_set_scen2_X = testSet_Scen2_two_class[,1:2]
test_lin_set_scen2_Y = testSet_Scen2_two_class[,3]

lin_pred_test_scen2 =  ifelse(as.vector(1*((intercept_lin_scen2 + test_lin_set_scen2_X %*% slope_lin_scen2)>0.5)),'1','0')
lin_pred_test_error = mean(lin_pred_test_scen2 != test_lin_set_scen2_Y)
print("Scenario 2 - Testing Error")
print(lin_pred_test_error)