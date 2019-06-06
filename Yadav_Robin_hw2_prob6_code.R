# Problem 6: Linear Method for Classification: Scenario 1

library(MASS)
# Restoring Data
load("testing_two_class.RData")
load("training_two_class.RData")

# Scatter Plot for Scenario 1
plot(training_two_class[1:100, ], col="green", xlim=c(-2,5), ylim=c(-2,6), xlab = "x", ylab = "y", main="Two-Class Classification Problem Plot")
points(training_two_class[101:200, ], col="red") # points() used to plot on the existing plot
legend(x=3.5,y=6,legend=c("Class Green","Class Red"),cex=.8,col=c("green","red"),pch=c(1,1))
abline(a=0,b=1, col=5, lwd=3) #y=bx+a


# Training linear regression model with training data: Class 1=green, Class 2=red
y_lin_train = c(rep(1, 100), rep(0, 100))
train_lin_set_scen1 = training_two_class
#train_lin_set_scen1 = as.data.frame(train_lin_set_scen1)
#names(train_lin_set_scen1) = c("X1", "X2","Y")

lin_model_scen1 = lm(y_lin_train ~ train_lin_set_scen1)
summary(lin_model_scen1)
intercept_lin_scen1 = lin_model_scen1$coef[1]
slope_lin_scen1 = lin_model_scen1$coef[-1]
# # X2 = (-c1/c3) + (-c2/c3)*X1
abline(coef=c((intercept_lin_scen1-0.5)/(-slope_lin_scen1[2]),slope_lin_scen1[1]/(-slope_lin_scen1[2])), col=4,lty=2,lwd=3)


### Training Error : Linear Classifier greater than 0.5
lin_pred_train_scen1 =  as.vector(1*((intercept_lin_scen1 + train_lin_set_scen1%*%slope_lin_scen1)>0.5))
lin_pred_train_error = mean(lin_pred_train_scen1 != y_lin_train)
print(lin_pred_train_error)

### Testinng Error:Class 1=green, Class 2=red
y_lin_test = c(rep(1,500), rep(0,500))
test_lin_set_scen1 = testing_two_class
lin_pred_test_scen1 =  as.vector(1*((intercept_lin_scen1 + test_lin_set_scen1%*%slope_lin_scen1)>0.5))
lin_pred_test_error = mean(lin_pred_test_scen1 != y_lin_test)
print(lin_pred_test_error)




########################
# mymodel = glm(Y ~ X1+X2, data=training_two_label, family='binomial')
# summary(mymodel)
# 
# # Coefficient Obtained  from summary of the model
# # X2 = (-c1/c3) + (-c2/c3)*X1
# slope <- coef(mymodel)[2]/(-coef(mymodel)[3])
# intercept <- coef(mymodel)[1]/(-coef(mymodel)[3])
# abline(a=intercept,b=slope,col=4)
# legend(x=3.2,y=-1.2,legend=c("Bayes Decision","Linear Decision"),cex=.8,col=c(col=5,col=4),pch=c(20,20))
# 
# 
# training_two_label$log_n <- predict(mymodel,training_two_label)
# #test_set$log_n <- predict(model,test_set)
# 
# training_two_label <- training_two_label %>% mutate(log_v= ifelse(log_n > 0, 'red','green'))
# log_training_error <- nrow(subset(training_two_label, log_v!=Y))
# # test_set <- test_set %>% mutate(log_v = ifelse(log_n > 0, 'red','green'))
# # log_test_error_rate <- nrow(subset(test_set,log_v!=true_v))/nrow(test_set)











# ##### (c) training errors and testing error boundary decision line
# ### Counting Training Errors
# count_green_tr = 0
# for (i in 1:100){
#   if((training_two_class[i,2] + ((c2/c3)*training_two_class[i,1]) + (c1/c3)) < 0){
#     count_green_tr = count_green_tr + 1
#   }
# }
# count_red_tr = 0
# for (i in 101:200){
#   if((training_two_class[i,2] + ((c2/c3)*training_two_class[i,1]) + (c1/c3)) > 0){
#     count_red_tr = count_red_tr + 1
#   }
# }
# count_on_line = 0
# for (i in 1:200){
#   if((training_two_class[i,2] + ((c2/c3)*training_two_class[i,1]) + (c1/c3)) == 0){
#     count_on_line = count_on_line + 1
#   }
# }
# errorcount_train = (100 - count_green_tr) + (100 - count_red_tr)
# print("Training Error Count:")
# print(errorcount_train)
# 
# ### Counting Testing Errors
# count_green_ts = 0
# for (i in 1:500){
#   if((testing_two_class[i,2] + ((c2/c3)*testing_two_class[i,1]) + (c1/c3)) < 0){
#     count_green_ts = count_green_ts + 1
#   }
# }
# count_red_ts = 0
# for (i in 501:1000){
#   if((testing_two_class[i,2] + ((c2/c3)*testing_two_class[i,1]) + (c1/c3)) > 0){
#     count_red_ts = count_red_ts + 1
#   }
# }
# count_on_line_ts = 0
# for (i in 1:1000){
#   if((testing_two_class[i,2] + ((c2/c3)*testing_two_class[i,1]) + (c1/c3)) == 0){
#     count_on_line_ts = count_on_line_ts + 1
#   }
# }
# errorcount_test = (500 - count_green_ts) + (500 - count_red_ts)
# #print("Testing Error Count:")
# #print(errorcount_test)
#