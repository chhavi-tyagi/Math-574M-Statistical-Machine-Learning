# Problem 5: Bayes Classification Rule: Scenario 1


library(MASS)
##### (b) Bayes Decision Boundary

# Restoring Data
load("training_two_class.RData")
load("testing_two_class.RData")

# Plot for Scenario 1
plot(training_two_class[1:100, ], col="green", xlim=c(-2,5), ylim=c(-2,6), xlab = "x1", ylab = "x2", main="Two-Class Classification Problem Plot")
points(training_two_class[101:200, ], col="red") # points() used to plot on the existing plot
legend(x=3.5,y=6,legend=c("Class Green","Class Red"),cex=.8,col=c("green","red"),pch=c(1,1))

# Bayes Decision Boundary X = Y
abline(a=0,b=1) #y=bx+a

## Training Error : Class 1=green, Class 2=red
ybay_train = c(rep(1,100), rep(0,100))
train_set_scen1 = cbind(training_two_class, ybay_train)
ybay_train_pred = 1*(train_set_scen1[,1]>train_set_scen1[,2])
ybay_train_erros = mean(ybay_train_pred!=train_set_scen1[,3])
print(ybay_train_erros)



## Testing Error : Class 1=green, Class 2=red
ybay_test = c(rep(1,500), rep(0,500))
test_set_scen1 = cbind(testing_two_class, ybay_test)
ybay_test_pred = 1*(test_set_scen1[,1]>test_set_scen1[,2])
ybay_test_erros = mean(ybay_test_pred!=test_set_scen1[,3])
print(ybay_test_erros)



# ######### Training Error for Bayes Decisicion Boundary ############
# training_dataset = cbind(training_two_class[,1], training_two_class[,2])
# training_dataset = as.data.frame(training_dataset)
# names(training_dataset) = c('X1_train','X2_train')
# 
# training_dataset$y_train = rep(c('green','red'),each=100)
# training_dataset = training_dataset %>% mutate(bayes_v = ifelse(X1_train > X2_train,'green','red'))
# bayes_train_error = nrow(subset(training_dataset,bayes_v!=y_train))
# 
# 
# ######### Testing Error for Bayes Decision Boundary #############
# testing_dataset = cbind(testing_two_class[,1], testing_two_class[,2])
# testing_dataset = as.data.frame(testing_dataset)
# names(testing_dataset) = c('X1_test','X2_test')
# 
# testing_dataset$y_test = rep(c('green','red'),each=500)
# 
# testing_dataset = testing_dataset %>% mutate(bayes_v = ifelse(X1_test > X2_test,'green','red'))
# bayes_test_error= nrow(subset(testing_dataset,bayes_v!=y_test))



##########################################################################################

# 
# 
# ##### (c) training errors and test error # x - y = 0, boundary decision line
# ### Counting Training Errors
# count_green_tr = 0
# for (i in 1:100){
#   if(((training_two_class[i,1]) - (training_two_class[i,2])) > 0){
#      count_green_tr = count_green_tr + 1
#   }
# }
# count_red_tr = 0
# for (i in 101:200){
#   if(((training_two_class[i,1]) - (training_two_class[i,2])) < 0){
#     count_red_tr = count_red_tr + 1
#   }
# }
# count_on_line = 0
# for (i in 1:200){
#   if(((training_two_class[i,1]) - (training_two_class[i,2])) == 0){
#     count_on_line = count_on_line + 1
#   }
# }
# errorcount_train = (100 - count_green_tr) + (100 - count_red_tr) 
# print("Training Error Count:")
# print(errorcount_train)
# 
# 
# ### Counting Testing Errors
# count_green_ts = 0
# for (i in 1:500){
#   if(((testing_two_class[i,1]) - (testing_two_class[i,2])) > 0){
#     count_green_ts = count_green_ts + 1
#   }
# }
# count_red_ts = 0
# for (i in 501:1000){
#   if(((testing_two_class[i,1]) - (testing_two_class[i,2])) < 0){
#     count_red_ts = count_red_ts + 1
#   }
# }
# count_on_line_ts = 0
# for (i in 1:1000){
#   if(((testing_two_class[i,1]) - (testing_two_class[i,2])) == 0){
#     count_on_line_ts = count_on_line_ts + 1
#   }
# }
# errorcount_test = (500 - count_green_ts) + (500 - count_red_ts) 
# print("Testing Error Count:")
# print(errorcount_test)
