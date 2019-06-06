# Problem 4: Two-Class Classification Problem: Scenario 1

library(MASS)
# (a)
set.seed(2000)
a = c(2,1)
mu_1 = t(a)
sig_1 = matrix(c(1,0,0,1), nrow=2)
bi_var_gauss_1  = mvrnorm(100, mu_1, sig_1)
#plot(density(bi_var_gauss_1))
b = c(1,2)
mu_2 = t(b)
sig_2 = matrix(c(1,0,0,1), nrow=2)
bi_var_gauss_2  = mvrnorm(100, mu_2, sig_2)

training_two_class = rbind(bi_var_gauss_1, bi_var_gauss_2)
#Save training dataset for future
save(training_two_class, file="training_two_class.RData")

# (b)
plot(bi_var_gauss_1, col="green", xlim=c(-2,5), ylim=c(-2,6), xlab = "x", ylab = "y", main="Two-Class Classification Problem Plot")
points(bi_var_gauss_2, col="red") # points() used to plot on the existing plot
legend(x=3.5,y=5.5,legend=c("Class Green","Class Red"),cex=.8,col=c("green","red"),pch=c(1,1))

# (c) Generating Test dataset for future
set.seed(2014)
a = c(2,1)
mu_test1 = t(a)
sig_test1 = matrix(c(1,0,0,1), nrow=2)
bi_var_gauss_test1  = mvrnorm(500, mu_test1, sig_test1)

b = c(1,2)
mu_test2 = t(b)
sig_test2 = matrix(c(1,0,0,1), nrow=2)
bi_var_gauss_test2  = mvrnorm(500, mu_test2, sig_test2)
testing_two_class = rbind(bi_var_gauss_test1, bi_var_gauss_test2)
#Saving testing data for future
save(testing_two_class, file="testing_two_class.RData")