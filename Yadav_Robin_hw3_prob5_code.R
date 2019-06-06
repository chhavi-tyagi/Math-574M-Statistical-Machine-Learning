# Problem 5: Two-Class Classifcation Problems: Scenario 2

# Generate a training set of n = 200 from a mixture data as follows.
# 
# step 1: Generate 10 points µk, k = 1, ..., 10 from a bivariate Gaussian distribution N((1, 0)T, I). 
# They will be used as means (centers) to generate the Green class for both training and
# test data.
# 
# step 2: Generate 10 points ??k, k = 1, ..., 10 from a bivariate Gaussian distribution N((0, 1)T, I).
# They will be used as means (centers) to generate the Red class.
# 
# step 3: For the Green class, generate 100 observations as follows: for each observation, randomly
# pick a µk with probability 1/10, and then generate a point from N(µk, I/5).
# 
# step 4: For the Red class, generate 100 observations as follows: for each observation, randomly
# pick a ??k with probability 1/10, and then generate a point from N(??k, I/5).


library(MASS)

##generate ten centers, which are treated as fixed parameters
Sig = matrix(c(1,0,0,1), nrow=2)
seed_center = 16
set.seed(seed_center)
center_green = mvrnorm(n=10, c(1,0), Sig)
center_red = mvrnorm(n=10, c(0,1), Sig)

##define a function "gendata2" first
gendata2 = function(n, mu1, mu2, Sig1, Sig2, myseed){
  set.seed(myseed)
  mean1 = mu1[sample(1:10, n, replace=T), ]
  mean2 = mu2[sample(1:10, n, replace=T), ]
  green = matrix(0, ncol=2, nrow=n)
  red = matrix(0, ncol=2, nrow=n)
  
  for(i in 1:n){
    green[i, ] = mvrnorm(1, mean1[i, ], Sig1)
    red[i, ] = mvrnorm(1, mean2[i, ], Sig2)
  }
  
  x = rbind(green,red)
  return(x)
  
}

## generating training set
seed_train =  2000
ntrain = 100
train2 = gendata2(ntrain, center_green, center_red, Sig/5, Sig/5, seed_train)
ytrain = c(rep(1, ntrain), rep(0, ntrain))
#Saving train data scenario#2 for future
trainSet_Scen2_two_class = cbind(train2, ytrain)
#trainSet_Scen2_two_class = as.data.frame(trainSet_Scen2_two_class)
#names(trainSet_Scen2_two_class) = c("X1_train", "X2_train", "Y_train")
save(trainSet_Scen2_two_class, file="trainSet_Scen2_two_class.RData")


# (b) Scatter Plot for Scenario 2
plot(train2[1:100, ], col="green", xlim=c(-2,4), ylim=c(-2,5), xlab = "X1", ylab = "X2", main="Two-Class Scenario 2- Training Data")
points(train2[101:200, ], col="red") # points() used to plot on the existing plot
legend(x=2.8,y=5,legend=c("Class Green","Class Red"),cex=.8,col=c("green","red"),pch=c(1,1))


## (c) generating testing set
seed_test =  2014
ntest = 500
test2 = gendata2(ntest, center_green, center_red, Sig/5, Sig/5, seed_test)
ytest = c(rep(1, ntest), rep(0, ntest))
#Saving test data  scenario#2 for future
testSet_Scen2_two_class = cbind(test2, ytest)
#testSet_Scen2_two_class = as.data.frame(testSet_Scen2_two_class)
#names(testSet_Scen2_two_class) = c("X1_test", "X2_test","Y_test")
save(testSet_Scen2_two_class, file="testSet_Scen2_two_class.RData")
