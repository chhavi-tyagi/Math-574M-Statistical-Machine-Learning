###  Code for Homework 3

##################
##### Problem 4
##################
library(MASS) 
ntrain <- 100  #half of training sample size
ntest <- 500   #half of test sample size

##generate training data: Class 1=green, Class 2=red
mean1 <- c(2,1)
mean2 <- c(1,2)
Sig <- matrix(c(1,0,0,1),nrow=2)
seed_train <- 2000
set.seed(seed_train)
train1 <- rbind(mvrnorm(ntrain,mean1,Sig),mvrnorm(ntrain,mean2,Sig))
ytrain <- c(rep(1,ntrain),rep(0,ntrain))
## generate the test data
seed_test <- 2014
set.seed(seed_test)
test1 <- rbind(mvrnorm(ntest,mean1,Sig),mvrnorm(ntest,mean2,Sig))
ytest <- c(rep(1,ntest),rep(0,ntest))

## fit lda
lda1 <- lda(train1,ytrain)
predtrain <- predict(lda1,train1)
predtest <- predict(lda1,test1)
ldatrainerr1 <- mean(predtrain$class!=ytrain)
ldatesterr1 <- mean(predtest$class!=ytest)

## fit logistic regression
train.data = as.data.frame(cbind(ytrain,train1))
names(train.data) = c("y",paste("x",1:2,sep=""))
test.data = as.data.frame(cbind(ytest,test1))
names(test.data) = c("y",paste("x",1:2,sep=""))
logistic1=glm(y~., data=train.data,family=binomial)
predtrain = predict(logistic1, newdata=train.data,type="response")
logtrainerr1=mean(round(predtrain)!=train.data$y)
predtest = predict(logistic1,newdata=test.data,type="response")
logtesterr1=mean(round(predtest)!=test.data$y)

## 
print("Scenario 1: Bayes and linear classifier errors")
errs1 <- matrix(0,2,2)
errs1[1,] <- c(ldatrainerr1, ldatesterr1)
errs1[2,] <- c(logtrainerr1,logtesterr1)
rownames(errs1) <- c("lda","logistic")
colnames(errs1) <- c("training error", "test error")
print(errs1)

#  training error test error
#lda               0.220      0.235
#logistic          0.215      0.236




###################
#### Problem 5
###################
##Scenario 2
library(MASS) 
ntrain <- 100  #half of training sample size
ntest <- 500   #half of test sample size

#Class 1=green, Class 2=red
#generate ten centers, which are treated as fixed parameters
seed_center <- 16
Sig <- matrix(c(1,0,0,1),nrow=2)
set.seed(seed_center)
center_green <- mvrnorm(n=10,c(1,0),Sig)
center_red <- mvrnorm(n=10,c(0,1),Sig)

##define a function "gendata2" first
gendata2 <-function(n,mu1,mu2,Sig1,Sig2,myseed)
{
  set.seed(myseed)
  mean1 <- mu1[sample(1:10,n,replace=T),]
  mean2 <- mu2[sample(1:10,n,replace=T),]
  green <- matrix(0,ncol=2,nrow=n)
  red <- matrix(0,ncol=2,nrow=n)
  for(i in 1:n){
    green[i,] <- mvrnorm(1,mean1[i,],Sig1)
    red[i,] <- mvrnorm(1,mean2[i,],Sig2)
  }
  x <- rbind(green,red)
  return(x)
}

#generate the training set
seed_train <- 2000
train2 <- gendata2(ntrain,center_green,center_red,Sig/5,Sig/5,seed_train)
ytrain <- c(rep(1,ntrain),rep(0,ntrain))
#generate the test set
seed_test <- 2014
test2 <- gendata2(ntest,center_green,center_red,Sig/5,Sig/5,seed_test)
ytest <- c(rep(1,ntest),rep(0,ntest))


################
## Problem 6
################
##fit linear classifier with training data 
linfit2 <- lm(ytrain~train2)
inter2 <- linfit2$coef[1]
slope2 <- linfit2$coef[-1]
## compute the training error 
linpredtrain2 <- as.vector(1*((inter2+train2%*%slope2)>0.5))
lintrainerr2 <- mean(linpredtrain2!=ytrain)
## compute the test error
linpredtest2 <- as.vector(1*((inter2+test2%*%slope2)>0.5))
lintesterr2 <- mean(linpredtest2!=ytest)
# (Intercept) 
#3   0.608955 
# slope2
#   train21    train22 
# 0.0739740 -0.1970626 


print(lintrainerr2)
#0.29
print(lintesterr2)
#0.284

## Data scatter plot, imposed with the  bayes rule (solid line) ##and linear classifier ##(dashed line)
#pdf("scenario2_linear.pdf")
par(mfrow=c(2,2))
plot(train2[1:ntrain,],xlab="X1",ylab="X2",xlim=range(train2[,1]),ylim=range(train2[,2]),col="green", main="Scenario 2: Training Data")
points(train2[(ntrain+1):(2*ntrain),],col="red")
abline(coef=c((inter2-0.5)/(-slope2[2]),slope2[1]/(-slope2[2])),lty=2,lwd=3)
# (Intercept)     train21 
#  0.5528955   0.3753833 
# x2 = 0.55+0.38 x1

plot(test2[1:ntest,],xlab="X1",ylab="X2",xlim=range(test2[,1]),ylim=range(test2[,2]),col="green", main="Scenario 2: Test Data")
points(test2[(ntest+1):(2*ntest),],col="red")
abline(coef=c((inter2-0.5)/(-slope2[2]),slope2[1]/(-slope2[2])),lty=2,lwd=3)
#graphics.off()
