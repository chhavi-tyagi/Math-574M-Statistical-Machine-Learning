## Homework 2 Code

#################
## Question 3
#################

#equal cost function: c(0,1)=c(1,0)=1
eqmyf <- function(x)
{ dnorm(x,0,1)/(0.65*dnorm(x,1,1)+0.35*dnorm(x,-1,2))-1
}
#unequal cost: c(0,1)=3,c(1,0)=2
ueqmyf <- function(x)
{ dnorm(x,0,1)/(0.65*dnorm(x,1,1)+0.35*dnorm(x,-1,2))-3/2
}

#solution
library(rootSolve) #required by the function uniroot.all
eqroot <- uniroot.all(eqmyf,c(-1e+3,1e+3))
ueqroot <- uniroot.all(ueqmyf,c(-1e+3,1e+3))


#alternative methods
#equal cost
root1 <- uniroot(eqmyf,interval=c(-10,0))$root
root2 <- uniroot(eqmyf,interval=c(0,10))$root
#unequal cost
root3 <- uniroot(ueqmyf,interval=c(-10,0))$root
root4 <- uniroot(ueqmyf,interval=c(0,10))$root


#########################
## Question 4 (Scenario 1)
#########################

library(MASS) 
ntrain <- 100  #half of training sample size
ntest <- 500   #half of test sample size

##define the model parameters
mean1 <- c(2,1)
mean2 <- c(1,2)
Sig <- matrix(c(1,0,0,1),nrow=2)

## generate the training data: Class 1=green, Class 2=red
seed_train <- 2000
set.seed(seed_train)
train1 <- rbind(mvrnorm(ntrain,mean1,Sig),mvrnorm(ntrain,mean2,Sig))
ytrain <- c(rep(1,ntrain),rep(0,ntrain))

## generate the test data: Class 1=green, Class 2=red
seed_test <- 2014
set.seed(seed_test)
test1 <- rbind(mvrnorm(ntest,mean1,Sig),mvrnorm(ntest,mean2,Sig))
ytest <- c(rep(1,ntest),rep(0,ntest))


## Question 5 (Bayes Rule)
##Compute the training error for Bayes rule
predtrain <- 1*(train1[,1]>train1[,2])
baytrainerr1 <- mean(predtrain!=ytrain)
##Compute the test error for Bayes rule
predtest <- 1*(test1[,1]>test1[,2])
baytesterr1 <- mean(predtest!=ytest)
print(baytrainerr1)
print(baytesterr1)
## 0.215, 0.239


################################
## Question 6 (Least Squares)
################################

##train linear classifier with training data
linfit1 <- lm(ytrain~train1)
inter1 <- linfit1$coef[1]
slope1 <- linfit1$coef[-1]
## compute the training error
linpredtrain1 <- as.vector(1*((inter1+train1%*%slope1)>0.5))
lintrainerr1 <- mean(linpredtrain1!=ytrain)
## compute the test error
linpredtest1 <- as.vector(1*((inter1+test1%*%slope1)>0.5))
lintesterr1 <- mean(linpredtest1!=ytest)
print(lintrainerr1)
print(lintesterr1)
#0.22, 0.235

##################################
## draw the scatter plot, imposed with the bayes rule (solid line) and linear classifier 
##################################

#pdf("scene1.pdf")
par(mfrow=c(2,1))
plot(train1[1:ntrain,],xlab="X1",ylab="X2",xlim=range(train1[,1]),ylim=range(train1[,2]),col="green", main="Scenario 1: Training Data")
points(train1[(ntrain+1):(2*ntrain),],col="red")
abline(coef=c(0,1),lty=1,lwd=3) 
abline(coef=c((inter1-0.5)/(-slope1[2]),slope1[1]/(-slope1[2])),lty=2,lwd=3)



## intercept=0.30, slope=0.85
plot(test1[1:ntest,],xlab="X1",ylab="X2",xlim=range(test1[,1]),ylim=range(test1[,2]),col="green", main="Scenario 1: Test Data")
points(test1[(ntest+1):(2*ntest),],col="red")
abline(coef=c(0,1),lty=1,lwd=3)
abline(coef=c((inter1-0.5)/(-slope1[2]),slope1[1]/(-slope1[2])),lty=2,lwd=3)
#graphics.off()











