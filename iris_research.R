

## Fisher's Iris Data

library(MASS)

iris = data.frame(rbind(iris3[,,1], iris3[,,2], iris3[,,3]), Species = rep(c("s", "c", "v"), rep(50,3)))
train = sample(1:150, 50)
table(iris$Species[train])
z = lda(Species ~ ., iris, prior = c(1,1,1)/3, subset=train)
#
ytrain = predict(z, iris[train, ])$class
table(ytrain, iris$Species[train])
train_error = mean(ytrain!=iris$Species[train])
# # 
# # ytest = predict(z, iris[-train, ])$class
# # table(ytest, iris$Species[-train])
# # test_error = mean(ytest!=iris$Species[-train])

# tr <- sample(1:50, 25)
# train <- rbind(iris3[tr,,1], iris3[tr,,2], iris3[tr,,3])
# test <- rbind(iris3[-tr,,1], iris3[-tr,,2], iris3[-tr,,3])
# cl <- factor(c(rep("s",25), rep("c",25), rep("v",25)))
# z <- lda(train, cl)
# predict(z, test)$class