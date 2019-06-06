# Problem 5: Classify "1", "2" and "3" for the zip code using the classification tree in R. Report both the training error
# and test error. You may consider using the R package tree.


## installing package
# install.packages('tree')
library(tree)

## Normalize function ##
data_norm <- function(x){
  return ((x-min(x)) / (max(x)-min(x)))
}

####### Reading Data from zip file #######
train_zipcode_data <- read.table("zip.train.gz")
train_data_1_2_3_only <- train_zipcode_data[train_zipcode_data[,1] %in% c(1,2,3), ]
X_train <- sapply(train_data_1_2_3_only[,-1], data_norm) # Normalizing training data
train_data <- cbind(train_data_1_2_3_only[,1], X_train)
train_data <- as.data.frame(train_data)
train_data$V1 <- as.factor(train_data$V1)

test_zipcode_data <-  read.table('zip.test.gz')
test_data_1_2_3_only <- test_zipcode_data[test_zipcode_data[,1] %in% c(1,2,3), ]
X_test <- sapply(test_data_1_2_3_only[,-1], data_norm) # Normalizing testing data
test_data <- cbind(test_data_1_2_3_only[,1], X_test)
test_data <- as.data.frame(test_data)
test_data$V1 <- as.factor(test_data$V1)


## Classification Tree ##

## fit the tree with split="deviance
set.seed(12)
tree1 <- tree(V1~. , data=train_data, split='deviance')
info1 <- summary(tree1)
# print(info1)
# tree2 <- tree(V1~. , data=train_data, split='gini')
# info2 <- summary(tree2)
# print(info2)

# compute the training error
train_pred1 <- predict(tree1, train_data[-1], type="class")
train_err <- mean(train_pred1 != train_data[,1])
cat('\n')
print('Training Error for Classification Tree:')
print(train_err)


# compute the testing error
test_pred1 <- predict(tree1, test_data[,-1], type="class")
test_err <- mean(test_pred1 != test_data[,1])
cat('\n')
print('Testing Error for CLassification Tree:')
print(test_err)

cat('\n')




