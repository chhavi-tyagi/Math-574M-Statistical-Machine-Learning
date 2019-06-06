# Problem 8

# library(MASS)
# help(mvrnorm)
mean1 = c(2,1)
cov1 = matrix(c(1,0,0,1),nrow=2)
data1 = mvrnorm(5,mean1,cov1)
# data1

# Save data1 to pdf
library(gridExtra)
pdf(file="Yadav_Robin_prob8_data1.pdf", height=10, width=8)
grid.table(data1)
dev.off()


