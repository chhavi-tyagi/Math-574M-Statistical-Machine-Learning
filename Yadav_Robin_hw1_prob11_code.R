# Problem 11

a = matrix(rnorm(20), 5, 4)
# a
b = a[1,]
# b
c = a[, 2:4]
# c
d = a[-c(1:2), ]
# d
## Saving a, b, c, d in a file
result = list(a, b, c, d)
# library(gridExtra)
# pdf("Yadav_Robin_prob11Data.pdf")
# grid.table(result)
# dev.off()

sink(file="Yadav_Robin_prob11")
print(result)
sink()