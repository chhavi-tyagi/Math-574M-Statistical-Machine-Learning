# Problem 9

x = seq(0, 10, 0.5)
# x
y = seq(2, 12, 0.5)
# y

## Saving scatter plot in pdf
pdf(file="scatterplot.pdf")
plot(x, y)
lines(x, y)
abline(a=0, b=1)
graphics.off()