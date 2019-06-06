# Problem 7

set.seed(2014)
x = rnorm(10, mean=0, sd=1)
print(x)

## Saving plot in pdf
pdf(file="xdensity.pdf")
plot(density(x))
graphics.off()