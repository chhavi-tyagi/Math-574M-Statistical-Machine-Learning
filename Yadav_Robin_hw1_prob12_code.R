# Problem 12



mymodel = lm(dist ~ speed)
summary(mymodel)
### draw the scatter plot
pdf(file="linearRegressionModel.pdf")
plot(cars, main="Stopping Distance versus Speed")
### draw the fitted regression line (red)
#lines(speed, fitted(mymodel), type="l", lty=1, col=2)
### draw a smooth line through a scatter plot (green)
lines(stats::lowess(cars), type="l", lty=2, col=3)
detach(cars)
graphics.off()