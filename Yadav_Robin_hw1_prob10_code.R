# Problem 10

# help(data)
data(cars)
#cars
attach(cars)

sink(file="Yadav_Robin_prob10")
mu = mean(dist)
print(mu)
sink()
sink(file="Yadav_Robin_prob10", append=T)
sig = var(dist)
print(sig)
sink()
pdf(file="Yadav_Robin_prob10_hist.pdf")
hist(dist)
graphics.off()