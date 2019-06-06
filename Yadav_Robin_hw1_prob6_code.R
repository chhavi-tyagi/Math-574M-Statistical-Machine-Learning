# Problem 6

sink(file="myoutput")
x = 1:5
# this is x
print(x)
sink()
sink(file="myoutput", append=T)
# this is y
y = sum(x)
print(y)
sink()


