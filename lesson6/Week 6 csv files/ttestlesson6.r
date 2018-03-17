ls()
getwd()
setwd("C:/Users/matt/source/repos/ds710/lesson6/Week 6 csv files")
cars = read.csv("Cars 2005.csv")
attach(cars)
hist(Price,las=1,cex.axis=1.4,cex.lab=1.4,cex.main=1.4,col="lightblue")
t.test(Price,mu=24721.86,alternative="less")
t.test(Price ~ Doors, alternative="two.sided") #Price is the mean testing, Doors is how to divide up the mean into groups
#conf.level for level of confidence
unemployment = read.csv("Unemployment_rates.csv")
attach(unemployment)
t.test(Rate_2013, Rate_2014, paired = T, alternative = "greater")

#more that just t test
#test for proportions in R
length(Cruise)
numCruise = length(which(Cruise == 1))
numCruise
totalCars = length(Cruise)
totalCars
prop.test(numCruise, n = totalCars, p = .5, alternative="greater")
#test for difference in proportions
prop.test(c(150,455), n=c(190,614),alternative="two.sided")
#chi squared
typeCount = summary(Type)
typeCount
prop2014 = c(.022,.073,.118,.761,.026)
prop2014 * 804
chisq.test(typeCount, p=prop2014)
summary(Cylinder)
counts = table(Cylinder)
counts
counts = summary(as.factor(Cylinder))
counts
chisq.test(counts)
#linear regression
plot(Mileage,Price)
model = lm(Price ~ Mileage)
model
abline(model, col="red",lwd=2)
predict(model, list(Mileage=20000))
cars[345, 1:4]
par(mfrow=c(2,2)) #prepare for 4 plots
plot(model)
logPrice = log(Price)
model2=lm(logPrice~Mileage)
plot(model2)
model2
lm(formula = logPrice ~ Mileage)
summary(model2)
a = summary(model2)
a$coefficients
a$coeff[1,1]
a$coeff[2,4]
