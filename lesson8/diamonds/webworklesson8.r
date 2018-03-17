x = matrix( c(1,3,2, 6,NA,4), nr = 3 )
apply(x, 2, max)
x
apply(x, 2, max, na.rm=T)
apply(x, 1, min, na.rm=T)
?sort
apply(x, 2, sort)
?tapply
mydata = read.csv("C:/Users/matt/source/repos/ds710/lesson8/diamonds/AmesHousing.csv")
mydata
attach(mydata)
tapply(mydata$SalePrice, INDEX = mydata$Land.Slope, median)
df = data.frame(45:54) 

PearsonSkew <- function(x) {
  3*(mean(x,na.rm = T) - median(x,na.rm = T)) / sd(x,na.rm = T)
}
y = c( 1, 1, 2, 10 )
PearsonSkew(y)
PearsonSkew(Bedroom.AbvGr)
PearsonSkew(X1st.Flr.SF)
PearsonSkew(Bsmt.Full.Bath)
PearsonSkew(Full.Bath)
apply(ames2, 2, PearsonSkew)

