?apply
x = matrix(1:6,nr=3)
x
apply(x,1,sum) #row sums
apply(x,2,sum) #column sums
#mydata = read.csv("")
#dim(mydata)
#mydata[1:5, 1:5]
#prop0 <- function(x) {
#  # find proportion of 0's in a vector
#  return(length(which( x == 0)) / length(x))
#} # end of prop0
#zeroes <- apply(mydata, 2, prop0)
mydata = read.table("C:/Users/matt/source/repos/ds710/lesson8/diamonds/diamonds.txt", header=T)
attach(mydata)

EqualityTest <- function(x, alpha) {
  # use a chi-squared GOF test to test whether x could be a random sample from a population in which all values are equally likely in all 5 values are equally likely
  #alpha = significance level
  counts = table(x)
  mytest = chisq.test(counts, p=c(1/5,1/5,1/5,1/5,1/5)) #1/5 vector optional default test is equally likely
  
  #extract p-value
  pval = mytest$p.value
  
  if(pval < alpha) { print("not plausible") }
  else { print("plausible")}
}
EqualityTest(Clarity,  alpha =.01)
#source("c:/") #import funciton
