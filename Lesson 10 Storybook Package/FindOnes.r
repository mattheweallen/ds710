FindOnes <- function(x){
  # Extracts rows of x with 1's in columns 10-12
  y = NULL
  num.rows = dim(x)[1]
  for( i in 1:num.rows ){
    num.ones = length(which( x[i, 10:12] == 1 ))
    if(num.ones > 0){
      y = rbind( y, x[i,] )
    } # end "if there are ones"
  } # end iteration over rows of x
  return(y)
} # end function FindOnes

FindOnes.Fast <- function(x){
  # Extracts rows of x with 1's in columns 10-12
  y = x
  j = 1
  num.rows = dim(x)[1]
  for( i in 1:num.rows ){
    num.ones = length(which( x[i, 10:12] == 1 ))
    if(num.ones > 0){
      y[j,] = x[i,]
      j = j + 1
    } # end "if there are ones"
  } # end iteration over rows of x
  return(y[1:(j-1),])
} # end function FindOnes.Fast


#from video
x = runif(10000000)
system.time(sqrt(x))

MySqrt <- function(x) {
  for(value in x) {
    sqrt(value)
  }
}
system.time(MySqrt(x))

install.packages("microbenchmark")
library(microbenchmark)
x = runif(50)
x
microbenchmark(
  sqrt(x),
  MySqrt(x)
  
)

timings = microbenchmark(
  sqrt(x),
  MySqrt(x)
  
)
boxplot(timings, las = 1) #las = 1 axis labels horizontal

#Note the log scale. The log scale can be changed using:
  
#log = F

#use existing functions

#use vectors

#dnorm

#user defined
my.dnorm <- function(x, mu, sigma) {
  (1/(sigma*sqrt(2*pi)))*exp(-(x-mu)^2/(2*sigma^2))
}

my.dnorm(1.5,0,1)
dnorm(1.5,0,1)

system.time(for(i in 100000) dnorm(1.5,0,1))
system.time(for(i in 100000) my.dnorm(1.5,0,1))

timings = microbenchmark(dnorm(1.5,0,1), my.dnorm(1.5,0,1))
timings
boxplot(timings)

#nrows in read in files, nrows can be an estimate
#preallocate vectors and matrices and tell it what type

power_of_3 = numeric(10)

my.powerof3 <- function(x) {

  for(i in 1:length(x)) {
    x[i] = 3^i
  }
  x
}

my.slower.powerof3 <- function(n) {
  x = 3
  for(i in 2:n) {
    x = c(x,x[i-1]*3)
  }
  x
}

my.powerof3(power_of_3)
my.slower.powerof3(10)

microbenchmark(my.powerof3(power_of_3), my.slower.powerof3(10))


ames_housing = read.csv("C:/Users/irgepi/ds/ds710/Lesson 10 Storybook Package/AmesHousing.csv")
ames2 = as.matrix(ames_housing)
ames2
#Bsmt Qual, Heating QC, Kitchen Qual

nrow0 <- function(x) dim(x)[1]
ncol0 <- function(x) dim(x)[2]

num.rows = dim(ames2)[1]

quality <- ames2[i,c(32,42,55)]
quality
quality = quality[ !is.na( quality ) ] #remove any missing values
at.least.one.Excellent <- any(quality == "Ex")
at.least.one.Excellent

y = matrix( , nr = 2930, nc = 82)
num.rows = dim(ames2)[1]
# for(i in 1:num.rows){
#   quality = ames2[ i, c(32,42,55) ]
#   quality = quality[ !is.na(quality) ]
#   at.least.one.Excellent = any( quality == "Ex" )
#   if( at.least.one.Excellent ){
#     Copy ames2[i, ] into the next empty row of y
#   }
# }

j = 1
y = matrix( , nr = 2930, nc = 82 )
num.rows = dim(ames2)[1]
for(i in 1:num.rows){
  quality = ames2[ i, c(32,42,55) ]
  quality = quality[ !is.na(quality) ]
  at.least.one.Excellent = any( quality == "Ex" )
  if( at.least.one.Excellent ){
    #Copy ames2[ i, ] into the next empty row of y
    y[ j, ] = ames2[ i, ]
    j = j + 1
  }
}

#y[ 1: (j-1), ] 


j = 1
y = matrix( , nr = 2930, nc=82 )
num.rows = dim(ames2)[1]
for(i in 1:num.rows){
  quality=ames2[ i, c(32,42,55) ]
  quality = quality[ !is.na(quality) ]
  at.least.one.Excellent = any(quality == "Ex")
  if( at.least.one.Excellent ){
    y[ j, ] = ames2[ i, ]
    j = j+1
  }
}
ames_subset = y[ 1: (j-1), ]
ames_subset
