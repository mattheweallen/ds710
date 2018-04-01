babydata = read.csv("C:/Users/irgepi/ds/ds710/lesson9/babyboom.csv")
head(babydata)
summary(babydata$Sex)
SexToFM <- function(x) {
  y <- rep.int(NA, length(x))
  y[x == "boy"] <- "M"
  y[x == "F"] <- "F"
  y[x == "female"] <- "F"
  y[x == "girl"] <- "F"
  y[x == "M"] <- "M"
  y[x == "Male"] <- "M"
  y
}

babydata$Sex <- SexToFM(babydata$Sex)
summary(babydata$Sex)

babydata = read.csv("C:/Users/irgepi/ds/ds710/lesson9/babyboom.csv")
attach(babydata)
head(babydata)
summary(babydata$Sex)
male = c("boy", "M", "Male")
female = c("F", "female", "girl")
Sex %in% male

SexToFM <- function(x) {
  male = c("boy", "M", "Male")
  female = c("F", "female", "girl")
  y <- rep.int(NA, length(x))
  y[Sex %in% male] = "M"
  y[Sex %in% female] = "F"
  y
}

CleanSex <- SexToFM(Sex)
Sex <- CleanSex
cbind(as.character(babydata$Sex), CleanSex)
class(Sex)
is.character(Sex)
summary(Sex)
class(Weight)
is.numeric(Weight)
summary(Weight)
class(Time)
summary(Time)
Time2 = as.numeric(Time)
summary(Time2)
Time2
Time
Time[1:5]
Time2[1:5]
Time2 <- as.numeric(as.character(Time))
#as.numeric(levels(f)[f])
#https://stackoverflow.com/questions/3418128/how-to-convert-a-factor-to-an-integer-numeric-without-a-loss-of-information
#https://cran.r-project.org/doc/FAQ/R-FAQ.html#How-do-I-convert-factors-to-numeric_003f

Time2
cbind(as.character(Time), Time2)
Time <- Time2
summary(Time)
hist(Time)
boxplot(Time)
Sex[which(Time > 2359)]
which(Time < 0)
Time[which(Time > 2359)] = NA
Time
summary(Time)
trunc(Time) == Time
which(   ( trunc(Time) == Time ) == F   )
which(Time %% 100 > 59)
Time[which(Time %% 100 > 59)] = NA
Time

Weight
summary(Weight)
hist(Weight)
boxplot(Weight)
Weight = babydata$Weight
class(Weight)
which(Weight < 1000)
Weight[which(Weight < 1000)] = NA
Weight
Time

summary(Time)
which( is.na( Time ) )
!is.na(Time)
Time3 = Time[!is.na(Time)]
summary(Time3)
sd(Time3)
sd(Time, na.rm = T)


#babydata = read.csv("C:/Users/irgepi/ds/ds710/lesson9/babyboom.csv")
#attach(babydata)
summary(Time)
mydata2 = data.frame(Time,Sex,Weight)
mydata2 = data.frame(Time,Sex,Weight)
mydata2
has_all_measurements <- complete.cases(mydata2)
nrow(mydata2[has_all_measurements,])
dim(mydata2[has_all_measurements,])
mydata3 <- mydata2[has_all_measurements,]
dim(mydata3)

#problem 15
babydata = read.csv("C:/Users/irgepi/ds/ds710/lesson9/babyboom.csv")
attach(babydata)
SexToFM <- function(x) {
  male = c("boy", "M", "Male")
  female = c("F", "female", "girl")
  y <- rep.int(NA, length(x))
  y[Sex %in% male] = "M"
  y[Sex %in% female] = "F"
  y
}
CleanSex <- SexToFM(Sex)
Sex <- CleanSex
Time <- as.numeric(as.character(Time))
Time[which(Time > 2359)] = NA
Time[which(Time %% 100 > 59)] = NA
Weight[which(Weight < 1000)] = NA
mydata2 = data.frame(Time,Sex,Weight)
has_all_measurements <- complete.cases(mydata2)
mydata3 <- (mydata2[has_all_measurements,])
dim(mydata3)
rm(Time,Sex,Weight)
mydata3

mydata4 <- within(
  mydata3,
  {
    morn.born <- Time < 1200
  }
)
mydata4$Time[sort(mydata4$Time)]

born_order <- order(mydata4$Time)
mydata4[born_order,]

x = c(4, 6, 5)
sort(x)
order(x)
