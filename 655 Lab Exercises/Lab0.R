## Change the working directory
setwd("C:\\Users\\Elizabeth\\Dropbox\\MLM2019\\Labs\\Lab0")

## Open the dataset
data <- read.table("hsb_data.csv",header=T,sep=",")

## 1) Data structure
N <- unlist(tapply(data$newid,data$newid,length))
data$N <- rep(N,N)
data$n <- unlist(tapply(data$newid,data$newid,FUN=function(x) seq(1,length(x))))
data[1:47,c("newid","N","n","ses","sector")]

## 2) School level variables
schools <- data[data$n==1, ]
table(schools$sector)
mean(schools$sector)
table(schools$himinty)
mean(schools$himinty)
table(data$sector)
mean(data$sector)
table(data$himinty)
mean(data$himinty)

my.fun = function(x) c(mean(x),sqrt(var(x)))
apply(schools,2,my.fun)


## Student level variables
apply(data,2,my.fun)
table(data$minority)
mean(data$minority)
table(data$female)
mean(data$female)

# Calculate summary statistics for the student level variables
# to compare the school composition
data$meanmathach = rep(unlist(tapply(data$mathach,data$newid,mean)),N)
data$meanses = rep(unlist(tapply(data$ses,data$newid,mean)),N)
data$propnonwhite = rep(unlist(tapply(data$minority,data$newid,mean)),N)
data$propfemale = rep(unlist(tapply(data$female,data$newid,mean)),N)
schools <- data[data$n==1,]
apply(schools,2,summary)



