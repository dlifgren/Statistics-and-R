library(downloader)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleMiceWeights.csv"
filename <- "femaleMiceWeights.csv"
if(!file.exists("femaleMiceWeights.csv")) download(url,destfile=filename)
dat <- read.csv(filename) 

#1
n <- 100
p <- 1/6
reps <- 10000
t <- replicate(reps, {
  x=sample(1:6, n, replace=TRUE)
  z = (mean(x==6) - p) / sqrt(p*(1-p)/n)
})
mean(abs(t)>2)

#2
mypar(1,2)
qqnorm(t)
qqline(t)
hist(t)

ns <- c(5,30,30,100)
ps <- c(0.5,0.5, 0.01, 0.01)

z <- sapply(ns, function(n){
  replicate(reps, {
    x=sample(1:6, n, replace=TRUE)
    z = (mean(x==6) - p) / sqrt(p*(1-p)/n)
  })
})