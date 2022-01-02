library(tidyverse)
library(rafalib)
library(downloader)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleMiceWeights.csv"
filename <- "femaleMiceWeights.csv"
if(!file.exists("femaleMiceWeights.csv")) download(url,destfile=filename)
dat <- read.csv(filename) 

#1
n <- 100
p <- 1/6
reps <- 10000
set.seed(1)
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

mypar(2,2)
zz <- map2(ns, ps, function(n,p){
  replicate(reps, {
    x=sample(1:1/p, n, replace=TRUE)
    z = (mean(x==1) - p) / sqrt(p*(1-p)/n)
  })
})
for(i in seq_along(zz)){
  title=paste0("N=", ns[i]," ", "p=", ps[i] )
  qqnorm(zz[[i]], main = title)
  qqline(zz[[i]], col = 2)
}

for(i in 1:4){
  p <- ps[i]
  sides <- 1/p
  n <- ns[i]
  zs <- replicate(10000,{
    x <- sample(1:sides,n,replace=TRUE)
    (mean(x==1) - p) / sqrt(p*(1-p)/n)
  }) 
  qqnorm(zs)
  abline(0,1)
}

#3
X <- filter(dat, Diet=="chow") %>% select(Bodyweight) %>% unlist
Y <- filter(dat, Diet=="hf") %>% select(Bodyweight) %>% unlist
savg <- mean(X)
#4
#answer D
#5
# answer is 0
#6
sd(X)
#7

x <- 5.21/(sd(X)/sqrt(12))
pnorm(-x) + (1-pnorm(x))


x <- 2/(sd(X)/sqrt(12))

pnorm(-x) + (1-pnorm(x))

#8
se <- sqrt( (var(X)/12) + var(Y)/12 )

#9
tstat <- abs( mean(X)-mean(Y) )/se

#11
1-pnorm(tstat)
1-pnorm(abs(tstat))
pnorm(-abs(tstat))

pnorm(-tstat) + (1-pnorm(tstat))
#12
t.test(X,Y)
