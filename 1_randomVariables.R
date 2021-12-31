dat <- read.csv("femaleMiceWeights.csv")

library(downloader)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- "femaleControlsPopulation.csv"
if (!file.exists(filename)) download(url,destfile=filename)
population <- read.csv(filename)
population <- unlist(population) # turn it into a numeric

control <- sample(population,12)
mean(control)

library(UsingR)
x <- father.son$fheight


n <- 100
library(rafalib)
nullplot(-5,5,1,30, xlab="Observed differences (grams)", ylab="Frequency")
totals <- vector("numeric",11)
for (i in 1:n) {
  control <- sample(population,12)
  treatment <- sample(population,12)
  nulldiff <- mean(treatment) - mean(control)
  j <- pmax(pmin(round(nulldiff)+6,11),1)
  totals[j] <- totals[j]+1
  text(j-6,totals[j],pch=15,round(nulldiff,1))
  if(i < 15) Sys.sleep(1) ##You can add this line to see values appear slowly
}

library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- basename(url)
download(url, destfile=filename)
x <- unlist( read.csv(filename) )
#1
m <- mean(x)
#2
set.seed(1)
s <- sample(x,5)
abs(mean(s) - m)
#3
set.seed(5)
s <- sample(x,5)
abs(mean(s) - m)
#5
set.seed(1)
n <- 1000
output <- vector("numeric", length=n)
for(i in 1:n){
  s <- sample(x, 50)
  output[i] <- mean(s)
}
mean(abs(output-m)>1)
hist(output, breaks = 15, xlim = range(max(output),min(output)))
mypar(1,2)

#9
mean(output>23 & output<25)
#10
pnorm(25, 23.9, .43) - pnorm(23, 23.9, .43)






