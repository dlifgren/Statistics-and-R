library(tidyverse)
library(downloader)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/mice_pheno.csv"
filename <- "mice_pheno.csv"
download(url,destfile=filename)
dat <- read.csv(filename)
dat <- na.omit( dat )
#1
x <- dat %>%
  filter(Sex == "M", Diet == "chow" ) %>%
  select(Bodyweight)%>%
  unlist()
mean(x)
#2
library(rafalib)
popsd(x)
#3
set.seed(1)
s <- sample(x, 25)
X <- mean(s)
#4
y <- dat %>%
  filter(Sex == "M", Diet == "hf" ) %>%
  select(Bodyweight)%>%
  unlist()
y <- mean(y)
#5
popsd(y)
#6
set.seed(1)
sm <- sample(y,25)
Y <- mean(sm)
#7
abs( (y-x) - (X-Y) )





