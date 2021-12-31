par(mfrow = c(3,3))
for(i in 1:9){
  qqnorm(dat[,i])
}

qqnorm(dat[,1])

boxplot(InsectSprays$count ~ InsectSprays$spray)

boxplot(split(InsectSprays$count, InsectSprays$spray))

dat <- nym.2002
par(mfrow=c(1,2))
male <- dat%>%
  filter(gender=="Male")
hist(male$time, main = "Male")
female <- dat%>%
  filter(gender=="Female")
  hist(dat$time, main = "Female")
boxplot(dat$time~ dat$gende)

mypar(1,3)
males <- filter(nym.2002, gender=="Male") %>% select(time) %>% unlist
females <- filter(nym.2002, gender=="Female") %>% select(time) %>% unlist
boxplot(females, males)
hist(females,xlim=c(range( nym.2002$time)))
hist(males,xlim=c(range( nym.2002$time)))

library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- basename(url)
download(url, destfile=filename)
x <- unlist( read.csv(filename) )

RNGkind("Mersenne-Twister", "Inversion", "Rejection")
mean(x)
s <- sample(x, 5)
abs(mean(s)-mean(x))

library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- basename(url)
download(url, destfile=filename)
x <- unlist( read.csv(filename) )

set.seed(1)
s <- vector("numeric", 10000)
for(i in 1:10000){
  y <- sample(x,5)
  s[i] <- mean(y)
}
mean(abs(s-mean(x))>1)

install.packages("gapminder")
library(gapminder)
data(gapminder)
head(gapminder)

x <- gapminder %>%
  filter(year == "1952") %>%
  select(country, lifeExp) %>%
  pull(lifeExp, name = country)

par(mfrow=c(1,1))
hist(x)  
mean(x <=40)
  
prop = function(q) {
  mean(x <= q)
}  
qs = seq(from=min(x), to=max(x), length=20)    
props = sapply(qs, prop)
plot(qs, props)
props = sapply(qs, function(q) mean(x <= q))
plot(ecdf(x))

library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- basename(url)
download(url, destfile=filename)
x <- unlist( read.csv(filename) )

set.seed(1)
n <- 1000
avg50 <- vector("numeric", n)
for(i in 1:n){
  y <- sample(x,50)
  avg50[i] <- mean(y)
}

par(mfrow = c(1,2))
hist(avg5)
hist(avg50)

avg <- 23.9
sigma <- 0.43
p23 <- pnorm(23, avg, sigma)
p25 <- pnorm(25, avg, sigma)
p25-p23

library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/mice_pheno.csv"
filename <- basename(url)
download(url, destfile=filename)
dat <- read.csv(filename) 
dat <- na.omit( dat )


x <- dat%>%
  filter(Diet == "chow", Sex == "F")%>%
  pull(Bodyweight)
set.seed(2)
X <- sample(x, 25)%>%
  mean()

y <- dat %>%
  filter(Diet == "hf", Sex == "F")%>%
  pull(Bodyweight)
set.seed(2)
Y <- sample(y, 25)%>%
  mean()

x_avg <- mean(x)
y_avg <- mean(y)
diff <- abs((y_avg-x_avg) - (Y-X))
diff

library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/mice_pheno.csv"
filename <- basename(url)
download(url, destfile=filename)
dat <- na.omit( read.csv(filename) )

y <- dat%>%
  filter(Diet == "chow", Sex == "M")%>%
  pull(Bodyweight)

z <- pnorm(y, mean(y), popsd(y))
mean(z<pnorm(1) & z>pnorm(-1))
#this is the same
t <- ( y - mean(y) )/popsd(y)
mean(abs(t)<=1)

mypar(1,1)
qqnorm(t)
abline(0,1)

y <- filter(dat, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist
set.seed(1)
avgs <- replicate(10000, mean( sample(y, 25)))
mypar(1,2)
hist(avgs)
qqnorm(avgs)
qqline(avgs)
mean(avgs)
x <- popsd(avgs)


library(downloader)
filename <- "femaleMiceWeights.csv"
dat <- read.csv(filename)


#Q1
set.seed(1)
n <- 100
sides <- 6
p <- 1/sides
zs <- replicate(10000,{
  x <- sample(1:sides,n,replace=TRUE)
  (mean(x==6) - p) / sqrt(p*(1-p)/n)
}) 
qqnorm(zs)
abline(0,1)#confirm it's well approximated with normal distribution
mean(abs(zs) > 2)
#Q2
ps <- c(0.5,0.5,0.01,0.01)
ns <- c(5,30,30,100)
library(rafalib)
mypar(4,2)
for(i in 1:4){
  p <- ps[i]
  sides <- 1/p
  n <- ns[i]
  zs <- replicate(10000,{
    x <- sample(1:sides,n,replace=TRUE)
    (mean(x==1) - p) / sqrt(p*(1-p)/n)
  }) 
  hist(zs,nclass=7)
  qqnorm(zs)
  abline(0,1)
}
#Q3
X <- filter(dat, Diet=="chow") %>% select(Bodyweight) %>% unlist
Y <- filter(dat, Diet=="hf") %>% select(Bodyweight) %>% unlist
mean(X)


