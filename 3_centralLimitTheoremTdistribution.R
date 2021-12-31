library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/mice_pheno.csv"
filename <- basename(url)
download(url, destfile=filename)
dat <- na.omit( read.csv(filename) )
#1
pnorm(1)-pnorm(-1)
#2
pnorm(2)-pnorm(-2)
#3
pnorm(3)-pnorm(-3)
#4
y <- filter(dat, Sex == "M", Diet == "chow")%>%
  pull(Bodyweight)

t <- ( y - mean(y) )/popsd(y)
mean(abs(t)<=1)
#5
mean(t<=2 & t>=-2)
#6
mean(abs(t)<=3)
#7
mypar(1,1)
qqnorm(y)
qqline(y)
answer:C
#8
MC <- filter(dat, Sex == "M", Diet == "chow")%>%
  pull(Bodyweight)
MT<- filter(dat, Sex == "M", Diet == "hf")%>%
  pull(Bodyweight)
FC<- filter(dat, Sex == "F", Diet == "chow")%>%
  pull(Bodyweight)
FT <- filter(dat, Sex == "F", Diet == "hf")%>%
  pull(Bodyweight)
mypar(2,2)
t <- list("Male Control" = MC, "Male Treatment" = MT, "Female Control" = FC, "Female Treatment" = FT)
for(i in seq_along(t)){
  qqnorm(t[[i]], main = names(t[i]))
  qqline(t[[i]])
}
#9
set.seed(1)
y <- filter(dat, Sex == "M", Diet == "chow")%>%
  pull(Bodyweight)
avgs <- replicate(1000, mean(sample(y,25)))
mypar(1,2)
hist(avgs)
qqnorm(avgs)
qqline(avgs)
mean(avgs)
#10
sd(avgs)
#11
#according to CLT, average of population (y) is same as average of samples (avgs)
mean(y)
mean(avgs)
#likewise, standard deviation of samples (avgs) is the same as standard dev of pop(y)/sqrt(25)
sd(avgs)
popsd(y)/sqrt(25)
#12
set.seed(1)
stds <- replicate(10000, sd((sample(y,25))))
hist(stds)
mean(stds<3.5)
#13
x=seq(0.0001,0.9999,len=300)
q <- qnorm(x)
q <- qnorm(x, mean(x), popsd(x))
hist(q)
qt <- qt(x, 30)
hist(qt)
qqnorm(q)
qqline(q)
qqnorm(qt)
qqline(qt)
#answer = C
