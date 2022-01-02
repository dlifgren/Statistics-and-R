#1
set.seed(1)
s <- rnorm(5)
tstat <- sqrt(5)*mean(s)/sd(s)

#2
set.seed(1)
tg <- function(n){
  s <- rnorm(n)
  tstat <- sqrt(n)*mean(s)/sd(s)
  return(tstat)
}
ttest <- replicate(1000, tg(5))



#3
B =100
ps = seq(1/(B+1), 1-1/(B+1),len=B)
q <- qt(ps,df=4)

mypar(2,2)
reps <- c(5,10,20,40)
for(i in seq_along(reps)){
  ttest <- replicate(1000, tg(reps[i]))
  qqplot(q, ttest, xlim = c(-6,6), ylim = c(-6,6))
  qqline(ttest)
}
qqplot(q, ttest)
qqline(mc)
reps