xs = seq(-3, 3,1.5)
n=length(xs)
set.seed(2)
ys = rnorm(n, mean=xs * 1 + 0.5, sd=1) 
plot(xs,ys, xlim=c(-6,6), ylim=c(-10,10), xlab='x',ylab='mu|x', main='Confidence Intervalls for mu', sub = '200 random betas')
#plot(xs,ys, xlim=c(-6,6), ylim=c(-10,10), xlab='x',ylab='')
l = lm(ys ~ xs)
abline(l, col='green', lwd=2)
d = summary(l)
mean = d$coefficients[,1]
sds = d$coefficients[,2]
for (i in 1:100){
  beta0 = rnorm(n = 1, mean = mean[1], sds[1]) #Just for demo, in real t distributed     
  beta1 = rnorm(1, mean=mean[2], sds[2]) #Just for demo in real t-distributed
  lines(seq(-10,10,0.1), beta0 + beta1 * seq(-10,10,0.1), col='lightgrey')
  abline(l, col='green', lwd=2)
  points(xs,ys, xlim=c(-4,4), ylim=c(-10,10))
}


datanew <- data.frame(xs = seq(-6,6,by = 0.1))
pred1 <- predict(l, newdata = datanew, interval = "confidence", level=0.9) 
pred2 <- predict(l, newdata = datanew, interval = "prediction", level=0.9)
lines(datanew$xs, pred1[,2], col = "blue") 
lines(datanew$xs, pred1[,3], col = "blue") 
lines(datanew$xs, pred2[,2], col = "red") 
lines(datanew$xs, pred2[,3], col = "red")
