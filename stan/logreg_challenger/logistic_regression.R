#https://mc-stan.org/docs/2_21/stan-users-guide/logistic-probit-regression-section.html
#setwd("~/Documents/workspace/da/stan/logreg_challenger/")
library(ggplot2)
challenger = read.csv('challenger.txt', header = TRUE)
Temp = challenger$Temp
Failure = challenger$Failure
qplot(Temp, Failure)
erg<-glm(as.factor(Failure)~Temp, family=binomial(logit), data = challenger)
summary(erg)
a<-erg$coefficients[1]
b<-erg$coefficients[2]
x<-seq(30,100,2)
fit = data.frame(x = x, y = exp(a+b*x)/(1+exp(a+b*x)))
#library(ggstance)
df = data.frame(x=Temp, y=Failure)
ggplot(df) + 
  geom_point(aes(x=x,y=Failure),alpha=0.6) + 
  geom_line(data = fit, aes(x = x, y = y), col='blue')


###########
# Bayesian Version
library('rstan')
model = stan_model(file='log_reg.stan')
log.samples = sampling(model, list(N=23L,x=challenger$Temp, y=challenger$Failure, N2=length(x), x2=x))
traceplot(log.samples)
print(log.samples)
d = extract(log.samples)
dens(d$alpha, main='alpha')

#pdf('res.png')
plot(Temp, Failure, xlim=c(30,100), main='Challenger', sub='Bayes vs ML') 
l = function(y_pred,x2, col='green'){
  m = apply(y_pred, 2,quantile, probs=c(0.50)) 
  lines(x2, m,col=col)
  q05 = apply(y_pred, 2, quantile, probs=c(0.30)) 
  q95 = apply(y_pred, 2, quantile, probs=c(0.70)) 
  lines(x2, q05,col=col)
  lines(x2, q95,col=col)
}
l(d$p_predict,x2=x)
lines(fit$x, fit$y)
#dev.off()






