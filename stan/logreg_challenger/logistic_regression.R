#https://mc-stan.org/docs/2_21/stan-users-guide/logistic-probit-regression-section.html
#setwd("~/Documents/workspace/da/stan/logreg_challenger/")
library(ggplot2)
challenger = read.csv('stan/logreg_challenger/challenger.txt', header = TRUE)
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
if (FALSE){
# STAN Version
  library('rstan')
  model = stan_model(file='stan/logreg_challenger/log_reg.stan')
  log.samples = sampling(model, list(N=23L,x=challenger$Temp, y=challenger$Failure, N2=length(x), x2=x))
  traceplot(log.samples)
  print(log.samples)
  d = extract(log.samples)
  rethinking::dens(d$alpha, main='alpha')
}
library(cmdstanr)
m_rcmdstan <- cmdstan_model('stan/logreg_challenger/log_reg.stan') #Compiling
samples = m_rcmdstan$sample(num_samples = 2000,data=list(N=23L,x=challenger$Temp, y=challenger$Failure, N2=length(x), x2=x))
bayesplot::mcmc_trace(samples$draws(c('alpha', 'beta', 'lp__')))
samples
d = samples$draws('p_predict', format='df')[,1:36]

plot(Temp, Failure, xlim=c(30,100), main='Challenger', sub='Bayes vs ML') 
#pdf('res.png')
l = function(y_pred,x2, col='green'){
  m = apply(y_pred, 2,quantile, probs=c(0.50)) 
  lines(x2, m,col=col)
  q05 = apply(y_pred, 2, quantile, probs=c(0.30)) 
  q95 = apply(y_pred, 2, quantile, probs=c(0.70)) 
  lines(x2, q05,col=col)
  lines(x2, q95,col=col)
}
l(d,x2=x)
lines(fit$x, fit$y)
#dev.off()






