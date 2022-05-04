#setwd("~/Documents/workspace/da/stan/howell1")
library(cmdstanr)
library(rethinking) #For the data set
data(Howell1)
d = Howell1[Howell1$age >= 18, ]

#res = stan('unconditional.stan', data = list(N=nrow(d), height=d$height))
model = cmdstan_model('unconditional.stan') #Compile
samples_cmd = model$sample(data = list(N=nrow(d), height=d$height))
post_s = samples_cmd$draws(format = "df") 

plot(density(post_s$mu))

#Posterior Predictive
post_pred = rnorm(nrow(post_s), mean = post_s$mu, sd=post_s$sigma)
hist(d$height, freq = FALSE, ylim=c(0,0.07))
rug(d$height)
lines(density(post_pred))
