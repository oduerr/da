N = 1e5
mu = runif(N,1,20)
sigma= rcauchy(N, 3, 2)
#We remove points far away 
ok = sigma > 0 & sigma < 100
sum(ok)
mu = mu[ok]
sigma = sigma[ok]


hist(mu, xlim = c(0,20), xlab='mu', freq = FALSE)
hist(sigma,1000, xlab='sigma', xlim=c(0,20), freq = FALSE)
hist(rnorm(sum(ok),mu, sigma), xlab='y', 1000,xlim=c(0,20), freq = FALSE)
