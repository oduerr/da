N = 1e5
mu = runif(N,1,20)
sigma= rcauchy(N, 3, 2)
ok = sigma > 0 & sigma < 100
sum(ok)
mu = mu[ok]
sigma = sigma[ok]


hist(mu, xlim = c(0,20))
hist(sigma,1000, freq = FALSE, xlim=c(0,20))
hist(rnorm(sum(ok),mu, sigma), 1000,xlim=c(0,20), freq = FALSE)
