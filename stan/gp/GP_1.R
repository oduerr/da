library(rstan)
parallel::detectCores()
options(mc.cores = parallel::detectCores())


###### Samples from GP ######

xs = seq(-10,10, length.out = 100)
fit = stan(file = '~/Documents/workspace/da/stan/gp/GP_1.stan', 
           data=list(N=length(xs), x = xs),
           chains = 1,
           iter = 100)

#Figure with two samples
f = extract(fit)$f
plot(xs, f[1,], ylim=c(-2,2), type='p', col=1, xlab='x', ylab='f')
for (i in 1:2) {
  points(xs, f[i,], ylim=c(-2,2), type='p', col=i)
  points(xs, f[i,], col=i, type='l')
}

