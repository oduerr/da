#Grid Approximation as done in the Homework
W = 6
N = 9
un_norm_post = function (p){
  return (dbinom(W, size = N, prob = p) * dunif(p, 0.4, 1))
}
ps = seq(0,1,length.out = 500)
un = un_norm_post(ps)
post_a = un/sum(un)
samples_grid = sample(ps, prob=post_a, size=1e3, replace = TRUE)


library(rstan)
res = stan('world.stan', data = data.frame(N=9, W=6))
post_samples = rstan::extract(res)
samples_stan = post_samples$p

#Comparison with the grid approximation
hist(samples_grid, freq = FALSE, xlim=c(0,1))
lines(density(samples_stan), col='red')
