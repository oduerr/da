setwd("~/Documents/workspace/da/stan/world_fraction_water")
#Grid Approximation as done in the Homework
W = 4
N = 8
un_norm_post = function (p){
  return (dbinom(W, size = N, prob = p) * dunif(p, 0.4, 1))
}
ps = seq(0,1,length.out = 500)
un = un_norm_post(ps)
post_a = un/sum(un)
samples_grid = sample(ps, prob=post_a, size=1e3, replace = TRUE)

#Using RStan
library(rstan)
start_time <- Sys.time()
res = stan('world.stan', data = list(N=N, W=W))
post_samples = rstan::extract(res)
samples_stan = post_samples$p
Sys.time() - start_time #~20 sec
hist(samples_stan, xlim = c(0.0,1.0), freq = FALSE)
rug(samples_stan)
lines(density(samples_stan))

#Using cmdrstan
library(cmdstanr)
start_time <- Sys.time()
model = cmdstan_model('world.stan')
samples_cmd = model$sample(data = list(N=N, W=W), seed=123)
samples_cmd = samples_cmd$draws("p")
Sys.time() - start_time #~10 sec

#Comparison with the grid approximation
hist(samples_grid, freq = FALSE, xlim=c(0,1))
lines(density(samples_stan), col='red')
lines(density(samples_cmd), col='green')
