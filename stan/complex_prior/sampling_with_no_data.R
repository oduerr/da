library(cmdstanr)

mod <- cmdstan_model('stan/complex_prior/2_normals.stan')

# Sample from the model
fit <- mod$sample(
  data = list(), # no data in this case
  seed = 123,
  iter_warmup = 500,
  iter_sampling = 1000,
  chains = 6
)

# Print a summary of the samples
fit$summary()
d = fit$draws(format = 'df')
bayesplot::mcmc_trace(d)
hist(d$theta, 100, freq = FALSE)
