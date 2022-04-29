# rcmdstan is a interface to stan
if (FALSE){
  install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
  library(cmdstanr)
  install_cmdstan()
  check_cmdstan_toolchain(fix = TRUE, quiet = TRUE)
}
library(cmdstanr)
stan_file <- file.path(cmdstan_path(), "examples", "bernoulli", "bernoulli.stan")
mod <- cmdstan_model(stan_file)
data_file <- file.path(cmdstan_path(), "examples", "bernoulli", "bernoulli.data.json")
data_file
fit <- mod$sample(data = data_file, seed=123)
