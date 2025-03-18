library(cmdstanr)
library(ggplot2)
library(posterior)  # For summarizing draws
library(bayesplot)  # For visualization

if (FALSE) {
  # Install CmdStan
  cmdstanr::install_cmdstan()
}

# Define observed data
data_list <- list(y_obs = 1.42)

# Compile the Stan model
model <- cmdstan_model("stan/unconditional_gaussian/unconditional_gaussian.stan")

# Sample from the posterior
fit <- model$sample(
  data = data_list,
  iter_warmup = 1000,
  iter_sampling = 2000,
  chains = 4,
  seed = 42
)

# Summarize the posterior
print(fit$summary(c("mu", "sigma")))

# Extract posterior draws
posterior_samples <- fit$draws(format = "df")

# Convert to data frame
mu_samples <- posterior_samples$mu
sigma_samples <- posterior_samples$sigma
