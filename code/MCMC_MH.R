# Define the log-posterior function (unnormalized)
log_posterior <- function(params, x, y, N) {
  a <- params[1]
  b <- params[2]
  sigma <- params[3]
  if (sigma <= 0) {
    return(-Inf)
  }
  # Log-prior distributions
  log_prior_a <- dnorm(a, mean = 0, sd = 10, log = TRUE)
  log_prior_b <- dnorm(b, mean = 0, sd = 10, log = TRUE)
  log_prior_sigma <- dunif(sigma, min = 0, max = 100, log = TRUE)
  
  # Log-likelihood
  mu <- a * x + b
  log_likelihood <- sum(dnorm(y, mean = mu, sd = sigma, log = TRUE))
  
  # Log-posterior
  log_posterior <- log_prior_a + log_prior_b + log_prior_sigma + log_likelihood
  return(log_posterior)
}

# Metropolis-Hastings algorithm
metropolis_hastings <- function(log_posterior, initial_values, data, n_iter, proposal_sd) {
  N <- length(data$y)
  x <- data$x
  y <- data$y
  
  # Storage for samples
  samples <- matrix(NA, n_iter, length(initial_values))
  samples[1, ] <- initial_values
  
  # Current values
  current_values <- initial_values
  current_log_post <- log_posterior(current_values, x, y, N)
  proposed_values = rep(NA, length(initial_values))
  for (i in 2:n_iter) {
    # Propose new values
    proposed_values <- rnorm(3, mean = current_values, sd = proposal_sd) #New value for a
    
    # Calculate log-posterior for proposed values
    proposed_log_post <- log_posterior(proposed_values, x, y, N)
    
    # Acceptance ratio
    acceptance_ratio <- exp(proposed_log_post - current_log_post)
    
    # Accept or reject the proposal
    if (runif(1) < acceptance_ratio) {
      current_values <- proposed_values
      current_log_post <- proposed_log_post
    }
    
    # Store the current values
    samples[i, ] <- current_values
  }
  
  return(samples)
}

set.seed(1)
initial_values <- c(0, 0, 1)
c1 <- metropolis_hastings(log_posterior, initial_values, data, 1000, proposal_sd)


