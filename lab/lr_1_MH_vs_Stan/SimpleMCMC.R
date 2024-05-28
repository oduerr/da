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


# Generate some example data
if (FALSE){
  set.seed(123)
  N <- 20
  x <- rnorm(N)
  a_true <- 2
  b_true <- 1
  sigma_true <- 1
  y <- rnorm(N, mean = a_true * x + b_true, sd = sigma_true)
  data <- list(x = x, y = y, N = N)
} else{
  library(edudat)
  df = load_data("sbp.csv")
  data = as.stan_data(df)
  lm(y ~ x, df) #87.61, 1.10
}

# Initial values
initial_values <- c(0, 0, 1)
log_posterior(initial_values, data$x, data$y, data$N)

# Number of iterations
n_iter <- 10000

##### Initialization: Generating figure for lecture note ####
set.seed(1)
initial_values <- c(0, 0, 1)
c1 <- metropolis_hastings(log_posterior, initial_values, data, 1000, proposal_sd)
df = data.frame(
  t = 1:length(c1[,1]),
  theta = c1[,1],
  chain='Chain 1'
)
initial_values <- c(5, 0, 1)
c2 <- metropolis_hastings(log_posterior, initial_values, data, 1000, proposal_sd)
df = rbind(df, data.frame(
  t = 1:length(c2[,1]),
  theta = c2[,1],
  chain='Chain 2'
))

library(tidyverse) 
ggplot(df, aes(x = t, y = theta, col=chain)) +
  geom_line() +
  labs(title = "Two Simple Monte Carlo Chains", x = "t", y = expression(theta)) +
  theme_minimal() + 
  theme(legend.position="none")
ggsave("lecture_notes/MCMC/2chains.png", width = 6, height = 4, units = "in",  dpi = 300)

##### Looking at the marginal from t = 250 ####
df %>% filter(t > 250) %>%
ggplot(aes(x = theta, fill=chain)) +
  geom_density(alpha = 0.5) +
  geom_rug(sides = "b") + 
  labs(title = "Marginal Distributions of Two Chains", x = expression(theta), y = "Density") +
  xlim(1, 3) +
  theme_minimal() 
ggsave("lecture_notes/MCMC/2chains-marginal.png", width = 6, height = 4, units = "in",  dpi = 300)



#############################################



# Proposal standard deviation
### The proposal standard deviation is a tuning parameter that affects the acceptance rate of the algorithm.
proposal_sd <- c(0.1, 5, 5)
# Run the Metropolis-Hastings algorithm
samples <- metropolis_hastings(log_posterior, initial_values, data, n_iter, proposal_sd)
samples_df <- as.data.frame(samples)
samples_mcmc <- coda::as.mcmc(samples_df)
coda::effectiveSize(samples_mcmc) 


# Analyze the results
colnames(samples_df) <- c("a", "b", "sigma")
summary(samples_df)

# Plot the results
par(mfrow = c(3, 1))
plot(samples_df$a, type = "l", main = "Trace plot of a")
plot(samples_df$b, type = "l", main = "Trace plot of b")
plot(samples_df$sigma, type = "l", main = "Trace plot of sigma")
par(mfrow = c(1, 1))

#### Doing the same with cmdrstan ###
library(cmdstanr)
m_rcmdstan <- cmdstan_model('~/Documents/GitHub/da/lab/lr_mcmc_diag/linear_regression.stan') #Compiling
samples = m_rcmdstan$sample(data=list(x=data$x[,1],N=data$N, y=data$y), chains=1, iter_sampling = 10000)
print(samples)
samples_stan = samples$draws(format = 'df')

## Scatterplot Matrix
pairs(~a+b+sigma, samples_stan)
cor(samples_stan$a, samples_stan$b)
cor(samples_stan$a, samples_stan$sigma)


# Comparing the posterior samples for a from the two methods
plot(density(samples_stan$a))
lines(density(samples_df$a), col='blue')
legend("topright", c("Stan", "Metropolis"), col=c("black", "blue"), lty=1)

plot(density(samples_stan$b))
lines(density(samples_df$b), col='blue')
legend("topright", c("Stan", "Metropolis"), col=c("black", "blue"), lty=1)

plot(density(samples_stan$sigma))
lines(density(samples_df$sigma), col='blue')
legend("topright", c("Stan", "Metropolis"), col=c("black", "blue"), lty=1)

plot(samples_stan$a[1000:1200], col = "red", type = "l", main = "Trace plot of a", alpha=0.5)
lines(samples_df$a[1000:1200], type = "l", main = "Trace plot of a", col='blue')
legend("topright", c("Stan", "Metropolis"), col=c("red", "blue"), lty=1)

plot(samples_stan$sigma[1000:1200], col = "red", type = "l", main = "Trace plot of sigma", alpha=0.5)
lines(samples_df$sigma[1000:1200], type = "l", main = "Trace plot of sigma", col='blue')
legend("topright", c("Stan", "Metropolis"), col=c("red", "blue"), lty=1)


##### Initialization: Generating figure for lecture note ####
library(ggplot2)

# Create data frames for the subsets
stan_data <- data.frame(Index = 1000:1200, a = samples_stan$a[1000:1200], Method = "Stan")
metropolis_data <- data.frame(Index = 1000:1200, a = samples_df$a[1000:1200], Method = "Metropolis")

# Combine the data frames
combined_data <- rbind(stan_data, metropolis_data)

# Plot with ggplot2
ggplot(combined_data, aes(x = Index, y = a, color = Method)) +
  geom_line() +
  labs(title = "Trace plot of a", color = "Method") +
  theme_minimal()
ggsave("lecture_notes/MCMC/traceplot_stan_vs_mh.png", width = 6, height = 4, units = "in",  dpi = 300)
##### Initialization: Generating figure for lecture note ####



# Cacluation of the effective sample size
library(coda)
samples_mcmc <- as.mcmc(samples_df)
coda::effectiveSize(samples_mcmc) / nrow(samples_df)
coda::effectiveSize(samples_stan) / nrow(samples_stan)
samples




########### Diagnostics ################

calculate_neff <- function(samples) {
  N <- length(samples)
  mean_samples <- mean(samples)
  gamma_0 <- var(samples) * (N - 1) / N
  
  # Function to calculate autocovariance at lag k
  autocovariance <- function(k) {
    cov_sum <- sum((samples[1:(N-k)] - mean_samples) * (samples[(k+1):N] - mean_samples))
    return(cov_sum / N)
  }
  
  # Calculate autocovariances and autocorrelations
  max_lag <- 100  # Define the maximum lag to consider
  autocov <- sapply(0:max_lag, autocovariance)
  autocorr <- autocov / gamma_0
  
  # Calculate the autocorrelation time
  tau <- 1 + 2 * sum(autocorr[2:(max_lag + 1)])
  
  # Calculate effective sample size
  neff <- N / tau
  
  return(neff)
}

calculate_neff(samples_df$a)
coda::effectiveSize(samples_mcmc)


### Tuning Parameters:
# Extract NUTS parameters
samples$metadata()$step_size_adaptation
samples$metadata()$max_depth
samples$diagnostic_summary()



