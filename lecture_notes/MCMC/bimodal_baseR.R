# Set up necessary parameters
true_sd = 0.75
proposal_sd = 0.5

# Set seed for reproducibility
set.seed(123)

# Define the bimodal density function
bimodal_density <- function(x) {
  0.5 * dnorm(x, mean = -3, sd = true_sd) + 0.5 * dnorm(x, mean = 3, sd = true_sd)
}

# Metropolis-Hastings algorithm
mcmc_sample <- function(n_iter, proposal_sd, init_value) {
  samples <- numeric(n_iter)
  samples[1] = init_value
  current_value = init_value
  
  for (i in 2:n_iter) {
    proposed_value = rnorm(1, mean = current_value, sd = proposal_sd)
    acceptance_ratio = bimodal_density(proposed_value) / bimodal_density(current_value)
    
    if (runif(1) < acceptance_ratio) {
      current_value = proposed_value
    }
    
    samples[i] = current_value
  }
  
  return(samples)
}

# Generate samples for 3 chains
n_chains = 3
n_iter = 10000
init_values = seq(-5, 5, length.out = n_chains)
all_samples = lapply(init_values, function(init) mcmc_sample(n_iter, proposal_sd, init))

# Combine all samples into one vector for density estimation
combined_samples <- unlist(all_samples)

# Set up plotting layout
par(mfrow = c(1, 2), mar = c(5, 4, 4, 1) + 0.1)

# Plot trace plots
plot(NULL, xlim = c(1, n_iter), ylim = range(combined_samples), xlab = "Iteration", ylab = "Sample Value", main = "Trace Plot of Samples from Bimodal Distribution (3 Chains)")
colors <- c("red", "green", "blue")
for (i in 1:n_chains) {
  lines(1:n_iter, all_samples[[i]], col = colors[i], lty = 1)
}
legend("topright", legend = paste("Chain", 1:n_chains), col = colors, lty = 1)

# Plot true density and sampled density
true_density_x <- seq(-8, 8, length.out = 1000)
true_density_y <- bimodal_density(true_density_x)
sampled_density <- density(combined_samples)

plot(true_density_y, true_density_x, type = "l", col = "blue", xlab = "Density", ylab = "Value", main = "True vs Sampled Density", ylim = range(true_density_x))
lines(sampled_density$y, sampled_density$x, col = "red")
legend("bottomright", legend = c("True Density", "Sampled Density"), col = c("blue", "red"), lty = 1)

# Save the plot to a file
dev.copy(png, filename = "lecture_notes/MCMC/bimodal2.png", width = 16*300, height = 9*300, res = 300)
dev.off()
