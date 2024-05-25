# Load necessary libraries
library(ggplot2)
library(dplyr)
library(patchwork)

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

# Generate samples for 10 chains
n_chains = 3
n_iter = 10000
init_values = seq(-5, 5, length.out = n_chains)
all_samples = lapply(init_values, function(init) mcmc_sample(n_iter, proposal_sd, init))

# Combine samples into a single data frame
samples_df <- do.call(rbind, lapply(1:n_chains, function(chain) {
  data.frame(Iteration = 1:n_iter, Sample = all_samples[[chain]], Chain = as.factor(chain))
}))

# Plot trace plot using ggplot2
trace_plot <- ggplot(samples_df, aes(x = Iteration, y = Sample, color = Chain)) +
  geom_line(alpha = 0.9) +
  labs(title = "Trace Plots of MCMC Chains", x = "Iteration", y = "Sample Value", color = "Chain") +
  theme_minimal()

# Combine all samples into one vector for density estimation
combined_samples <- unlist(all_samples)

# True density plot
true_density <- data.frame(x = seq(-8, 8, length.out = 1000))
true_density$y <- bimodal_density(true_density$x)

true_density_plot <- ggplot(true_density, aes(x, y)) +
  geom_line(color = "blue", size = 1) +
  coord_flip() +
  labs(title = "True vs Sampled Density", x = "Density", y = "Value") +
  theme_minimal()

# Sampled density plot
sampled_density <- density(combined_samples)
sampled_density_df <- data.frame(x = sampled_density$x, y = sampled_density$y)

sampled_density_plot <- ggplot(sampled_density_df, aes(x, y)) +
  geom_line(color = "red", size = 1) +
  coord_flip() +
  theme_minimal()

# Combine true and sampled density plots
density_plot <- true_density_plot +
  geom_line(data = sampled_density_df, aes(x, y), color = "red", size = 1)

# Combine the trace plot and the density plot using patchwork
combined_plot <- trace_plot + density_plot + plot_layout(ncol = 2, widths = c(2, 1))

# Print the combined plot
print(combined_plot)

ggsave("lecture_notes/MCMC/bimodal.png", width = 16*0.7, height = 9*0.7, units = "in",  dpi = 300)
