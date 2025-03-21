# Set parameters
mu <- 42   # True mean
sigma2 <- 1  # Variance
n <- 6  # Sample size
set.seed(123)  # For reproducibility

# a) Draw a sample of size 6 and compute the mean and median
sample_data <- rnorm(n, mean = mu, sd = sqrt(sigma2))
sample_mean <- mean(sample_data)
sample_median <- median(sample_data)

cat("Sample Mean:", sample_mean, "\n")
cat("Sample Median:", sample_median, "\n")

# b) Repeat the sampling process 10,000 times and analyze distribution of the mean
num_simulations <- 10000
means <- numeric(num_simulations)

for (i in 1:num_simulations) {
  sample_data <- rnorm(n, mean = mu, sd = sqrt(sigma2))
  means[i] <- mean(sample_data)
}

# Plot the density of the sample means
plot(density(means), main = "Sampling Distribution of the Mean",
     xlab = "Sample Mean", ylab = "Density", col = "blue", lwd = 2)

# c) Unbiasedness of the mean
mean_estimate <- mean(means)
cat("Mean of sample means:", mean_estimate, "\n")
cat("Theoretical Mean:", mu, "\n")

# d) Compare mean and median distributions
medians <- numeric(num_simulations)

for (i in 1:num_simulations) {
  sample_data <- rnorm(n, mean = mu, sd = sqrt(sigma2))
  medians[i] <- median(sample_data)
}

# Plot both densities
plot(density(means), main = "Comparison of Mean and Median Estimators",
     xlab = "Estimator", ylab = "Density", col = "blue", lwd = 2)
lines(density(medians), col = "red", lwd = 2)
legend("topright", legend = c("Mean", "Median"),
       col = c("blue", "red"), lwd = 2)

# Calculate standard deviations
sd_mean <- sd(means)
sd_median <- sd(medians)

cat("Standard Deviation of Means:", sd_mean, "\n")
cat("Standard Deviation of Medians:", sd_median, "\n")

# e) Consistency of the mean estimator
sample_sizes <- c(5, 10, 50, 100, 500, 1000)
means_consistency <- sapply(sample_sizes, function(n) {
  means_large <- replicate(num_simulations, mean(rnorm(n, mean = mu, sd = sqrt(sigma2))))
  mean(means_large)
})

plot(sample_sizes, means_consistency, type = "b", col = "blue", pch = 16,
     xlab = "Sample Size", ylab = "Mean Estimate",
     main = "Consistency of the Sample Mean")
abline(h = mu, col = "red", lwd = 2, lty = 2)
