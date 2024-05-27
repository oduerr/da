# Initial values
set.seed(123)
Steps = 10000
plot_max = 500
x1 = rep(NA, Steps)
x2 = rep(NA, Steps)

# Target distribution in r (up to a constant factor)
p_r = function(r) {
  return (dnorm(r, mean = 1, sd = 0.25))
}

# Proposal distribution standard deviations
proposal_sd_x1 = 0.5
proposal_sd_x2 = 0.5

# Initial values for x1 and x2
x1[1] = 1
x2[1] = 0

for (t in 2:Steps) {
  # Propose new values in x1 and x2
  x1_star = x1[t-1] + rnorm(1, mean = 0, sd = proposal_sd_x1)
  x2_star = x2[t-1] + rnorm(1, mean = 0, sd = proposal_sd_x2)
  
  # Calculate r and r_star
  r = sqrt(x1[t-1]^2 + x2[t-1]^2)
  r_star = sqrt(x1_star^2 + x2_star^2)
  
  # Compute the Jacobian determinant
  J = 1/r
  J_star = 1/r_star
  
  # Compute the acceptance probability
  if (r_star > 0) {  # Ensure r_star is within valid range
    A = min(1, (p_r(r_star) * J_star) / (p_r(r) * J))
  } else {
    A = 0
  }
  
  # Accept or reject the new values
  if (runif(1) < A) {
    x1[t] = x1_star
    x2[t] = x2_star
  } else {
    x1[t] = x1[t-1]
    x2[t] = x2[t-1]
  }
}

# Plot the trace of the samples in x1
plot(1:plot_max, x1[1:plot_max], type = "l", xlab = "Steps", ylab = "x1 and x2", main="Trace of the samples in x")
lines(1:plot_max, x2[1:plot_max], type = "l", xlab = "Steps",  col='red')

par(mfrow=c(1,2))
# Plot the histogram of r
r_samples = sqrt(x1^2 + x2^2)
hist(r_samples[200:Steps], main = paste0("Density of r, mean=",round(mean(r_samples),2)), freq=FALSE, breaks = 30)
# Scatter plot of (x1, x2)
plot(x1, x2, main = "Scatter plot of (x1, x2)", xlab = "x1", ylab = "x2", pch='.')
par(mfrow=c(1,1))


