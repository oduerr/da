# Initial values
#set.seed(123)
Steps = 10000
plot_max = 500
thetas_r = rep(NA, Steps)
thetas_phi = rep(NA, Steps)

# Target distribution in r and phi
# Target distribution in r (up to a constant factor)
p_r = function(r) {
  return (dnorm(r, mean = 1, sd = 0.25))
}

# Proposal distribution standard deviations
proposal_sd_r = 0.1
proposal_sd_phi = 0.1

# Initial values for r and phi
r = 1
phi = 0

for (t in 1:Steps) {
  # Propose new values
  r_star = r + rnorm(1, mean = 0, sd = proposal_sd_r)
  phi_star = phi + rnorm(1, mean = 0, sd = proposal_sd_phi)
  
  # Reflect phi_star within [0, 2*pi]
  phi_star = phi_star %% (2 * pi)
  
  # Compute the acceptance probability
  if (r_star > 0) {  # Ensure r_star is within valid range
    A = min(1, p_r(r_star) / p_r(r))
  } else {
    A = 0
  }
  
  # Accept or reject the new values
  if (runif(1) < A) {
    r = r_star
    phi = phi_star
  }
  
  # Store the samples
  thetas_r[t] = r
  thetas_phi[t] = phi
}

# Transform r and phi to x and y
x = thetas_r * cos(thetas_phi)
y = thetas_r * sin(thetas_phi)

# Plot the trace of the samples in r
plot(1:plot_max, thetas_r[1:plot_max], type = "l", xlab = "Steps", ylab = "r", main="Trace of the samples in r")

par(mfrow=c(1,2))
# Plot the histogram of r
r_samples = sqrt(x^2 + y^2)
hist(r_samples[200:Steps], main = paste0("Density of r, mean=",round(mean(r_samples),2)), freq=FALSE, breaks = 30)
# Scatter plot of (x1, x2)
plot(x, y, main = "Scatter plot of (x1, x2)", xlab = "x1", ylab = "x2", pch='.', 
     xlim=c(-2,2), ylim=c(-2,2))
par(mfrow=c(1,1))


mean(thetas_r[200:Steps])
sd(thetas_r[200:Steps])
