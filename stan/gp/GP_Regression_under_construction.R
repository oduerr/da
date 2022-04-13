require(rstan)
parallel::detectCores()
options(mc.cores = parallel::detectCores())
set.seed(42)
xs = seq(-5,5,length.out=100)

mu_true = function(xs) sin(xs) + 0.1 * xs  
x_train = c(-4,-2, 0, 1, 4, 4.2)
#x_train = seq(-5,5,length.out=20)
N = length(x_train)
y_train = rnorm(N, mean = mu_true(x_train), sd=0.2)
x_test = seq(-6,6, length.out = 200)
N_test = length(x_test)

fit_regression = stan(file = '~/Documents/workspace/da/stan/gp/GP_regression.stan', 
                  data=list(
                    N1=N, 
                    x1 = x_train,
                    y1 = y_train,
                    N2 = N_test,
                    x2 = x_test
                    ),
                  chains = 4,
                  iter = 10000)

fit_regression
post = extract(fit_regression)
means = apply(post$y2, 2, mean)
sds = apply(post$y2, 2, sd)

plot(x_train, y_train, ylim=c(-1.5,1.5), xlim=c(-6,6), col='blue')
#curve(mu_true, add=TRUE, col='red')
lines(x_test, means, col='blue')
lines(x_test, means + sds)
lines(x_test, means - sds)



#see https://mc-stan.org/docs/2_22/functions-reference/covariance.html
cov_exp_quad_r = function(xs, alpha, rho) {
  delta = 1e-9 #For numerical stability
  N = length(xs)
  K = matrix(nrow=N, ncol=N)
  for (i in 1:N){
    for (j in 1:N){
      K[i,j] = alpha^2 * exp(-1.0/(2*rho^2) * (xs[i]-xs[j])^2)
    }
  }
  # diagonal elements
  for (n in 1:N)
    K[n, n] = K[n, n] + delta;
  return (K)
}

x_test = seq(-8,8, length.out = 50)
N_test = length(x_test)

S_xx = cov_exp_quad_r(x_train, alpha, rho)  
S_xx_inv = solve(S_xx)

T = length(post$alpha)
f_test_mu = matrix(nrow=T, ncol=N_test)
f_test_sig = matrix(nrow=T, ncol=N_test)
for (t in seq(1,T)){
  #t = 100
  alpha = post$alpha[t]
  rho= post$alpha[t]
  f_x = post$f[t,]
  
  S_yy = cov_exp_quad_r(x_test, alpha, rho)
  #S_xy = cov_exp_quad_r(c(xs, x_test), alpha, rho)
  
  S_yx = cov_exp_quad_r(c(x_test, x_train), alpha, rho)[1:length(x_test), (N_test+1):(N_test+length(x_train))]
  #f_y = (S_yx %*% S_xx_inv) %*% f_x
  f_test_mu[t,] =  (S_yx %*% S_xx_inv) %*% f_x
}

mus_test = colMeans(f_test_mu)
std_test = apply(f_test, 2, sd)
plot(x_train, y_train, ylim=c(-5,5), xlim=c(-8,8), col='blue')
curve(mu_true, add=TRUE)
lines(x_test, mus_test , col='red')
abline(h=0, lty=2)
lines(x_test, mus_test + std_test, col='green')
lines(x_test, mus_test - std_test, col='green')



########## Analytical Solution
mu = S_yx %*% S_xx_inv %*% y_train
lines(x_test, mu, col='green')
