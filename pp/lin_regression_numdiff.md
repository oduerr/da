# Linear Regression with probabilistic programming

This notebook shows how to do a simple linear regression using
stochastic gradient descent. You will see that you just need to define
the likelihood of the model and the rest is done automatically. We will
first use the `numDeriv` package to calculate the gradient and use the
high level probabilistic programming language Stan.

## Data Generation

``` r
# Generating data from a linear regression model
set.seed(1)
x = seq(0,5, length.out = 50)
y = rnorm(50, mean = 2 * x + 1, sd = 1)
plot(x,y)
lines(x, 2 * x + 1, xlab = "x", ylab = "y")
```

![](lin_regression_numdiff.markdown_strict_files/figure-markdown_strict/unnamed-chunk-1-1.png)

## Definition of the likelihood

The negative log-likelihood of the linear regression model given the
data, can be calculated as follows:

``` r
nll <- function(params, x, y) {
  beta_0 <- params[1]
  beta_1 <- params[2]
  sigma <- params[3]
  y_hat <- beta_0 + beta_1 * x 
  nll <- -mean(dnorm(y, mean = y_hat, sd = sigma, log = TRUE))
  return(nll)
}
nll(c(0,1,1), x, y) #beta_0 = 0, beta_1 = 1, sigma = 1 ==> ~8.81
```

    [1] 8.810049

## Introduction to Automatic Differentiation

Integration is hard, but differentiation became easy with computers. In
the following we show how to calculate the gradient of a function using
the `numDeriv` package. The `numDeriv` package uses numerical
differentiation to calculate the gradient. Basically all you have to do
is to define a function returning a single float value and then call the
`grad` function to get the gradient (the vector of partial derivatives)
at a certain point.

``` r
library(numDeriv)
x2 = function(x) x^2
grad(x2, 2) #derivative of x^2 w.r.t x at x=2
```

    [1] 4

``` r
#(𝑥+𝑦)⋅𝑧
f = function(p) (p[1] + p[2]) * p[3]
grad(f, c(-2,5,-4)) #derivative of (x+y)*z w.r.t x,y,z at x=1, y=2, z=3
```

    [1] -4 -4  3

``` r
f = function(p) {
  res = 0
  for (i in 1:10){
    if (i %% 2 == 0)
      res = res + p[1] * i
    else
      res = res + p[2] * i
  }
  res = res + p[3]
  return(res)
}
grad(f, c(-2,5,-4)) #30 25  1
```

    [1] 30 25  1

## Calculation of the gradient of the NLL

Note that it is possible to calculate the gradient of the negative
log-likelihood using the `numDeriv` package. The gradient is calculated
as follows:

``` r
p = c(0,1,1)
grad_est <- grad(func = function(p) nll(p, x, y), p) #derivative of nll w.r.t p
grad_est
```

    [1]  -3.600448 -11.156105 -14.782220

## Performing Gradient Descent

Let’s now execute a simple gradient descent. We begin with some initial
parameter values and then iteratively update these parameters in the
direction of the negative gradient.

``` r
p = c(1,1,1) # Initial values for the parameters
lr = 0.01    # Definition of the learning rate
for (i in 1:1000) {
  if (i == 1 | i %% 100 == 0)
    print(c(i, p, nll(p, x, y)))
  grad_est <- grad(func = function(p) nll(p, x, y), x = p)
  p = p - lr * grad_est # Update the parameters
}
```

    [1] 1.0000 1.0000 1.0000 1.0000 5.7096
    [1] 100.000000   1.260382   1.948605   1.012503   1.264351
    [1] 200.0000000   1.2239937   1.9607943   0.8421578   1.2267745
    [1] 300.0000000   1.1925877   1.9703901   0.8250877   1.2252470
    [1] 400.0000000   1.1700275   1.9772832   0.8236344   1.2246833
    [1] 500.0000000   1.1540792   1.9821561   0.8232895   1.2244027
    [1] 600.0000000   1.1428227   1.9855954   0.8231369   1.2242630
    [1] 700.0000000   1.1348814   1.9880218   0.8230619   1.2241935
    [1] 800.0000000   1.1292800   1.9897332   0.8230247   1.2241589
    [1] 900.0000000   1.1253295   1.9909403   0.8230062   1.2241417
    [1] 1000.0000000    1.1225434    1.9917915    0.8229969    1.2241331

``` r
print(p)
```

    [1] 1.1225202 1.9917986 0.8229969

After about 1000 iterations, the parameters are close to their optimal
values. It’s important to note that there are much more advanced
optimization algorithms available that typically use the second
derivative (Hessian) and require far fewer iterations. While it is
technically possible to compute the Hessian using the `numDeriv`
package, doing so can become complex. In this tutorial, our focus is
more on understanding the principles of gradient descent rather than on
achieving rapid optimization.

## Comparison with R lm function

We can employ the highly optimized `lm` function in R to find the
maximum likelihood estimates efficiently. This method is not only fast
but also provides robust estimates for linear models.

``` r
# Obtain the coefficients of the linear model
lm(y ~ x)$coefficients
```

    (Intercept)           x 
       1.115878    1.993828 

``` r
# Calculate the log-likelihood of the linear model
# Note: This is the sum of the log-likelihoods, not the average.
logLik(lm(y ~ x)) 
```

    'log Lik.' -61.20623 (df=3)

## Utilizing Stan for Optimization

We can also employ the cmdrstan package to perform the optimization.
Below, you can view the Stan code used for this purpose:

``` stan
data {
  int<lower=0> N;           // Number of data points
  vector[N] x;              // Predictor variable
  vector[N] y;              // Outcome variable
}

parameters {
  real beta_0;              // Intercept
  real beta_1;              // Slope
  real<lower=0> sigma;      // Standard deviation of the residuals
}

model {
    // Adding the likelihood of the data given the parameters
    for (i in 1:N) {
        target += normal_lpdf(y[i] | beta_1 * x[i] + beta_0, sigma);
    }
}
```

It is a best practice to store Stan code in a separate file when using
`cmdrstan`. This approach simplifies debugging and code management.
Additionally, storing models in separate files allows for caching,
improving efficiency when models are reused.

``` r
library(cmdstanr)
stan_data = list(N = length(x), x = x, y = y)
mod = cmdstan_model('lr.stan')
mod$optimize(data = stan_data)
```

    Initial log joint probability = -280.23 
        Iter      log prob        ||dx||      ||grad||       alpha      alpha0  # evals  Notes  
          13      -61.2062   0.000122445   0.000296856           1           1       17    
    Optimization terminated normally:  
      Convergence detected: relative gradient magnitude is below tolerance 
    Finished in  0.1 seconds.

     variable estimate
       lp__     -61.21
       beta_0     1.12
       beta_1     1.99
       sigma      0.82

### The target += syntax

The `target +=` is used to add the log-likelihood to the target. In
Bayesian context the `target` is the log-posterior that is the
log-likelihood plus the log-prior. Here it is just the log-likelihood.
The `target` is a global variable that is used to store the quantity to
minimize. The `target` is then used to calculate the gradients and the
Hessian.

### Alternative model syntax

Instead of employing a loop, which can be computationally slower, a
vectorized approach offers a more efficient alternative. Additionally,
the `~` syntax can be used as an alternative to `target +=`. The `~`
syntax is generally more readable and often preferred for its clarity.
All three model definitions provided below yield the same results for
the coefficients:

```
#| class-output: stan
// Loop (slowest method)
model {
    for (i in 1:N) {
        target += normal_lpdf(y[i] | beta_1 * x[i] + beta_0, sigma);
    }
}

// Vectorized approach
model {
    target += normal_lpdf(y | beta_1 * x + beta_0, sigma);
}

// Using the '~' syntax
model {
    y ~ normal(beta_1 * x + beta_0, sigma);
}
```

A subtle point about using the ~ syntax is that constants in the density
function, such as $\sqrt(2 \pi)$, are not included. This exclusion does
not affect optimization, but it results in different `lp__` values.
Therefore, do not be surprised if the `lp__ values` differ.
