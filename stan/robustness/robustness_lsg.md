
## Robust Regression

Use the following code to create some data, including two outlieres:

``` r
######## Creating Data #####
s <- matrix(c(1, .6, 
              .6, 1), 
            nrow = 2, ncol = 2)
set.seed(3)
#Data w/o outliers
d = data.frame(MASS::mvrnorm(n = 100, mu = c(0,0), Sigma = s))
colnames(d) = c('y','x')
#sorting
idx = sort(d$x, index.return = TRUE)$ix
d = d[idx,1:2]
o <- d
o[c(1:2), 1] <- c(5, 4.5)
dat = list(
  N = nrow(o),
  x = o$x,
  y = o$y
)
plot(y ~ x, dat, main='data with 2 outliers')
```

![](robustness_lsg_files/figure-gfm/data-1.png)<!-- -->

1)  Fit a linear model to the data, using the following Stan code.

``` r
stan_code = "data{
  int<lower=0> N;
  vector[N] y;
  vector[N] x;
}

parameters{
  real a; 
  real b;
  real<lower=0> sigma;
}

model{
  //y ~ normal(mu, sigma);
  y ~ normal(a * x + b, sigma);
  a ~ normal(0, 10); 
  b ~ normal(0, 10); 
  sigma ~ normal(0,10);
}"
```

``` r
  library(rstan)
  gauss.mod = stan_model(model_code = stan_code)
  gauss.sample = sampling(gauss.mod, data=dat)
```

2)  Plot the data and include *E*(*y*\|*x*) = *ā* ⋅ *x* + *b̄* , where
    *ā* and *b̄* are the respective posterior means. Include the maximum
    likelihood solution for both the data with and without outliers (you
    can use `lm` for the maximum likelihood solution).

``` r
  gauss_a = mean(extract(gauss.sample, 'a')$a)
  gauss_b = mean(extract(gauss.sample, 'b')$b)
  plot(y ~ x, dat, main='Non-Robust regressions')
  curve(gauss_a*x + gauss_b, add = TRUE, lty=2, lwd=2, col='black')
  abline(lm(y ~ x, dat), col='red')
  abline(lm(y ~ x, d), col='green')
```

![](robustness_lsg_files/figure-gfm/student-t-1.png)<!-- -->

``` r
  mean(extract(gauss.sample, 'a')$a) #0.13
```

    ## [1] 0.1380934

3)  Robust regression: Modify the stan code above to output a
    t-distribution instead of a Gaussian. See
    e.g. <https://mc-stan.org/docs/2_18/functions-reference/student-t-distribution.html>
    how to parametrize a t-distribution. Ensure that the parameter is
    *ν* ≥ 1. Plot *E*(*y*\|*x*) = *ā* ⋅ *x* + *b̄* together with the
    maximum likelihood solutions from above.

``` r
stan_code_t = "data{
  int<lower=0> N;
  vector[N] y;
  vector[N] x;
}

parameters{
  real a; 
  real b;
  real<lower=0> sigma;
  real<lower=1> nu;
}

model{
  y ~ student_t(nu, a * x + b, sigma);
  a ~ normal(0, 10); 
  b ~ normal(0, 10); 
  nu ~ normal(1,10);
  sigma ~ normal(0, 10); 
}"
```

``` r
  t.mod = stan_model(model_code = stan_code_t)
  t.sample = sampling(t.mod, data=dat) 
```

``` r
  t_a = mean(extract(t.sample, 'a')$a)
  t_b = mean(extract(t.sample, 'b')$b)
  plot(y ~ x, dat, main='Robust regressions')
  curve(t_a*x + t_b, add = TRUE, lwd=2, col='black', lty=2)
  abline(lm(y ~ x, dat), col='red')
  abline(lm(y ~ x, d), col='green')
```

![](robustness_lsg_files/figure-gfm/t-dist-1.png)<!-- -->

``` r
  mean(extract(t.sample, 'a')$a) #0.36
```

    ## [1] 0.3601904

``` r
  #Note that this is quite some difference between 0.36 and 0.13
```
