Understanding LOO
-----------------

Some Data for linear regression (same as before)

``` r
lsg = TRUE
N = 4
x = c(-2.,-0.66666, 0.666, 2.)
y = c(-6.25027354, -2.50213382, -6.07525495,  7.92081243)

N = 10
SD = 0.3
A = 1
B = 2
x = seq(-2,2,length.out = N)
y = rnorm(N,A*x+B, sd=SD)
plot(x,y)
```

![](bayes5_loo_files/figure-markdown_github/unnamed-chunk-1-1.png)

1.  Fit the and check the results

``` r
  library(rstan)
  fit = stan(file = 'lr.stan', data=list(N=length(y),y=y,x=x))
  fit
```

1.  Theoretical result

``` r
  library(loo)
```

    ## Warning: package 'loo' was built under R version 3.5.2

    ## This is loo version 2.2.0

    ## - Online documentation and vignettes at mc-stan.org/loo

    ## - As of v2.0.0 loo defaults to 1 core but we recommend using as many as possible. Use the 'cores' argument or set options(mc.cores = NUM_CORES) for an entire session.

    ## 
    ## Attaching package: 'loo'

    ## The following object is masked from 'package:rstan':
    ## 
    ##     loo

``` r
  a_sam = A
  b_sam = B
  s_sam = SD
  mean(-log(dnorm(y,a_sam * x + b_sam, s_sam))) #NLL
```

    ## [1] 0.3200139
