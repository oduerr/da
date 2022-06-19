
# Correlated Hierarchical Model

This notebook introduces correlated hierarchical models by modeling the
results of the English premier league.  
See
<https://github.com/oduerr/da/blob/master/stan/football/Hierarchical.md>
for details on the dataset and references to the data set.

## Data preparation

We first load the data:

``` r
lsg = TRUE #Set to FALSE before submitting
require(rstan)
```

    ## Loading required package: rstan

    ## Loading required package: StanHeaders

    ## Loading required package: ggplot2

    ## rstan (Version 2.21.2, GitRev: 2e1f913d3ca3)

    ## For execution on a local, multicore CPU with excess RAM we recommend calling
    ## options(mc.cores = parallel::detectCores()).
    ## To avoid recompilation of unchanged Stan programs, we recommend calling
    ## rstan_options(auto_write = TRUE)

``` r
set.seed(1) #set seed 
options(mc.cores = parallel::detectCores())
data = read.csv('https://raw.githubusercontent.com/MaggieLieu/STAN_tutorials/master/Hierarchical/premiereleague.csv',col.names = c('Home','score1', 'score2', 'Away'), stringsAsFactors = FALSE)

ng = nrow(data)
cat('we have G =', ng, 'games \n')
```

    ## we have G = 328 games

``` r
nt = length(unique(data$Home))
cat('We have T = ', nt, 'teams \n')
```

    ## We have T =  20 teams

``` r
teams = unique(data$Home)
ht = unlist(sapply(1:ng, function(g) which(teams == data$Home[g])))
at = unlist(sapply(1:ng, function(g) which(teams == data$Away[g])))

np=200
ngob = ng-np #number of games to fit
my_data = list(
  nt = nt, 
  ng = ngob,
  ht = ht[1:ngob], 
  at = at[1:ngob], 
  s1 = data$score1[1:ngob],
  s2 = data$score2[1:ngob],
  np = np,
  htnew = ht[(ngob+1):ng],
  atnew = at[(ngob+1):ng]
)
cat('We use my_data$np = ', my_data$ng, 'games for training and my_data$np =', my_data$np ,' for testing \n')
```

    ## We use my_data$np =  128 games for training and my_data$np = 200  for testing

# Correlated Hierarchical Model

Football (soccer) is an ideal example to get a hand on hierarchical
models with correlated priors. For a introduction using intercept and
slope models see,
e.g. <https://mc-stan.org/docs/2_29/stan-users-guide/multivariate-hierarchical-priors.html>

## Multivariate Priors

Let’s start to understand multivariate priors, by telling the data
generating story. We start to describe how the prior is generated.
Without seeing any games, the attack and defense ability of a team *j*
is sampled from a multivariate normal MVN with the covariance matrix *Σ*
.

$$
 \\begin{bmatrix}att_j \\\\ def_j \\end{bmatrix} \\sim N(0,\\Sigma) = N (\\begin{bmatrix}0 \\\\ 0 \\end{bmatrix} ,\\begin{bmatrix} \\sigma^2\_{\\tt{att}} & cov(att,def) \\\\ cov(att,def) & \\sigma^2\_{\\tt{def}} \\end{bmatrix} 
$$

Averaged over all teams in the league, the attack abilities sum up to
zero (at least for a huge league). The same is true for the defense
abilities. Note that this is a desired property since we other wise
would have to enforce that the abilities sum up to zero (as we needed to
do in
<https://github.com/oduerr/da/blob/master/stan/football/Hierarchical.md>).
To describe this model have 3 parameters: the spread of the defense and
attack abilities $\\sigma\_\\tt{att} \\ge 0$ and
$\\sigma\_\\tt{def} \\ge 0$ and the covariance
$cov(\\tt{att}, \\tt{def})\\in \[-\\infty, \\infty\]$ between both
quantities. A more interpretable quantity then the covariance is the
correlation $\\rho\_{1,2}=cor(\\tt{att},\\tt{def})\\in\[-1,1\]$, which
can be calculated via
$cov(\\tt{att}, \\tt{def}) = cor(\\tt{att},\\tt{def}) \\sigma\_\\tt{att} \\sigma\_\\tt{def}$.
In principle, we can construct the covariance matrix from the
correlation matrix *ρ* as follows (*ρ* is the matrix in the middle) :

$$ \\Sigma = \\begin{bmatrix} \\sigma\_{\\tt{att}} & 0 \\\\ 0 & \\sigma\_{\\tt{def}} \\end{bmatrix} \\cdot \\begin{bmatrix} 1 & \\rho\_{1,2} \\\\ \\rho\_{1,2} & 1 \\end{bmatrix} \\cdot \\begin{bmatrix} \\sigma\_{\\tt{att}} & 0 \\\\ 0 & \\sigma\_{\\tt{def}} \\end{bmatrix}
$$

This matrix multiplication can be done in stan as follows

``` stan
diag_matrix(sigma) * rho * diag_matrix(sigma)
```

or in a single command

``` stan
quad_form_diag(rho, sigma)
```

where `quad_form_diag` is a performant implementation of the three
matrix multiplication. We could take the following stan code to sample,
from a multivariate Gaussian:

``` stan
data {
  int<lower=0> K;
  vector[K] sigma;
  matrix[K,K] rho;
  int<lower=0> J;
}
transformed data{
  vector[K] mu = rep_vector(0, K);
}
generated quantities {
  matrix[J,2] samples;
  matrix[K, K] Sigma;
  Sigma = quad_form_diag(rho, sigma);
  for (j in 1:J) {
    samples[j] = multi_normal_rng(mu, Sigma)';
  }
}
```

Let’s test it. To be less depended on fluctuations, we choose a large
league with *J* = 20000 teams, i.e. we sample 20000 draws and set
$\\sigma\_{\\tt att}$=1.0 and $\\sigma\_{\\tt def}=2.0$ and the
correlation to $cor({\\tt att}, {\\tt def})=0.8$.

``` r
  sigma = c(1,2)
  rho = matrix(c(1,0.8,0.8,1), ncol=2)
  dat = list(J=20000, sigma=sigma, rho=rho, K=2)
  s_rstan = rstan::sampling(model_1, data=dat, algorithm="Fixed_param", chain=1, iter=1)  
  mvsamples = rstan::extract(s_rstan, 'samples')$samples[1,,]
  colnames(mvsamples) = c('att', 'def')
  sd(mvsamples[,1])  #should be 1
  sd(mvsamples[,2])  #should be 2
  cor(mvsamples[,1], mvsamples[,2]) #should be 0.8
```

``` r
library(magrittr)
```

    ## 
    ## Attaching package: 'magrittr'

    ## The following object is masked from 'package:rstan':
    ## 
    ##     extract

``` r
library(ggplot2)
data.frame(mvsamples[1:1000,]) %>% 
ggplot(aes(x=att, y=def)) +
  geom_point(size=0.4, alpha = 0.3) + 
  stat_ellipse(col='blue', level = 0.9) + 
  stat_ellipse(col='blue', level = 0.3) 
```

![](Correlated_files/figure-gfm/stan_cor_plot-1.png)<!-- -->

### Cholesky Decomposition

Sampling from a correlated MVN *N*(0,*Σ*) is quite time consuming.
Especially for repeated draws this can be done much faster and elegantly
via the Cholesky decomposition:

$$
 \\begin{bmatrix}att_j \\\\ def_j \\end{bmatrix} = \\begin{bmatrix} \\sigma\_{\\tt{att}} & 0 \\\\ 0 & \\sigma\_{\\tt{def}} \\end{bmatrix} \\cdot L' \\cdot Z
\\quad \\quad (2)
$$

Where *Z* is a 2 x J matrix with elements from *N*(0,1) and *L*′ the
transposed Cholesky decomposition matrix *L* of the **correlation**
matrix *ρ*. This decomposition is great, since we can give more
intuitively priors for the **correlation** (between two variables) and
**spread** (of the single variable, for example the attack abilities for
all teams) compared to the covariance matrix.

#### Checking the Cholesky Decomposition (base R)

Let’s check decomposition eq. (2) first, before we continue. Say
$\\sigma\_{\\tt att}$=1.0 and $\\sigma\_{\\tt def}=2.0$ and the
correlation is $cor({\\tt att}, {\\tt def})=0.8$.

``` r
set.seed(42)
J = 20000 #teams
(S = matrix(c(1,0,0,2), nrow=2))
```

    ##      [,1] [,2]
    ## [1,]    1    0
    ## [2,]    0    2

``` r
(L = chol(rho)) #Cholesky of Correlation
```

    ##      [,1] [,2]
    ## [1,]    1  0.8
    ## [2,]    0  0.6

``` r
t(L) %*% L
```

    ##      [,1] [,2]
    ## [1,]  1.0  0.8
    ## [2,]  0.8  1.0

``` r
Z = matrix(rnorm(2*J), nrow = 2)
samples = S %*% t(L) %*% Z
#plot(samples[1,], samples[2,], xlab='att', ylab='def', main='Random draws from hyperprior (20 Teams)')
cor(t(samples)) #0.7971283 should be 0.8
```

    ##           [,1]      [,2]
    ## [1,] 1.0000000 0.7971283
    ## [2,] 0.7971283 1.0000000

``` r
sd(samples[1,]) #1.007653 should be 1
```

    ## [1] 1.007653

``` r
sd(samples[2,]) #2.008362 should be 2
```

    ## [1] 2.008362

``` r
samples = t(samples)
colnames(samples) = c('att', 'def')
data.frame(samples[1:1000,]) %>% 
  ggplot(aes(x=att, y=def)) +
    geom_point(size=0.4, alpha = 0.3) + 
    stat_ellipse(col='blue', level = 0.9) + 
    stat_ellipse(col='blue', level = 0.3) +
    labs(title = 'Samples using the Choslesky Decomposition')
```

![](Correlated_files/figure-gfm/cor_cholesky-1.png)<!-- -->

We see that the samples are indeed from a MVG.

In stan the equation (2) can be coded as:

``` stan
//sigma a vector holding the variances
//L Cholesky decomposition
//Z ~ N(0,1)
diag_pre_multiply(sigma, L) * Z
```

## Coding the strength of the teams

We just saw that the Cholesky Decomposition allows to transform
independend standard Gaussian to a correlated MVG. Before seeing any
data the attack and defense abilities of each team can be constructed as
follows. First, two random variables are drawn from a Gaussian. Then
these two parameters are transformed to a MVG using the Cholesky
Decomposition and $\\sigma\_{\\tt att}$ and $\\sigma\_{\\tt def}$. The
later transformation is the same for all teams. The abilities of the
individual teams can be described by two variables for all teams that
makes a matrix *A*<sub>*z*</sub> with dimension (2,*J*), with *J* the
number of teams.

### Summing up

We have the following parameters:

For all teams in common (hyperparameters):

-   `L_u` the Cholesky Matrix which stores the attack and defense
    correlation. For the two dimensional case in football this is one
    variable.
-   `sigma_u` which corresponds to the spread, here two variables
    $\\sigma\_{\\tt att}$ and $\\sigma\_{\\tt def}$.
-   `home` the home advantage

For all teams individually (Parameters):

-   The matrix `A_z` with dimension (2,*J*) which describes the strength
    of the teams in an uncorrelated space.

### Setting priors

The model is constructed such that *A*<sub>*z*</sub> ∼ *N*(0,1). We set
both $\\sigma\_{\\tt att} \\sim {\\tt exponential(1.0)}$ and
$\\sigma\_{\\tt def} \\sim {\\tt exponential(1.0)}$.

How to set the correlation coefficient? In principle, we could argue
that there should be a positive correlation, good teams are better in
defense and attack. However, its quite convenient to use a standard
prior for correlation. This is the `LKJcorr` prior which produces a
correlation from nearly uniform for parameter value 1 to centered around
zero for larger parameter values.

The rest stays the same as before.

``` r
cfit = stan(file = 'hier_model_cor.stan', data = my_data)
```

#### Checking the fit (seem to be OK)

``` r
rstan::check_divergences(cfit)
```

    ## 0 of 4000 iterations ended with a divergence.

``` r
rstan::check_treedepth(cfit)
```

    ## 0 of 4000 iterations saturated the maximum tree depth of 10.

``` r
bayesplot::mcmc_rhat_hist(bayesplot::rhat(cfit))
```

    ## Warning: Dropped 2 NAs from 'new_rhat(rhat)'.

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Correlated_files/figure-gfm/checks-1.png)<!-- -->

``` r
bayesplot::mcmc_neff_hist(bayesplot::neff_ratio(cfit))
```

    ## Warning: Dropped 2 NAs from 'new_neff_ratio(ratio)'.

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Correlated_files/figure-gfm/checks-2.png)<!-- -->

### Posterior correlation

Determining of the posterior correlations

``` r
L_us = rstan::extract(cfit)$L_u
dim(L_us)
```

    ## [1] 4000    2    2

``` r
L_us[1,1:2,1:2] %*% t(L_us[1,1:2,1:2])
```

    ##           [,1]      [,2]
    ## [1,] 1.0000000 0.5747204
    ## [2,] 0.5747204 1.0000000

There seems quite a correlation between the attack and defense
capabilities of the individual teams. Let’s have a look at the
posteriors for the individual teams.

``` r
A = rstan::extract(cfit, 'A_z')$A_z #4000    2   20
attack = colMeans(A)[1,1:20]
defense = colMeans(A)[2,1:20]
plot(attack,defense, pch=20, xlim=c(-1.5,2.0), ylim=c(-1,1))
text(attack,defense, labels=teams, cex=0.7, adj=c(-0.05,-0.8) )
```

![](Correlated_files/figure-gfm/cap_posterios-1.png)<!-- -->

For predictive performance see:
<https://github.com/oduerr/da/blob/master/stan/football/performance_w_nll.png>

### 
