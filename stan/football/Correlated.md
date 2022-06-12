
# Correlated Hierarchical Model

This notebook introduces correlated hierarchical models. Before we
start, we read in the data (See
<https://github.com/oduerr/da/blob/master/stan/football/Hierarchical.md>
for details on the dataset)

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

Football is an ideal example to get a hand on hierarchical models. Let’s
start to understand by telling the data generating story. We start to
describe how the prior is generated. Without seeing any games, the
attack and defense ability of a team *j* is sampled from a multivariate
normal MVN.

$$
 \\begin{bmatrix}att_j \\\\ def_j \\end{bmatrix} \\sim N (\\begin{bmatrix}0 \\\\ 0 \\end{bmatrix} ,\\begin{bmatrix} \\sigma^2\_{\\tt{att}} & cov(att,def) \\\\ cov(att,def) & \\sigma^2\_{\\tt{def}} \\end{bmatrix}
$$
Averaged over all teams in the league, the attack abilities sum up to
zero (at least for a huge league). The same is true for the defense
abilities. Note that this is a desired property since we other wise
would have to enforce that the abilities sum up to zero (as we needed to
do in
<https://github.com/oduerr/da/blob/master/stan/football/Hierarchical.md>)

We can sample from this MVN very elegantly via the Cholesky
decomposition:

$$
 \\begin{bmatrix}att_j \\\\ def_j \\end{bmatrix} = \\begin{bmatrix} \\sigma\_{\\tt{att}} & 0 \\\\ 0 & \\sigma\_{\\tt{def}} \\end{bmatrix} \\cdot L' \\cdot Z
$$

Where *Z* is a 2 x J matrix with elements from *N*(0,1) and *L*′ the
transposed Cholesky decompostion matrix *L* of the **correlation**
matrix. This decomposition is great, since we can give more intuitively
priors for the **correlation** (between two variables) and **spread**
(of the single variable, for example the attack abilities for all teams)
compared to the covariance matrix.

### Checking the Cholesky Decomposition

Let’s check decomposition first, before we continue. Say
$\\sigma\_{\\tt att}$=1.0 and $\\sigma\_{\\tt def}=2.0$ and the
correlation is $cor({\\tt att}, {\\tt def})=0.8$ .

``` r
set.seed(42)
J = 20 #teams
(S = matrix(c(1,0,0,2), nrow=2))
```

    ##      [,1] [,2]
    ## [1,]    1    0
    ## [2,]    0    2

``` r
(L = chol(matrix(c(1,0.8,0.8,1), nrow=2))) #Cholesky of Correlation
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
plot(samples[1,], samples[2,], xlab='att', ylab='def', main='Random draws from hyperprior (20 Teams)')
```

![](Correlated_files/figure-gfm/samples_teams-1.png)<!-- -->

``` r
cor(t(samples)) #0.8117834 should be 0.8
```

    ##           [,1]      [,2]
    ## [1,] 1.0000000 0.8117834
    ## [2,] 0.8117834 1.0000000

``` r
sd(samples[1,]) #1.250193 should be 1
```

    ## [1] 1.250193

``` r
sd(samples[2,]) #2.451003 should be 2
```

    ## [1] 2.451003

We see that the samples are indeed from a MVG. Increasing the number of
teams to infinity would lead to the exact values. Note that these priors
have been chosen to illustrate / validate Cholesky decomposition. Later
other priors will be used.

### Coding the strength of the teams

The dimension of the matrix Z, is (2,*J*), with *J* the number of teams.
At the beginning all elements are for attack and defense are standard
normal *N*(0,1) and this is then updated during the course of the games.

### Setting priors

We set both $\\sigma\_{\\tt att} \\sim {\\tt exponential(1.0)}$ and
$\\sigma\_{\\tt def} \\sim {\\tt exponential(1.0)}$. How to set the
correlation coefficient? In principle, we could argue that there should
be a positive correlation, good teams are better in defense and attack.
However, its quite convenient to use a standard prior for correlation.
This is the `LKJcorr` prior which produces a correlation from nearly
uniform for parameter value 1 to centered around zero for larger
parameter values.

### Summing up

We have the following parameters

-   `L_u` the Choleski Matrix which stores the attack and defense
    correlation
-   `sigma_u` which corresponds to the spread

The rest stays the same as before.

``` r
cfit = stan(file = 'hier_model_cor.stan', data = my_data)
```

    ## Trying to compile a simple C file

#### Checking the fit (seem to be OK)

``` r
rstan::check_divergences(cfit)
```

    ## 0 of 4000 iterations ended with a divergence.

``` r
check_treedepth(cfit)
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
    ## [1,] 1.0000000 0.9288244
    ## [2,] 0.9288244 1.0000000

There seems quite a correlation between the attack and defense
capabilities of the individual teams. Let’s have a look at the
posteriors for the individual teams.

``` r
A = extract(cfit, 'A_z')$A_z #4000    2   20
attack = colMeans(A)[1,1:20]
defense = colMeans(A)[2,1:20]
plot(attack,defense, pch=20, xlim=c(-1.5,2.0), ylim=c(-1,1))
text(attack,defense, labels=teams, cex=0.7, adj=c(-0.05,-0.8) )
```

![](Correlated_files/figure-gfm/cap_posterios-1.png)<!-- -->

For predictive performance see:
<https://github.com/oduerr/da/blob/master/stan/football/performance_w_nll.png>

### 
