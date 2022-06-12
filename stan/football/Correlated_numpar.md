
# Problems with effective number of parameters

See
<https://github.com/oduerr/da/blob/master/stan/football/Hierarchical.md>
and references therein for details on the dataset.

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

![](Correlated_numpar_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
bayesplot::mcmc_neff_hist(bayesplot::neff_ratio(cfit))
```

    ## Warning: Dropped 2 NAs from 'new_neff_ratio(ratio)'.

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Correlated_numpar_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

### Checking the effective number of parameters

``` r
res = loo::loo(cfit)
```

    ## Warning: Some Pareto k diagnostic values are too high. See help('pareto-k-diagnostic') for details.

``` r
res
```

    ## 
    ## Computed from 4000 by 128 log-likelihood matrix
    ## 
    ##          Estimate     SE
    ## elpd_loo -60787.5 2487.1
    ## p_loo     11759.3  627.7
    ## looic    121575.0 4974.3
    ## ------
    ## Monte Carlo SE of elpd_loo is NA.
    ## 
    ## Pareto k diagnostic values:
    ##                          Count Pct.    Min. n_eff
    ## (-Inf, 0.5]   (good)       0     0.0%  <NA>      
    ##  (0.5, 0.7]   (ok)         0     0.0%  <NA>      
    ##    (0.7, 1]   (bad)        0     0.0%  <NA>      
    ##    (1, Inf)   (very bad) 128   100.0%  1         
    ## See help('pareto-k-diagnostic') for details.

``` r
(elpd.loo = res$elpd_loo) #All chains a very bad
```

    ## Warning: Accessing elpd_loo using '$' is deprecated and will be removed in
    ## a future release. Please extract the elpd_loo estimate from the 'estimates'
    ## component instead.

    ## [1] -60787.52

``` r
(p.loo = res$estimates['p_loo',1]) #11686.85
```

    ## [1] 11759.34
