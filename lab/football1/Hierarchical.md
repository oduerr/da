
# Excercise:

This notebook compares a non-hierarchical model and a hierarchical model
to predict the scores of English premier league matches.

## Your tasks

Step through the notebook and try to understand it. Try to answer the
following questions:

- Why the $log$ in $log(theta_g2) = att_at + def_ht$?
- What are the effective number of parameters in the hierarchical model
  and non-hierarchical model (use the output of the `loo` function)?
- What is the home advantage in the non-hierarchical model and the
  hierarchical model?
- Change the number of games for training. How does the performance of
  the models change?

### Background

This notebook is a tutorial on Hierarchical modelling using Stan which
is adopted from
<https://github.com/MaggieLieu/STAN_tutorials/blob/master/Hierarchical/Hierarchical.Rmd>.
The model is also described in the paper:
<https://discovery.ucl.ac.uk/id/eprint/16040/1/16040.pdf>

Modifications include:

- Removing of the pooled model

- Removing the fixed prior for home advantage (which has a std of about
  1e-4)

- Nevertheless, it appears necessary, as mentioned in the paper, to
  implement a sum-to-zero restriction for both the defense and offense.
  This stems from the fact that teams compete against one another,
  making these abilities dependent on each other. For further details,
  please refer to the aforementioned paper and its associated
  references.

``` r
lsg = TRUE #Set to FALSE before submitting
#require(rstan)
#set.seed(1) #set seed 
library(cmdstanr)
```

    ## This is cmdstanr version 0.8.0.9000

    ## - CmdStanR documentation and vignettes: mc-stan.org/cmdstanr

    ## - CmdStan path: /Users/oli/.cmdstan/cmdstan-2.34.1

    ## - CmdStan version: 2.34.1

    ## 
    ## A newer version of CmdStan is available. See ?install_cmdstan() to install it.
    ## To disable this check set option or environment variable cmdstanr_no_ver_check=TRUE.

``` r
options(mc.cores = parallel::detectCores())
```

### Reading the data

First we read in the data

``` r
data = read.csv('https://raw.githubusercontent.com/MaggieLieu/STAN_tutorials/master/Hierarchical/premiereleague.csv',col.names = c('Home','score1', 'score2', 'Away'), stringsAsFactors = FALSE)
```

``` r
head(data)
```

    ##                Home score1 score2                   Away
    ## 1   West Ham United      0      5        Manchester City
    ## 2           Burnley      3      0            Southampton
    ## 3    Crystal Palace      0      0                Everton
    ## 4           Watford      0      3 Brighton & Hove Albion
    ## 5   AFC Bournemouth      1      1       Sheffield United
    ## 6 Tottenham Hotspur      3      1            Aston Villa

``` r
ng = nrow(data)
cat('we have G =', ng, 'games \n')
```

    ## we have G = 328 games

``` r
nt = length(unique(data$Home))
cat('We have T = ', nt, 'teams \n')
```

    ## We have T =  20 teams

### Data Preparation

Now convert team names for each match into numbers (for the lookup)

``` r
#Some R-Magic to convert the team names to numbers, no need to understand this
teams = unique(data$Home)
ht = unlist(sapply(1:ng, function(g) which(teams == data$Home[g])))
at = unlist(sapply(1:ng, function(g) which(teams == data$Away[g])))
```

``` r
# we will save the last np games to predict
np=200
ngob = ng-np #ngames obsered ngob = number of games to fit
print(paste0("Using the first ", ngob, " games to fit the model and ", np, " games to predict"))
```

    ## [1] "Using the first 128 games to fit the model and 200 games to predict"

``` r
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
```

### Traditional (non-hierachical) method

We will assume that the goals scored come from a poisson distribution

- s1 ~ Poisson(theta_g1) \#game g score by home team

- s2 ~ Poisson(theta_g2) \#game g score by away team

Assuming a log-linear random effect model

- log(theta_g1) = home + att_ht + def_at

- log(theta_g2) = att_at + def_ht

Where home is a constant for the advantage for the team hosting the game
att and def are the attack and defense abilities of the teams where the
indices at,ht correspond to the t=1-20 teams.

``` r
library(here)
library(cmdstanr)
nhmodel <- cmdstan_model(here('lab/football1/non_hier_model.stan'))
nhfit = nhmodel$sample(data = my_data)
d = loo::loo(nhfit$draws("log_lik"))
print(d)
```

#### Non-Hierarchical model predicted scores of the unseen

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.1     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.1     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
s1new = nhfit$draws(format = "df", 's1new') %>% select(-.chain, -.iteration, -.draw)
```

    ## Warning: Dropping 'draws_df' class as required metadata was removed.

``` r
s2new = nhfit$draws(format = "df", 's2new') %>% select(-.chain, -.iteration, -.draw)
```

    ## Warning: Dropping 'draws_df' class as required metadata was removed.

``` r
pred_scores = c(colMeans(s1new),colMeans(s2new))
true_scores = c(data$score1[(ngob+1):ng],data$score2[(ngob+1):ng] )
plot(true_scores, pred_scores, xlim=c(0,5), ylim=c(0,5), pch=20, ylab='predicted scores', xlab='true scores')
abline(a=0,  b=1, lty='dashed')
```

![](Hierarchical_files/figure-gfm/non_hier_pred_scores-1.png)<!-- -->

``` r
sqrt(mean((pred_scores - true_scores)^2))
```

    ## [1] 1.203396

``` r
cor(pred_scores, true_scores)
```

    ## [1] 0.3170073

Below is a function to calculate the score of the model. Using the
following rules:

1.  Exact Score Prediction: 3 Points are awarded if both the predicted
    home and away scores match the actual scores exactly.
2.  Draw Prediction: 1 Point is awarded if the predicted result is a
    draw (both teams score the same number of goals) and the actual
    result is also a draw.
3.  Correct Outcome Prediction: 1 Point is awarded if the predicted
    outcome (win/loss) matches the actual outcome, even if the exact
    scores do not match.

``` r
get_score = function(s1new, s2new, data){
  s1_pred_score = round(colMeans(s1new),0)
  s2_pred_score = round(colMeans(s2new),0)
  points = 0
  for (i in 1:length(s1_pred_score)){
      pred_scorediff = s1_pred_score - s2_pred_score
      if (s1_pred_score[i] == data$score1[ngob+i]
          && s2_pred_score[i] == data$score2[ngob+i]){
        points = points + 3
      } else {
        if (s1_pred_score[i] == s2_pred_score[i]){ #Call it a draw
          if (data$score1[ngob+i] == data$score2[ngob+i]) {
            points = points + 1
          }
        } else {#No draw
           if(sign(pred_scorediff[i]) == sign(data$score1[ngob+i] - data$score2[ngob+i])){
             points = points + 1
           }
        }
      } 
  }
  return (points)
}
get_score(s1new, s2new, data)
```

    ## [1] 144

#### Defense / Attack Non-Hierachical

We can also look at the attack/defense of the teams:

``` r
attack = nhfit$draws(format = "df", 'att') %>% 
  select(-.chain, -.iteration, -.draw) %>% colMeans()
```

    ## Warning: Dropping 'draws_df' class as required metadata was removed.

``` r
defense = nhfit$draws(format = "df", 'def') %>% 
  select(-.chain, -.iteration, -.draw) %>% colMeans()
```

    ## Warning: Dropping 'draws_df' class as required metadata was removed.

``` r
plot(attack,defense,xlim=c(-0.4,1.1))
abline(h=0)
abline(v=0)
text(attack,defense, labels=teams, cex=0.7, pos=4)
```

![](Hierarchical_files/figure-gfm/attack_defense_non_hier-1.png)<!-- -->

### Your Excercise / Determine the home advantage

Plot the posterior of the variable `home`

``` r
library(tidybayes)
#home_adv = rstan::extract(nhfit)$home
#hist(home_adv, 100, freq = FALSE, main=paste0("Home Adv, mean: ", round(mean(home_adv),2)))
#lines(density(home_adv))

tidy_draws(nhfit) %>% 
  select(home) %>% 
  ggplot(aes(x=home)) + geom_density() 
```

![](Hierarchical_files/figure-gfm/home_adv_non_hier-1.png)<!-- -->

# Hierarchical model

In a hierarchical model, the parameters of interest, in our case the
attack and defense ability are drawn from the population distribution.

- att\[t\] ~ normal(mu_att, tau_att)

- def\[t\] ~ normal(mu_def, tau_def)

Instead we define priors on the population, known as the hyperpriors.

``` r
hmodel <- cmdstan_model(here('lab/football1/hier_model.stan')) 
hfit = hmodel$sample(data = my_data)
```

## Checking the chain

``` r
hfit
```

    ##    variable    mean  median   sd  mad      q5     q95 rhat ess_bulk ess_tail
    ##  lp__       -183.71 -184.99 8.91 6.89 -195.55 -164.59 1.04       97       39
    ##  home          0.35    0.35 0.08 0.08    0.23    0.47 1.00     2144     2906
    ##  att_raw[1]   -0.09   -0.09 0.20 0.20   -0.42    0.22 1.00     1107     2142
    ##  att_raw[2]    0.07    0.07 0.19 0.18   -0.25    0.37 1.00     4287     2839
    ##  att_raw[3]   -0.30   -0.28 0.22 0.21   -0.66    0.06 1.00     4560     2603
    ##  att_raw[4]   -0.48   -0.47 0.24 0.23   -0.90   -0.11 1.00     3265     2946
    ##  att_raw[5]   -0.10   -0.09 0.20 0.20   -0.45    0.22 1.01     3989     2864
    ##  att_raw[6]    0.12    0.11 0.19 0.19   -0.19    0.43 1.00     3760     2655
    ##  att_raw[7]    0.48    0.48 0.18 0.18    0.19    0.77 1.00     3342     2800
    ##  att_raw[8]   -0.27   -0.26 0.24 0.24   -0.70    0.10 1.01      559      190
    ## 
    ##  # showing 10 of 1268 rows (change via 'max_rows' argument or 'cmdstanr_max_rows' option)

``` r
bayesplot::mcmc_trace(hfit$draws(c("mu_att", "lp__")))
```

![](Hierarchical_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
loo::loo(hfit$draws("log_lik"))
```

    ## Warning: Relative effective sample sizes ('r_eff' argument) not specified.
    ## For models fit with MCMC, the reported PSIS effective sample sizes and 
    ## MCSE estimates will be over-optimistic.

    ## Warning: Some Pareto k diagnostic values are slightly high. See help('pareto-k-diagnostic') for details.

    ## 
    ## Computed from 4000 by 128 log-likelihood matrix
    ## 
    ##          Estimate   SE
    ## elpd_loo   -386.0 13.3
    ## p_loo        23.0  3.5
    ## looic       772.0 26.6
    ## ------
    ## Monte Carlo SE of elpd_loo is 0.1.
    ## 
    ## Pareto k diagnostic values:
    ##                          Count Pct.    Min. n_eff
    ## (-Inf, 0.5]   (good)     126   98.4%   655       
    ##  (0.5, 0.7]   (ok)         2    1.6%   309       
    ##    (0.7, 1]   (bad)        0    0.0%   <NA>      
    ##    (1, Inf)   (very bad)   0    0.0%   <NA>      
    ## 
    ## All Pareto k estimates are ok (k < 0.7).
    ## See help('pareto-k-diagnostic') for details.

``` r
loo::loo(nhfit$draws("log_lik"))
```

    ## Warning: Relative effective sample sizes ('r_eff' argument) not specified.
    ## For models fit with MCMC, the reported PSIS effective sample sizes and 
    ## MCSE estimates will be over-optimistic.
    ## Warning: Some Pareto k diagnostic values are slightly high. See help('pareto-k-diagnostic') for details.

    ## 
    ## Computed from 4000 by 128 log-likelihood matrix
    ## 
    ##          Estimate   SE
    ## elpd_loo   -392.3 11.8
    ## p_loo        35.4  3.6
    ## looic       784.7 23.6
    ## ------
    ## Monte Carlo SE of elpd_loo is 0.2.
    ## 
    ## Pareto k diagnostic values:
    ##                          Count Pct.    Min. n_eff
    ## (-Inf, 0.5]   (good)     122   95.3%   239       
    ##  (0.5, 0.7]   (ok)         6    4.7%   502       
    ##    (0.7, 1]   (bad)        0    0.0%   <NA>      
    ##    (1, Inf)   (very bad)   0    0.0%   <NA>      
    ## 
    ## All Pareto k estimates are ok (k < 0.7).
    ## See help('pareto-k-diagnostic') for details.

## Prediction of Hierarchical Model

``` r
s1new = hfit$draws(format = "df", 's1new') %>% select(-.chain, -.iteration, -.draw)
```

    ## Warning: Dropping 'draws_df' class as required metadata was removed.

``` r
s2new = hfit$draws(format = "df", 's2new') %>% select(-.chain, -.iteration, -.draw)
```

    ## Warning: Dropping 'draws_df' class as required metadata was removed.

``` r
pred_scores = c(colMeans(s1new),colMeans(s2new))
true_scores = c(data$score1[(ngob+1):ng],data$score2[(ngob+1):ng] )
plot(true_scores, pred_scores, xlim=c(0,5), ylim=c(0,5), pch=20, ylab='predicted scores', xlab='true scores')
abline(a=0,  b=1, lty='dashed')
```

![](Hierarchical_files/figure-gfm/hpreds-1.png)<!-- -->

``` r
sqrt(mean((pred_scores - true_scores)^2))
```

    ## [1] 1.108828

``` r
cor(pred_scores, true_scores)
```

    ## [1] 0.3403446

``` r
 get_score(s1new, s2new, data)
```

    ## [1] 147

### Mean attack and dense abilities

``` r
attack = hfit$draws(format = "df", 'att') %>% 
  select(-.chain, -.iteration, -.draw) %>% colMeans()
```

    ## Warning: Dropping 'draws_df' class as required metadata was removed.

``` r
defense = hfit$draws(format = "df", 'def') %>% 
  select(-.chain, -.iteration, -.draw) %>% colMeans()
```

    ## Warning: Dropping 'draws_df' class as required metadata was removed.

``` r
plot(attack,defense,xlim=c(-0.4,1.1))
abline(h=0)
abline(v=0)
text(attack,defense, labels=teams, cex=0.7, pos=4)
```

![](Hierarchical_files/figure-gfm/attack_defense_hier-1.png)<!-- -->

#### Home Advantage h-model you job!

``` r
tidy_draws(hfit) %>% 
  select(home) %>% 
  ggplot(aes(x=home)) + geom_density() 
```

![](Hierarchical_files/figure-gfm/home_adv_hier-1.png)<!-- -->
