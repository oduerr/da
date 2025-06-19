# Football with Bayes
Oliver Dürr

-   [Analysing the Bundesliga 2024/2025](#analysing-the-bundesliga-20242025)
-   [Loading the data](#loading-the-data)
-   [Preparing the data for Stan](#preparing-the-data-for-stan)
-   [A Model for the goals scored 🥅](#a-model-for-the-goals-scored)
    -   [Prior for the attack and defence strength](#prior-for-the-attack-and-defence-strength)
-   [Conditioning on the data / Fitting the model](#conditioning-on-the-data-fitting-the-model)
-   [Using the fitted model](#using-the-fitted-model)
    -   [Home Advantage](#home-advantage)
    -   [Attack and defense posteriors for the individual teams](#attack-and-defense-posteriors-for-the-individual-teams)
    -   [Expected goals for each game \[optional\]](#expected-goals-for-each-game-optional)

## Analysing the Bundesliga 2024/2025

This is basically a guided excercise with little input from you side. Just step through the code an try to understand it. Fokus not so much the plotting code, but the Stan code and larger principals.

## Loading the data

The data has been created with `https://github.com/oduerr/da/blob/master/stan/football/get_bundesliga_data.R`. But for you convenience, we have already prepared the data for you. You can donwload the data as follows:

``` r
# Uncomment the following line to read the data for the Euro 24
data <- read.csv('https://github.com/oduerr/da/raw/master/stan/football/bundesliga_2025.csv', stringsAsFactors = FALSE)
head(data)
##     Competition_Name Gender Country Season_End_Year      Round Wk Day
## 1 Fußball-Bundesliga      M     GER            2025 Bundesliga  1 Fri
## 2 Fußball-Bundesliga      M     GER            2025 Bundesliga  1 Sat
## 3 Fußball-Bundesliga      M     GER            2025 Bundesliga  1 Sat
## 4 Fußball-Bundesliga      M     GER            2025 Bundesliga  1 Sat
## 5 Fußball-Bundesliga      M     GER            2025 Bundesliga  1 Sat
## 6 Fußball-Bundesliga      M     GER            2025 Bundesliga  1 Sat
##         Date  Time       Home HomeGoals Home_xG          Away AwayGoals Away_xG
## 1 2024-08-23 20:30   Gladbach         2     1.6    Leverkusen         3     2.7
## 2 2024-08-24 15:30 Hoffenheim         3     3.5 Holstein Kiel         2     1.7
## 3 2024-08-24 15:30   Mainz 05         1     1.2  Union Berlin         1     0.6
## 4 2024-08-24 15:30   Augsburg         2     1.0 Werder Bremen         2     1.4
## 5 2024-08-24 15:30   Freiburg         3     2.1     Stuttgart         1     0.4
## 6 2024-08-24 15:30 RB Leipzig         1     1.3        Bochum         0     1.0
##   Attendance                    Venue          Referee Notes
## 1      54042 Stadion im Borussia-Park  Robert Schröder      
## 2      18503            PreZero Arena   Tobias Stieler      
## 3      31500               Mewa Arena      Harm Osmers      
## 4      30660                WWK Arena Sascha Stegemann      
## 5      34700      Europa-Park Stadion      Tobias Welz      
## 6      44611           Red Bull Arena   Benjamin Brand      
##                                                                                           MatchURL
## 1 https://fbref.com/en/matches/d42e53df/Monchengladbach-Bayer-Leverkusen-August-23-2024-Bundesliga
## 2         https://fbref.com/en/matches/b549a198/Hoffenheim-Holstein-Kiel-August-24-2024-Bundesliga
## 3            https://fbref.com/en/matches/02dc2f7e/Mainz-05-Union-Berlin-August-24-2024-Bundesliga
## 4           https://fbref.com/en/matches/967de8db/Augsburg-Werder-Bremen-August-24-2024-Bundesliga
## 5               https://fbref.com/en/matches/a64e4ea4/Freiburg-Stuttgart-August-24-2024-Bundesliga
## 6                https://fbref.com/en/matches/cd173a0a/RB-Leipzig-Bochum-August-24-2024-Bundesliga
##   ht at
## 1  7 11
## 2  9 10
## 3 12 16
## 4  1 17
## 5  6 15
## 6 13  3
```

## Preparing the data for Stan

``` r
ng = nrow(data)
teams = data$Home %>%
  unique() %>%
  sort()
nt = length(teams)
ht = data$ht
at = data$at

my_data = list(
  nt = nt, 
  ng = ng,
  ht = data$ht,
  at = data$at,
  s1 = data$HomeGoals,
  s2 = data$AwayGoals
)
```

## A Model for the goals scored 🥅

We will assume that the number of goals scored by the home team *s*<sub>1</sub> and the away team *s*<sub>2</sub> follows a Poisson distribution. This has been shown to be a good model for the number of goals scored in a football match. We model the rate parameter *θ* of the Poisson distribution, related to the attack and defense strengths of the teams, as follows:

*s*<sub>1</sub> ∼ Pois(*θ*<sub>1</sub>)  goals scored by the home team

*s*<sub>2</sub> ∼ Pois(*θ*<sub>2</sub>)  goals scored by the away team

This is equivalent to performing two separate Poisson regressions, one for each team.

We assume that:

*θ*<sub>1</sub> = exp (home+att<sub>ht</sub>−def<sub>at</sub>)

*θ*<sub>2</sub> = exp (att<sub>at</sub>−def<sub>ht</sub>)

### Prior for the attack and defence strength

In Bayesian statistics, we further need to specify a prior for the parameters (our degree of believe in the attack and defense abilities before seeing any data). For that we use a hierarchical model with correlated parameters, as described in the lecture. Have a look at the [Stan code](https://github.com/oduerr/da/blob/master/stan/football/hier_model_cor_nocholesky.stan) and try to understand it. Download the stan file, when you reproduce the analysis.

## Conditioning on the data / Fitting the model

After we state the model we fit the model to the data, or in Bayesian parlance, we update our degree of belief after seeing the data.

``` r
library(cmdstanr)
options(mc.cores = parallel::detectCores())
library(here)
hmodel = cmdstan_model(here("stan", "football", "hier_model_cor_nocholesky.stan"))
hfit = hmodel$sample(data = my_data)
## Running MCMC with 4 chains, at most 10 in parallel...
## 
## Chain 1 Iteration:    1 / 2000 [  0%]  (Warmup) 
## Chain 1 Iteration:  100 / 2000 [  5%]  (Warmup) 
## Chain 2 Iteration:    1 / 2000 [  0%]  (Warmup) 
## Chain 2 Iteration:  100 / 2000 [  5%]  (Warmup) 
## Chain 3 Iteration:    1 / 2000 [  0%]  (Warmup) 
## Chain 3 Iteration:  100 / 2000 [  5%]  (Warmup) 
## Chain 4 Iteration:    1 / 2000 [  0%]  (Warmup) 
## Chain 4 Iteration:  100 / 2000 [  5%]  (Warmup) 
## Chain 1 Iteration:  200 / 2000 [ 10%]  (Warmup) 
## Chain 1 Iteration:  300 / 2000 [ 15%]  (Warmup) 
## Chain 1 Iteration:  400 / 2000 [ 20%]  (Warmup) 
## Chain 2 Iteration:  200 / 2000 [ 10%]  (Warmup) 
## Chain 2 Iteration:  300 / 2000 [ 15%]  (Warmup) 
## Chain 2 Iteration:  400 / 2000 [ 20%]  (Warmup) 
## Chain 3 Iteration:  200 / 2000 [ 10%]  (Warmup) 
## Chain 3 Iteration:  300 / 2000 [ 15%]  (Warmup) 
## Chain 4 Iteration:  200 / 2000 [ 10%]  (Warmup) 
## Chain 4 Iteration:  300 / 2000 [ 15%]  (Warmup) 
## Chain 1 Iteration:  500 / 2000 [ 25%]  (Warmup) 
## Chain 1 Iteration:  600 / 2000 [ 30%]  (Warmup) 
## Chain 2 Iteration:  500 / 2000 [ 25%]  (Warmup) 
## Chain 2 Iteration:  600 / 2000 [ 30%]  (Warmup) 
## Chain 3 Iteration:  400 / 2000 [ 20%]  (Warmup) 
## Chain 3 Iteration:  500 / 2000 [ 25%]  (Warmup) 
## Chain 3 Iteration:  600 / 2000 [ 30%]  (Warmup) 
## Chain 4 Iteration:  400 / 2000 [ 20%]  (Warmup) 
## Chain 4 Iteration:  500 / 2000 [ 25%]  (Warmup) 
## Chain 4 Iteration:  600 / 2000 [ 30%]  (Warmup) 
## Chain 1 Iteration:  700 / 2000 [ 35%]  (Warmup) 
## Chain 1 Iteration:  800 / 2000 [ 40%]  (Warmup) 
## Chain 1 Iteration:  900 / 2000 [ 45%]  (Warmup) 
## Chain 2 Iteration:  700 / 2000 [ 35%]  (Warmup) 
## Chain 2 Iteration:  800 / 2000 [ 40%]  (Warmup) 
## Chain 2 Iteration:  900 / 2000 [ 45%]  (Warmup) 
## Chain 3 Iteration:  700 / 2000 [ 35%]  (Warmup) 
## Chain 3 Iteration:  800 / 2000 [ 40%]  (Warmup) 
## Chain 3 Iteration:  900 / 2000 [ 45%]  (Warmup) 
## Chain 4 Iteration:  700 / 2000 [ 35%]  (Warmup) 
## Chain 4 Iteration:  800 / 2000 [ 40%]  (Warmup) 
## Chain 1 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
## Chain 1 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
## Chain 1 Iteration: 1100 / 2000 [ 55%]  (Sampling) 
## Chain 2 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
## Chain 2 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
## Chain 2 Iteration: 1100 / 2000 [ 55%]  (Sampling) 
## Chain 3 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
## Chain 3 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
## Chain 3 Iteration: 1100 / 2000 [ 55%]  (Sampling) 
## Chain 4 Iteration:  900 / 2000 [ 45%]  (Warmup) 
## Chain 4 Iteration: 1000 / 2000 [ 50%]  (Warmup) 
## Chain 4 Iteration: 1001 / 2000 [ 50%]  (Sampling) 
## Chain 1 Iteration: 1200 / 2000 [ 60%]  (Sampling) 
## Chain 2 Iteration: 1200 / 2000 [ 60%]  (Sampling) 
## Chain 3 Iteration: 1200 / 2000 [ 60%]  (Sampling) 
## Chain 4 Iteration: 1100 / 2000 [ 55%]  (Sampling) 
## Chain 4 Iteration: 1200 / 2000 [ 60%]  (Sampling) 
## Chain 1 Iteration: 1300 / 2000 [ 65%]  (Sampling) 
## Chain 1 Iteration: 1400 / 2000 [ 70%]  (Sampling) 
## Chain 2 Iteration: 1300 / 2000 [ 65%]  (Sampling) 
## Chain 2 Iteration: 1400 / 2000 [ 70%]  (Sampling) 
## Chain 3 Iteration: 1300 / 2000 [ 65%]  (Sampling) 
## Chain 3 Iteration: 1400 / 2000 [ 70%]  (Sampling) 
## Chain 4 Iteration: 1300 / 2000 [ 65%]  (Sampling) 
## Chain 1 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
## Chain 1 Iteration: 1600 / 2000 [ 80%]  (Sampling) 
## Chain 2 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
## Chain 2 Iteration: 1600 / 2000 [ 80%]  (Sampling) 
## Chain 3 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
## Chain 3 Iteration: 1600 / 2000 [ 80%]  (Sampling) 
## Chain 4 Iteration: 1400 / 2000 [ 70%]  (Sampling) 
## Chain 4 Iteration: 1500 / 2000 [ 75%]  (Sampling) 
## Chain 1 Iteration: 1700 / 2000 [ 85%]  (Sampling) 
## Chain 2 Iteration: 1700 / 2000 [ 85%]  (Sampling) 
## Chain 3 Iteration: 1700 / 2000 [ 85%]  (Sampling) 
## Chain 3 Iteration: 1800 / 2000 [ 90%]  (Sampling) 
## Chain 4 Iteration: 1600 / 2000 [ 80%]  (Sampling) 
## Chain 4 Iteration: 1700 / 2000 [ 85%]  (Sampling) 
## Chain 1 Iteration: 1800 / 2000 [ 90%]  (Sampling) 
## Chain 1 Iteration: 1900 / 2000 [ 95%]  (Sampling) 
## Chain 2 Iteration: 1800 / 2000 [ 90%]  (Sampling) 
## Chain 2 Iteration: 1900 / 2000 [ 95%]  (Sampling) 
## Chain 3 Iteration: 1900 / 2000 [ 95%]  (Sampling) 
## Chain 4 Iteration: 1800 / 2000 [ 90%]  (Sampling) 
## Chain 1 Iteration: 2000 / 2000 [100%]  (Sampling) 
## Chain 2 Iteration: 2000 / 2000 [100%]  (Sampling) 
## Chain 3 Iteration: 2000 / 2000 [100%]  (Sampling) 
## Chain 4 Iteration: 1900 / 2000 [ 95%]  (Sampling) 
## Chain 4 Iteration: 2000 / 2000 [100%]  (Sampling) 
## Chain 1 finished in 1.2 seconds.
## Chain 2 finished in 1.2 seconds.
## Chain 3 finished in 1.1 seconds.
## Chain 4 finished in 1.2 seconds.
## 
## All 4 chains finished successfully.
## Mean chain execution time: 1.2 seconds.
## Total execution time: 1.4 seconds.
p1 = bayesplot::mcmc_rhat_hist(bayesplot::rhat(hfit))
p2 = bayesplot::mcmc_neff_hist(bayesplot::neff_ratio(hfit))
ggpubr::ggarrange(p1, p2, ncol=2)
```

![](football_hierachical.markdown_strict_files/figure-markdown_strict/unnamed-chunk-2-1.png)

The fitting of the model is good, as the Rhat values are close to 1 and we have no divergent transitions. The effective sample size is also good.

## Using the fitted model

### Home Advantage

First we look at the home advantage.

``` r
  homes =  hfit %>% tidybayes::spread_draws(home) %>% select('home')
  hist(homes$home, 50)
  abline(v=0)
```

![](football_hierachical.markdown_strict_files/figure-markdown_strict/unnamed-chunk-3-1.png)

``` r
  #Probability that there is no home advantage
  mean(homes$home < 0)
```

    [1] 0.016

### Attack and defense posteriors for the individual teams

We plot the median of the attack and defense strengths of the teams together with the spread of the posterior. Shown are the median values along with the 25% and 75% quantiles. There is considerable uncertainty in the strengths of the teams, but that’s the nature of the game.

``` r
## 1 ─ draw A[i,j] into long format ----------------------------------------
A_draws <- hfit %>%                       # cmdstanr fit
  tidybayes::gather_draws(A[row, col])    # row = 1 (attack), row = 2 (defence)

A_summary <- A_draws %>% 
  group_by(row, col) %>%
  summarise(
    q25 = quantile(.value, 0.25),
    q50 = quantile(.value, 0.50),  # optional: median
    q75 = quantile(.value, 0.75),
    .groups = "drop"
  ) %>% 
  mutate(
    team = teams[col],
    role = if_else(row == 1, "attack", "defence")
  ) 


## 2 ─ pivot *only* by team
plot_data <- A_summary %>%
  select(-row, -col) %>%                          # drop row/col indices
  pivot_wider(
    id_cols   = team,
    names_from  = role,
    values_from = c(q25, q50, q75),
    names_glue  = "{role}_{.value}"
  )


ggplot(plot_data, aes(x = attack_q50, y = defence_q50, label = team)) +
  geom_point(size = 2.5, color = "blue") +
  geom_errorbarh(aes(xmin = attack_q25, xmax = attack_q75), height = 0.05, alpha = 0.6) +
  geom_errorbar(aes(ymin = defence_q25, ymax = defence_q75), width = 0.05, alpha = 0.6) +
  geom_text(hjust = -0.1, vjust = 0.5, size = 3.2) +
  labs(
    title = "Posterior team abilities: Attack vs Defence",
    x = "Attack (median ± 50% CrI)",
    y = "Defence (median ± 50% CrI)"
  ) +
  theme_minimal()
```

![](football_hierachical.markdown_strict_files/figure-markdown_strict/unnamed-chunk-4-1.png)

Looking at the plot you can confirm the obvious that FC Bayer is best in the leage by far. But also interesting is that ‘St. Pauli’ is quite good in defence (but bad in attack).

### Expected goals for each game \[optional\]

We now investigate the expected goals for the home and away team in each game. For that we calculate the quantities *θ*<sub>1</sub> and *θ*<sub>2</sub> from the posterior samples and add the home advantage. The expected number of goals is the average over all posterior draws. We also compare against the expected number of goals which have been provided by worldfootball.

``` r
As <- hfit %>% tidybayes::spread_draws(A[i, j]) %>% select(i, j, A)
homes =  hfit %>% tidybayes::spread_draws(home) %>% select('home') #posterior samples for homes

mean_goals_ht = mean_goals_at = rep(NA, length(ng)) 

for (g in 1:ng){
  #g = 1
  id1 = data[g, 'ht']
  id2 = data[g, 'at']
  
  att_1 <- As %>% filter(j == id1, i == 1) # posterior samples for att_1 (of ht) 
  att_2 <- As %>% filter(j == id2, i == 1) # 
  def_1 <- As %>% filter(j == id1, i == 2)
  def_2 <- As %>% filter(j == id2, i == 2)
  
  theta_1 <- exp(homes$home + att_1$A - def_2$A)
  theta_2 <- exp(att_2$A - def_1$A)
  
  mean_goals_ht[g] = mean(theta_1)
  mean_goals_at[g] = mean(theta_2)
}

data$mean_goals_ht = mean_goals_ht
data$mean_goals_at = mean_goals_at

plot(data$Home_xG, mean_goals_ht, asp=1, 
     main = 'Comparison of the expected number goals (our model vs worldfootball)')
abline(0,1)
```

![](football_hierachical.markdown_strict_files/figure-markdown_strict/unnamed-chunk-5-1.png)

``` r
plot(data$Away_xG, mean_goals_at, asp=1, 
     main = 'Comparison of the expected number goals (our model vs worldfootball)')
abline(0,1)
```

![](football_hierachical.markdown_strict_files/figure-markdown_strict/unnamed-chunk-5-2.png)

``` r
mean((data$mean_goals_ht - data$HomeGoals)^2)
```

    [1] 1.536581

``` r
mean((data$Home_xG - data$HomeGoals)^2) 
```

    [1] 1.192386

``` r
mean((data$mean_goals_at - data$AwayGoals)^2)
```

    [1] 1.312989

``` r
mean((data$Away_xG - data$AwayGoals)^2)
```

    [1] 1.111961

If you find that interesting, in https://oduerr.github.io/da/Euro24/euro24.html you will find a similar analysis for the Euro 24 including predictions of the goals.
