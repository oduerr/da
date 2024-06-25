#### Refactoring of eval_performance.R but with cmdstanr and only NLL
# Calculated performance of foorball prediction models
# Generated quantities block is used to calculate the log likelihood
# The log likelihood is used to calculate the LOO-CV

library(cmdstanr)
library(tidybayes)
library(dplyr)
library(magrittr)
#options(mc.cores = parallel::detectCores()) #
options(mc.cores = 2)


set.seed(1) #set seed 

#### Data ####
## From the blogpost
# data_old = read.csv('https://raw.githubusercontent.com/MaggieLieu/STAN_tutorials/master/Hierarchical/premiereleague.csv',col.names = c('Home','score1', 'score2', 'Away'), stringsAsFactors = FALSE)

if (FALSE){
  # Downloaded the data from https://www.football-data.co.uk/
  data_complete = read.csv('~/Documents/GitHub/da/website/Euro24/premierleague2019.csv', stringsAsFactors = FALSE)
  #https://www.football-data.co.uk/germanym.php
  data_complete = read.csv('~/Documents/GitHub/da/website/Euro24/bundesliga2000.csv', stringsAsFactors = FALSE) 
  data_complete = read.csv('~/Documents/GitHub/da/website/Euro24/bundesliga2000.csv', stringsAsFactors = FALSE) 
  b365 = data_complete %>% select(home_odds = WHH, draw_odds = WHD, away_odds = WHA)
}
data_complete = read.csv('~/Documents/GitHub/da/website/Euro24/bundesliga2023.csv', stringsAsFactors = FALSE) 
b365 = data_complete %>% select(home_odds = B365H, draw_odds = B365D, away_odds = B365A)
data <- data_complete %>%
    select(Home = HomeTeam, score1 = FTHG, score2 = FTAG, Away = AwayTeam)


rowSums(1/b365) #Is > 1 approx 1.05 the bookie margin

ng = nrow(data)
teams = unique(data$Home)
ht = unlist(sapply(1:ng, function(g) which(teams == data$Home[g])))
at = unlist(sapply(1:ng, function(g) which(teams == data$Away[g])))

ntrains = 10:320
ntrains = c(10,20,30,50,100, 200, 250, 300, 320, 360)
ntrains = c(10,100,300)
ntrains = seq(5,nrow(data_complete)-5, 5)

cat('we have G =', ng, 'games \n')
nt = length(unique(data$Home))
cat('We have T = ', nt, 'teams \n')


#### getting data for STAN ########
# we will save the last np games to predict
get_data = function(np){
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
    atnew = at[(ngob+1):ng],
    s1new = data$score1[(ngob+1):ng],
    s2new = data$score2[(ngob+1):ng],
    
    score1 = data$score1, #Have all in one place
    score2 = data$score2
  )
  return (my_data)
}

# Function to extract the name
extract_name <- function(path) {
  file_name <- basename(path)  # Extract the file name with extension
  dir_path <- dirname(path)  # Extract the directory path
  
  # Extract the relevant part of the directory path
  # This assumes the structure "da/website/Euro24"
  relevant_part <- sub("^.*GitHub/", "", dir_path)
  
  # Combine the relevant part of the directory path with the file name without extension
  combined_name <- file.path(relevant_part, sub("\\.stan$", "", file_name))
  
  return(combined_name)
}

###### Main Loop ##########
models = list()
if (FALSE){
}
models = append(models,cmdstan_model('~/Documents/GitHub/da/website/Euro24/hier_model_cor_nocholsky.stan'))
models = append(models,cmdstan_model('~/Documents/GitHub/da/website/Euro24/hier_model.stan'))
models = append(models,cmdstan_model('~/Documents/GitHub/da/stan/football/non_hier_model.stan'))
models = append(models,cmdstan_model('~/Documents/GitHub/da/stan/football/hier_model_nb.stan'))
models = append(models,cmdstan_model('~/Documents/GitHub/da/website/Euro24/hier_model_cor.stan'))
models = append(models,cmdstan_model('~/Documents/GitHub/da/website/Euro24/hier_model_cor_home.stan'))
         
calc_prob <- function(observed, theta) {
  mean(dpois(observed, theta, log = FALSE))
  #exp(mean(dpois(observed, theta, log = TRUE)))
}

calc_rps <- function(prob_wdl, outcome){
  # Initialize the cumulative sums
  cdf_f = cumsum(prob_wdl)
  cdf_y = c(0, 0, 0)
  
  # Set the observed outcome
  if (outcome[1] > outcome[2]){
    cdf_y = c(1, 1, 1)  # Home win
  } else if (outcome[1] == outcome[2]){
    cdf_y = c(0, 1, 1)  # Draw
  } else {
    cdf_y = c(0, 0, 1)  # Away win
  }
  
  # Calculate RPS
  rps = 0.5 * sum((cdf_f - cdf_y)^2)
  return(rps)
}

if (FALSE){
  #Example from Forecasting football match results in national league competitions using score-driven time series models
  outcome = c(3, 0)
  prob_wdl = c(0.5, 0.4, 0.1)
  calc_rps(prob_wdl, outcome) #  Should be 0.13
  
  prob_wdl = c(0.5, 0.3, 0.2) # 
  calc_rps(prob_wdl, outcome) # Should be 0.145 
}

       
calc_scores <- function(theta1new, theta2new, standat, ntrain, odds = NULL){
  # Calculate the scores
  nll = nll_res = nll_bookie= 0 
  bet_return = 0
  rps_booki = rps_total = 0
  min_sum_prob = 10
  ahead = min(18, standat$np)
  for (j in 1:ahead){
    # j = 1 don;t use i in the loop
    # Calculate probabilities of goals, wins, draws and losses
    theta1 = theta1new %>% filter(i == j) %>% select(i, theta1new) %>% pull()
    theta2 = theta2new %>% filter(i == j) %>% select(i, theta2new) %>% pull()
    prob_g1= apply(matrix(0:10, ncol=1), 1, function(x) calc_prob(x, theta1)) 
    prob_g2= apply(matrix(0:10, ncol=1), 1, function(x) calc_prob(x, theta2)) 
    prob_goals = outer(prob_g1, prob_g2, '*') %>% as.matrix 
    nll = nll + log(prob_g1[standat$s1new[j]+1]) + log(prob_g2[standat$s2new[j]+1])
    #log(prob_goals[data$score1[j]+1, data$score2[j]+1])
    
    # Calculate the probabilities of wins, draws and losses
    prob_win = sum(prob_goals[lower.tri(prob_goals, diag = FALSE)])
    prob_draw = sum(diag(prob_goals))
    prob_lose = sum(prob_goals[upper.tri(prob_goals, diag = FALSE)])
    sum_prob = prob_draw + prob_win + prob_lose
    if (sum_prob < 1 - 1e-3){
      cat('Error: probabilities do not sum to 1 but are', prob_draw + prob_win + prob_lose, '\n')
    }
    if (sum_prob < min_sum_prob){
      min_sum_prob = sum_prob
    }
    
    if (standat$s1new[j] > standat$s2new[j]){
      nll_res = nll_res + log(prob_win)
    } else if (standat$s1new[j] == standat$s2new[j]){
      nll_res = nll_res + log(prob_draw)
    } else {
      nll_res = nll_res + log(prob_lose)
    }
    
    outcome = c(standat$s1new[j], standat$s2new[j])
    rps_total = rps_total + calc_rps(prob_wdl=c(prob_win, prob_draw, prob_lose), outcome)
    
    # Calculate the log likelihood using the odds
    if (!is.null(odds)){
      o = odds[ntrain + j,]
      p365 = 1/o / rowSums(1/o) #probabilities
      
      
      nll_bookie = nll_bookie - 
        log(p365[1]) * (standat$s1new[j] > standat$s2new[j]) - 
        log(p365[2]) * (standat$s1new[j] == standat$s2new[j]) - 
        log(p365[3]) * (standat$s1new[j] < standat$s2new[j])
    
      rps_booki = rps_booki + calc_rps(prob_wdl=c(p365[1], p365[2], p365[3]), outcome)
      
      
      # Bet if the we expect to be better than the bookie without the margin
      if (prob_win > p365[1]) {
        bet_return <- bet_return + (o[1] * (standat$s1new[j] > standat$s2new[j]) - 1)
      }
      if (prob_draw > p365[2]) {
        bet_return <- bet_return + (o[2] * (standat$s1new[j] == standat$s2new[j]) - 1)
      }
      if (prob_lose > p365[3]) {
        bet_return <- bet_return + (o[3] * (standat$s1new[j] < standat$s2new[j]) - 1)
      }
      
      
    
    }
      
  } #Ahead loop
  return (data.frame(nll = -nll/ahead, nll_res = -nll_res/ahead, nll_bookie = as.numeric(nll_bookie/ahead), 
                     bet_return=as.numeric(bet_return/ahead), min_sum_prob = min_sum_prob, 
                     rps = rps_total/ahead, rps_booki = rps_booki/ahead))
}             


# Initialize progress bar
library(progress)
pb <- progress_bar$new(
  format = "  Training [:bar] :percent eta: :eta",
  total = length(ntrains) * length(models),
  width = 60
)

res.df = NULL
for (ntrain in ntrains){
  #ntrain = 100  
  np = ng - ntrain
  d = get_data(np)
  ngob = ng-np
  cat('---------------------------------------------------\n')
  cat(paste0('Number of trainingsdata ', ntrain))
  cat('---------------------------------------------------\n')
  for (m in models){
    try({
    #m = models[1]
    #m = cmdstan_model('~/Documents/GitHub/da/stan/football/hier_model_nb.stan')
    #m = cmdstan_model('~/Documents/GitHub/da/website/Euro24/hier_model_cor_home.stan')
    name = extract_name(m$stan_file())
    cat('Model ', name, '\n')
    
    s <- m$sample(data = d)
    
    # Adding Diagnostics
    diagnostics <- s$diagnostic_summary()
    res.df = rbind(res.df, data.frame(ntrain=ntrain, res=mean(diagnostics$num_divergent), type = 'num_divergent', name =  name))
    res.df = rbind(res.df, data.frame(ntrain=ntrain, res=mean(diagnostics$ebfmi), type = 'ebfmi', name =  name))
    res.df = rbind(res.df, data.frame(ntrain=ntrain, res=mean(diagnostics$num_max_treedepth), type = 'num_max_treedepth', name =  name))
    
    #s %>% tidybayes::spread_draws(A[i, j])
    theta1new <- s %>% tidybayes::spread_draws(theta1new[i]) %>% select(i,theta1new)
    theta2new <- s %>% tidybayes::spread_draws(theta2new[i]) %>% select(i,theta2new)
    scores = calc_scores(theta1new=theta1new, theta2new=theta2new, standat = d, ntrain, b365) #3.306292 1.295251
    
    res.df = rbind(res.df, data.frame(ntrain=ntrain, res=scores$min_sum_prob, type = 'MIN_SUM_PROB', name =  name))
    res.df = rbind(res.df, data.frame(ntrain=ntrain, res=scores$nll, type = 'NLL_PRED', name =  name))
    res.df = rbind(res.df, data.frame(ntrain=ntrain, res=scores$nll_res, type = 'NLL_RESULTS', name =  name))
    res.df = rbind(res.df, data.frame(ntrain=ntrain, res=scores$nll_bookie, type = 'NLL_BOOKIE', name =  name))
    res.df = rbind(res.df, data.frame(ntrain=ntrain, res=scores$bet_return, type = 'BET_RETURN', name =  name))
    res.df = rbind(res.df, data.frame(ntrain=ntrain, res=scores$rps, type = 'RPS', name =  name))
    res.df = rbind(res.df, data.frame(ntrain=ntrain, res=scores$rps_booki, type = 'rps_booki', name =  name))
    
    # Calculate the log likelihood of the prediction
    lp = -spread_draws(s, log_lik_pred)
    res.df = rbind(res.df, data.frame(ntrain=ntrain, res=mean(lp$log_lik_pred), type = 'NLL_PRED_STAN', name =  name))
    
    # Calculate the log likelihood of the training data using PSIS-LOO
    log_lik = s$draws('log_lik') #extract the log likelihood of the training data
    res = loo::loo(log_lik)
    NLL = -res$estimates['elpd_loo', 1] / dim(log_lik)[3]
    res.df = rbind(res.df, data.frame(ntrain=ntrain, res=NLL, type = 'NLL_PSIS', name =  name))
    
    # Update progress bar
    pb$tick()
    })
  }
  write.csv(res.df, '~/Documents/GitHub/da/website/Euro24/eval_performance_bundesliga_23_18Ahead.csv', row.names = FALSE)
}

# Writing the results to a file
cat('done')




























  
  
  
  