#### Extracting football data for Bayesian Modelling

#### Installing world of football R package ####
if (FALSE){
  remotes::install_github("JaseZiv/worldfootballR")   # dev version is recommended
}
library(worldfootballR)
library(dplyr)  

### Getting the data from world of football
df_raw <- fb_match_results(
  country         = "GER",   # three-letter ISO code
  gender          = "M",     # men's competition
  season_end_year = 2025,    # last calendar year of the season
  tier            = "1st"    # Bundesliga is Germany’s 1st tier
)


### Creating the data for the Bayesian Analysis
## Discarding relagation rounds
df = df_raw %>% filter(Round == 'Bundesliga') 
  
teams = df$Home %>%
  unique() %>%
  sort()
 
nt = length(teams) #Number of teams
ng = nrow(df) # Number of games

## Saniti Check number of games should be nt * (nt-1)
ng == nt*(nt-1)

ht = unlist(sapply(1:ng, function(g) which(teams == df$Home[g])))
at = unlist(sapply(1:ng, function(g) which(teams == df$Away[g])))

df$ht = ht
df$at = at

### Save the result as a csv-file
write.csv(df, file = "stan/football/bundesliga_2025.csv", row.names = FALSE) 




