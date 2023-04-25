if (FALSE){
  install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
  cmdstanr::install_cmdstan()
  #In a terminal I needed to install rosetta
  # softwareupdate --install-rosetta
}
library(cmdstanr)
