# da
Material for the data analytics course

Moodle Course Site: https://moodle.htwg-konstanz.de/moodle/course/view.php?id=1553 

Website (under construction): https://oduerr.github.io/da/Stan_Primer_Full.html

# Installation for SomSem 2025 at HTWG

## Ideal Case: Installing cmdstanr on your Laptop
Please install [cmdrstan](https://mc-stan.org/cmdstanr/) on your laptop. 

## Using HTWG machines
You can install the cmdstanr with
```
install.packages("cmdstanr", repos = c('https://stan-dev.r-universe.dev', getOption("repos")))
library(cmdstanr)
check_cmdstan_toolchain(fix=TRUE) #fix=TRUE important for PATH
install_cmdstan(cores = 8,  overwrite=TRUE)
```


## Fallback when not using HTWG machines (rstan instead of cmdstamr)
If you don't manage to install `cmdrstan` on you local machine. You can try to install [rstan](https://mc-stan.org/users/interfaces/rstan) on your local machine. Or you can  
you can use a Kaggle Notebooks as cloud based application. See [oduerr-rstan](https://www.kaggle.com/code/oduerr/oduerr-rstan) for an example of such a notebook.


## Checking installation
From https://github.com/oduerr/da/tree/master/stan/checking_installation upload the 'simple_lr.stan' file to the RStudio (server or local) and execute the file 'check_cmdstan_installation.R'


