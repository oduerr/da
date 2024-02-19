# da
Material for the data analytics course

website (under construction): https://oduerr.github.io/da/Stan_Primer_Full.html

# Installation for SomSem 2024 at HTWG

## Ideal Case: Installing cmdstanr on your Laptop
Please install [cmdrstan](https://mc-stan.org/cmdstanr/) on your laptop. 

## Docker installation for HTWG machines
If that is not working for you. You might use the docker installation. First, start the `Docker Desktop` application.
Then start the docker container in the command line using:
```
docker run --rm -ti -e PASSWORD=yourpassword -p 8787:8787 ghcr.io/jbris/stan-cmdstanr-docker
```
You can then access a running RStudio with instance cmdstanr via
[localhost:8787](http://localhost:8787) the username is `rstudio` and the password `yourpassword`.


## Fallback when not using HTWG machines (rstan instead of cmdstamr)
If you don't manage to install `cmdrstan` on you local machine. You can try to install [rstan](https://mc-stan.org/users/interfaces/rstan) on your local machine. Or you can  
you can use a Kaggle Notebooks as cloud based application. See [oduerr-rstan](https://www.kaggle.com/code/oduerr/oduerr-rstan) for an example of such a notebook.


## Checking installation
From https://github.com/oduerr/da/tree/master/stan/checking_installation upload the 'simple_lr.stan' file to the RStudio (server or local) and execute the file 'check_cmdstan_installation.R'


