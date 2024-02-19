# da
Material for the data analytics course

website (under construction): https://oduerr.github.io/da/Stan_Primer_Full.html

# Installation for SomSem 2024 at HTWG
Please install [cmdrstan](https://mc-stan.org/cmdstanr/) on your laptop. 

```
docker run --rm -ti -e PASSWORD=yourpassword -p 8787:8787 ghcr.io/jbris/stan-cmdstanr-docker
```


If you don't manage to do so you can also install [rstan](https://mc-stan.org/users/interfaces/rstan). As a fallback 
you can use a Kaggle Notebooks, see [oduerr-rstan](https://www.kaggle.com/code/oduerr/oduerr-rstan) for an example of such a notebooks.


# OLD installation

## Installation for SomSem 2023 at HTWG
This is most likely not working anymore. 
In you download folder you find a more detailed instruction `Einleitung CMDStan.pdf`

## Creation of the conda enviroment (takes time need to be done once)
Create a conda enviroment. Windows-Key Start `CMD-Anaconda`  
```
conda create -n stan-env -c conda-forge cmdstan
```

## Activate conda enviroment (needs to be done for each new session)

```
conda activate stan-env
```

## Starting RStudio
Set path to conda enviroment 
```
library(cmdstanr)
#change oduerr
set_cmdstan_path("C:/Users/YOUR_USER_NAME/.conda/envs/stan-env/Library/bin/cmdstan") 
```

## Checking installation

Go to download folder open and execute `check_cmdstan_installation.R`


