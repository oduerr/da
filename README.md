# da
Material for the data analytics course

website (under construction): https://oduerr.github.io/da/Stan_Primer_Full.html

# Installation for SomSem 2023 at HTWG
In you download folder you find a more detailed instruction `Einleitung CMDStan.pdf`
## Creation of the conda enviroment (takes time need to be done once)
Create a conda enviroment
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


