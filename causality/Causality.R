# Lineare Regression für Correlation ####
N = 1e4
set.seed(42)
a = rnorm(N)
b = 1.2 - 0.1 * a + rnorm(N,0, 0.1)
d = data.frame(a=scale(a), b=scale(b))
plot(a ~ b, d, pch='.')
coef(lm(a ~ b, d)) #b = -7.091218e-01
coef(lm(b ~ a, d)) #a = -7.091218e-01
cor(a,b) #-0.7091218


## Simulation #####
N = 50000 #The number of states (increased to get tigher confintervals)

#First model w/o causal link from M to D
As = rnorm(N)
Ms = 2 * As + rnorm(N)    #We consider additive models
Ds = -3 * As + rnorm(N)   
ds.5.2 = data.frame(A = As, M = Ms, D = Ds)

coef(lm(D ~ A, ds.5.2)) # ~ -3  
coef(lm(D ~ M, ds.5.2)) # ~ -1.2 Spurious
coef(lm(D ~ A + M ,ds.5.2)) # A approx -3, M approx 0
coef(lm(D ~ M + A ,ds.5.2)) # Same as above



# Conditional independance
library(rethinking) #Because of the dataset
data(WaffleDivorce)
d = WaffleDivorce
par(mfcol=c(1,2))
plot(Divorce ~ Marriage, d)
abline(lm(Divorce ~ Marriage, d))
plot(Divorce ~ MedianAgeMarriage, d)
abline(lm(Divorce ~ MedianAgeMarriage, d))
par(mfcol=c(1,1))

d$D = scale(d$Divorce) #Standardize data
d$M = scale(d$Marriage)
d$A = scale(d$MedianAgeMarriage)
confint(lm(D ~ A, d)) #A -0.83, -0.36 Significant different from 0
confint(lm(D ~ M, d)) #M is significant different from 0
confint(lm(D ~ A + M, d)) 
#Obs 1 A significant different from 0 
#Obs 2 M not significant different from 0
#Obs 3 A not significant different from simple Model D ~ A
#This translates knowing M does not have any extra information when A is known.
#I.e. (D \coprod M) | A

###############
# Plotting
library(dagitty)
dag5.1 <- dagitty( "dag{ A -> D; A -> M; M -> D }" )
coordinates(dag5.1) <- list( x=c(A=0,D=1,M=2) , y=c(A=1,D=0,M=1) ) #Nice drawing
drawdag( dag5.1 )
library(ggdag)
dd = tidy_dagitty(dag5.1)
ggdag(dd) + geom_dag_edges_link(edge_colour='red') 

dag5.2 <- dagitty( "dag{ A -> D; A -> M }" )
coordinates(dag5.2) <- list( x=c(A=0,D=1,M=2) , y=c(A=0,D=1,M=0) ) #Nice drawing
drawdag( dag5.2)

################
# Equivalent DAGS
# The data seems to prefer the dag5.2, D is cond inpedendend on M given A
eq = equivalentDAGs(dag5.2)
drawdag(eq)


#Causal Link from M to D
As = rnorm(N)
Ms = 2 * As + rnorm(N)    #We consider additive models
Ds = -3 * As + 4* Ms + rnorm(N)
ds.5.1 = data.frame(A = As, M = Ms, D = Ds)
confint(lm(D ~ A, ds.5.1)) # ~  5 = 8 - 3 Stronger than direct it also influences M
confint(lm(D ~ M, ds.5.1)) # ~  2.8 not the direct effect
confint(lm(D ~ A + M ,ds.5.1)) # A approx -3, M approx 4


### Milk and Brain ######
library(rethinking)
data(milk)
d <- milk
d$N = scale(d$neocortex.perc)
d$K = scale(log(d$kcal.per.g))
plot(K ~ N, d)
lm.fit = lm(K ~ N, d)
confint(lm.fit)
abline(lm.fit)


### Co linearity ######
N <- 100
height <- rnorm(N,180,20) 
leg_prop <- runif(N,0.4,0.5)
leg_left <- leg_prop*height + rnorm( N , 0 , 0.02 )
leg_right <- leg_prop*height + rnorm( N , 0 , 0.02 )
d <- data.frame(height,leg_left,leg_right)
confint(lm(height ~ leg_left + leg_right)) #Hughe Conf Intervalls
confint(lm(height ~ leg_left ))  #tigher Conf Intervalls


### Colider Bias ######
d <- sim_happiness( seed=1977 , N_years=1000 )
precis(d)
#The problem here is the coding, which does not work
lm(happiness ~ married + age, d)









