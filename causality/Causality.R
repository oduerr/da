# Lineare Regression für Correlation ####
N = 1e4
set.seed(42) 
a = rnorm(N)
b = 1.2 - 0.1 * a + rnorm(N,0, 0.1)
d = data.frame(a=scale(a), b=scale(b))
plot(a ~ b, d, pch='.')
coef(lm(a ~ b, d)) #b = -7.091218e-01
coef(lm(b ~ a, d)) # = -7.091218e-01
cor(a,b) #-0.7091218

a = scale(rnorm(N))
c = sample(c(-0.5,0.5), N, replace = TRUE)
b = scale(0.5*a + 3*c + rnorm(N))
plot(a,b, col=as.factor(c), pch='.')
cor(a,b)
lm(a ~ b)

cor(scale(a[c==0.5]),scale(b[c==0.5]))
cor(a[c==-0.5],b[c==-0.5])    
coef(lm(a ~ b + c ))
coef(lm(b ~ a + c ))

## Simulation (Kalisch Example) #####
set.seed(123)
n <- 1e4
z <- rnorm(n)
x <- 0.7*z + rnorm(n)
y <- 1*x + 0.5*z + rnorm(n)

coef(lm(y ~ x))
coef(lm(y ~ z + x))
confint(lm(y ~ z + x))

y <- 0.5*z + rnorm(n) #No dependence of x
confint(lm(y ~ x))
confint(lm(y ~ z + x))


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

#First model with causal link from M to D
As = rnorm(N)
Ms = 2 * As + rnorm(N)    #We consider additive models
Ds = -3 * As + 4*Ms + rnorm(N)   
ds.5.1 = data.frame(A = As, M = Ms, D = Ds)

coef(lm(D ~ A, ds.5.1)) # ~ -5 = 8 - 3 
coef(lm(D ~ M, ds.5.1)) # ~ -2.8 Spurious
coef(lm(D ~ A + M ,ds.5.1)) # A approx -3, M approx 4
coef(lm(D ~ M + A ,ds.5.1)) # Same as above



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
confint(lm.fit) #N -0.398 0.7468
abline(lm.fit)

d$M = scale(log(d$mass))
lm.fit = lm(K ~ N + M, d)
confint(lm.fit)
#N 0.033  1.261
#M 1.454 -0.1639


## Milk and Brain Simulation
N = 10000
set.seed(42)
U = rnorm(N)
M = rnorm(N, U) 
N = rnorm(N, U)
K = rnorm(N, -1.2*N + 2*M)
d_sim3 = data.frame(K,M,N)

coef(lm(K ~ N, d_sim3))

coef(lm(K ~ M, d_sim3))

coef(lm(K ~ M + N, d_sim3))


### Co linearity ######
N <- 100
height <- rnorm(N,180,20) 
leg_prop <- runif(N,0.4,0.5)
leg_left <- leg_prop*height + rnorm( N , 0 , 0.02 )
leg_right <- leg_prop*height + rnorm( N , 0 , 0.02 )
d <- data.frame(height,leg_left,leg_right)
confint(lm(height ~ leg_left + leg_right)) #Hughe Conf Intervalls
confint(lm(height ~ leg_left ))  #tigher Conf Intervalls


### Colider Bias Simulated Data (Berkeley)######
N = 100
set.seed(42)
S = rnorm(N) #Sports Score
I = rnorm(N) #Inteligence
A = ifelse(S > 1 | I > 1, TRUE, FALSE) #Admitted to Berkley
sum(A)
plot(S,I,col=ifelse(A > 0, 'green', 'red'),xlim=c(-3,3), ylim=c(-3,3), 
     xlab='Sport Score', ylab='IQ Score',
     main='Admitted to Berkey')
abline(lm(S[A] ~ I[A]), col='green')
abline(lm(S[!A] ~ I[!A]), col='red')
abline(lm(S ~ I), col='blue')
legend('topright',legend=c("Admitted", "Not Admitted", "All"),
       col=c("green", "red", "blue"), lty=1:2, cex=0.8)

plot(S[A],I[A], 
     xlim=c(-3,3), ylim=c(-3,3), 
     xlab='Sport Score', ylab='IQ Score',
     main='Admitted to Berkeley'
     )
abline(lm(S[A] ~ I[A]))

confint(lm(I ~ A + S))
confint(lm(S ~ I))

### Collider Bias Simulated ######
N = 1e5
X = rnorm(N)
Z = rnorm(N)
Y = 2*X + 1*Z + rnorm(N)
coef(lm(Z ~ X)) #X approx. 0

coef(lm(Z ~ X + Y)) #X approx. -1

# Plot
library(dagitty)
dag <- dagitty( "dag{ X -> Y; Z -> Y}" )
coordinates(dag) <- list( x=c(X=0,Y=1,Z=2) , y=c(X=0,Y=1,Z=0) ) #Nice drawing
drawdag( dag )

#### Path vs DAG #####
# Plot
library(dagitty)
dag <- dagitty( "dag{ X -> A; A ->E ;A -> B; A -> C; C -> E; E -> Y}" )
#coordinates(dag) <- list( x=c(X=0,Y=1,Z=2) , y=c(X=0,Y=1,Z=0) ) #Nice drawing
par(cex = 1.5)
drawdag( dag )


### sim happiness ######
d <- sim_happiness( seed=1977 , N_years=1000 )
precis(d)
#The problem here is the coding, which does not work
lm(happiness ~ married + age, d)


##### Backdoor Adjustment Set (I) ######
library(dagitty)
dag_6.1 <- dagitty( "dag {
    U [unobserved]
    X -> Y
    X <- U <- A -> C -> Y
    U -> B <- C
}")
adjustmentSets( dag_6.1 , exposure="X" , outcome="Y" )

##### Backdoor Adjustment Set (II) ######
library(dagitty)
dag <- dagitty( "dag {
    X -> Y
    X -> Z -> Y
}")
dagitty::adjustmentSets( dag , exposure="X" , outcome="Y" )
adjustmentSets( dag , exposure="X" , outcome="Y" ,effect="direct")
drawdag( dag )

##### Instrumental Variable ######
set.seed(73)
N <- 500
U_sim <- rnorm( N )
Q_sim <- sample( c(1:4) , size=N , replace=TRUE )
E_sim <- rnorm( N , U_sim + Q_sim )
W_sim <- rnorm( N , U_sim + 1.42*E_sim ) #Change 0 to 1.42
dat_sim <- data.frame(
        Y=W_sim,#standardize(W_sim) ,
        X=E_sim,#standardize(E_sim) ,
        A=Q_sim)#standardize(Q_sim) )

plot(Y ~ X, col=as.factor(dat_sim$A), dat_sim)

#a) The wrong coefficient
confint(lm(Y ~ X, dat_sim))

#b) Estimate the coeficient a (via two stage least square) 
alpha = coef(lm(X ~ A, dat_sim))['A']
dat_sim$Xh = alpha * dat_sim$A
coef(lm(Y ~ Xh, dat_sim)) #-0.23, 0.23

#b) Estimate the coeficient a (via projector) 
A = dat_sim$A
P.A <- A%*%solve(t(A)%*%A)%*%t(A)
X_tilde = dat_sim$X %*% P.A
Y_tilde = dat_sim$Y %*% P.A
d = data.frame(y = as.numeric(Y_tilde), x = as.numeric(X_tilde))
coef(lm(y ~ x + 0,d))

#b) Estimate the coeficient a (via projector II) 
l1 = lm(X ~ A, dat_sim)
X_tilde = predict(l1, dat_sim)

l2 = lm(Y ~ A, dat_sim)
Y_tilde = predict(l2, dat_sim)

coef(lm(Y_tilde ~ X_tilde + 0,d))




library(dagitty) 
dagIV <- dagitty( "dag{ A -> X <- H -> Y <- X }" ) 
instrumentalVariables( dagIV , exposure="X" , outcome="Y") 


##### Lemma for the formal proof of IV #####
# The regression coefficient can be written as cov(X,Y) / var(X)
lm(iris$Sepal.Length ~ iris$Petal.Length) #0.4089
cov(iris$Sepal.Length , iris$Petal.Length) / var(iris$Petal.Length)#0.4089







