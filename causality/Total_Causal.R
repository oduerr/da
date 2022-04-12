n = 10000
alpha = 1
beta = 2
gamma = 3

M = rnorm(n)
N = alpha * M + rnorm(n)
K = rnorm(n, beta*N + gamma*M) 
mean(K) #Approx 0

M = rnorm(n) + 1
N = alpha * M + rnorm(n)
K = rnorm(n, beta*N + gamma*M) 
mean(K) #Approx 5

alpha*beta+gamma #Exact 5
#Estimating (leave all open)
lm(K ~ M) #Approx 5
