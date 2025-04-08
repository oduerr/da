N <- 1000 #Number of Samples
Z <- sample(c(0,1), size=N, replace = TRUE)
S <- ifelse(Z == 0, rnorm(N, 44, 3), rnorm(N, 40, 3))
Y <- ifelse(Z == 0, rnorm(N, 4000, 500), rnorm(N, 3500, 500))
plot(S, Y, col=Z+1)
