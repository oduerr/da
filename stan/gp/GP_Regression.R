require(rstan)
parallel::detectCores()
options(mc.cores = parallel::detectCores())
set.seed(42)
xs = seq(-5,5,length.out=100)

mu_true = function(xs) sin(xs) + 0.1 * xs  
x_train = c(-4,-2, 0, 1, 4, 4.2)
#x_train = seq(-5,5,length.out=20)
N = length(x_train)
y_train = rnorm(N, mean = mu_true(x_train), sd=0.2)
x_test = seq(-6,6, length.out = 200)
N_test = length(x_test)

fit_regression = stan(file = '~/Documents/workspace/da/stan/gp/GP_regression.stan', 
                  data=list(
                    N1=N, 
                    x1 = x_train,
                    y1 = y_train,
                    N2 = N_test,
                    x2 = x_test
                    ),
                  chains = 4,
                  iter = 10000)

fit_regression
post = extract(fit_regression)
means = apply(post$y2, 2, mean)
sds = apply(post$y2, 2, sd)

plot(x_train, y_train, ylim=c(-1.5,1.5), xlim=c(-6,6), col='blue')
#curve(mu_true, add=TRUE, col='red')
lines(x_test, means, col='blue')
lines(x_test, means + sds)
lines(x_test, means - sds)

