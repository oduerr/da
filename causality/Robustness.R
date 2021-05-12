#### Simple ###

set.seed(42)

# X --> Y
# Training phase (DGP)
X = seq(5,6, length.out = 100) 
Y = 2 + 1 * X + 0.1*rnorm(100) 
df = data.frame(X=X, Y=Y)
plot(Y ~ X, xlim=c(0,8), ylim=c(5,9), df)
lm.prediction = lm(Y ~ X, df)
abline(lm.prediction)

# Generation of the novel data points (X shifted by 2)
X = X - 2 
Y = 2 + 1 * X + 0.1 *rnorm(100)
points(x=X, y=Y, col='green')
y_pred = predict(lm.prediction, data.frame(X=X))
points(x=X, y_pred, col='blue')
legend('topleft', c('training', 'observed', 'predicted'),col=c('black', 'green', 'blue'), pch=1)

# Y --> X
# Data Generating Process in the Training phase
Y = seq(7,8, length.out = 100) 
X = Y - 2 - 0.1*rnorm(100)
plot(Y ~ X, xlim=c(0,8), ylim=c(5,9), df)
# Training the model on the data 
lm.prediction = lm(Y ~ X, df)
abline(lm.prediction)

# Generation of the novel data point (Y causes)
Y = seq(7,8, length.out = 100)
X = Y - 2 - 0.1*rnorm(100) 
X = X - 2 #X is perturbed by -2
y_pred = predict(lm.prediction, data.frame(X=X))
points(x=X, y_pred, col='blue')
points(x=X, y=Y, col='green')
legend('topleft', c('training', 'observed', 'predicted'),col=c('black', 'green', 'blue'), pch=1)
