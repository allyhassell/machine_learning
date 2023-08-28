# Ridge regression and LASSO

# set RNG seed for repeatability.
set.seed(987654321)

# load our data.
Credit = read.csv("./Datasets/Credit.csv",header=TRUE)
head(Credit)

# show dimensions of our dataset.
dim(Credit)

summary(Credit)

plot(Credit[,-(7:10)],pch=46,col="blue")

# Linear Regression recap.
X <- model.matrix(balance ~ ., Credit)[,-1]
y <- Credit$balance
head(X)

# Linear model and summary.
linear.mod = lm(y ~ X)
summary(linear.mod)

# Training error vs. Test error
set.seed(987654312, sample.kind = "Rounding")
train <- sample(1:nrow(X), nrow(X)/2)
test <- -train
linear.mod <- lm(y[train] ~ X[train,])
linear.pred <- coef(linear.mod)[1] + X[test,] %*% coef(linear.mod)[-1]
head(linear.pred)
mean((linear.pred - y[test])^2)

####### Ridge Regression
library(glmnet)

# Lambda
grid <- 10^seq(5, -2, length=100)
grid

# BUild model and test error 
ridge.mod <- glmnet(X[train,], 
                    y[train], 
                    alpha=0, 
                    lambda=grid, 
                    thresh=1e-12)
ridge.pred <- predict(ridge.mod, s=0.01, newx=X[test,])
mean((ridge.pred - y[test])^2)

# LASSO
lasso.mod <- glmnet(X[train,],
                    y[train],
                    alpha=1,
                    lambda=grid,
                    thresh=1e-12)
lasso.pred <- predict(lasso.mod, s=0.01, newx=X[test,])
mean((lasso.pred - y[test])^2)

# Cross-validation for ridge regression
set.seed(987654313, sample.kind = "Rounding")
ridge.cv.out <- cv.glmnet(X[train,],
                          y[train],
                          alpha=0,
                          lambda=grid,
                          nfolds=10,
                          thresh=1e-12)
ridge.cv.out$lambda.min
plot(ridge.cv.out)

# Cross-validation for ridge regression

# We can now compute the test error:
bestlam <- ridge.cv.out$lambda.min
ridge.pred <- predict(ridge.cv.out, s=bestlam, newx=X[test,])
mean((ridge.pred - y[test])^2)
# [1] 10323.65

# Finally, we refit the ridge regression model on the full dataset 
# using λ min.
#out = glmnet(X, y, alpha=0, lambda=grid, thresh=1e-12)
#predict(out, type="coefficients", s=bestlam)[1:12,]


# Cross-validation for the LASSO.

# Cross-validation proceeds in exactly the same way for the lasso.
set.seed(987654313, sample.kind = "Rounding")
lasso.cv.out <- cv.glmnet(X[train,],
                          y[train],
                          alpha=1,
                          lambda=grid,
                          nfolds=10,
                          thresh=1e-12)
lasso.cv.out$lambda.min
# [1] 2.535364
plot(lasso.cv.out)

# Cross-validation for the LASSO.

# Compute the test error:

# Reminder: OLS   MSE on Test set was 10446.33
# Reminder: Ridge MSE on Test set was 10438.68 for s=0.01
# Reminder: Ridge MSE on Test set was 10126.62 for s=7
# Reminder: LASSO MSE on Test set was 10445.01 for s=0.01
# Reminder: LASSO MSE on Test set was 10199.61 for s=5
# Reminder: Ridge MSE on Test set was 10204.5  using CV
#           LASSO MSE on Test set was 10258.06 using CV 

# Compute the test error:
bestlam <- lasso.cv.out$lambda.min
lasso.pred <- predict(lasso.cv.out, s=bestlam, newx=X[test,])
mean((lasso.pred - y[test])^2)
# [1] 10258.06

# coefficients of best lasso model found using cv.
coef(lasso.cv.out) 

# Finally, we refit on the full dataset using λ min.
out = glmnet(X, y, alpha=1, lambda=grid, thresh=1e-12)
# small lambda, give the smallest MSE.
predict(out, type="coefficients", s=bestlam) #[1:12,]
# try a slightly larger lambda (=5), some coefficients reduced to zero. 
predict(out, type="coefficients", s=5) #[1:12,]

# Comparison of model predictions.
plot(y[test], linear.pred, ylim=c(-400,1700), xlab="y_test", ylab="predicted")
points(y[test], ridge.pred, col="blue")
points(y[test], lasso.pred, col="orange")
abline(0,1)

# Comments

# For the Credit dataset

# The lasso regression model has the smallest test error, but only a 
# slight improvement over the linear model.
# The large number of zero credit card balances probably affects the predictions and might need to be modelled separately.

# More generally:

# Penalized methods typically improve over ordinary linear regression 
# by trading off a small increase in bias for a large decrease in variance.

# Ridge regression will tend to perform better when there are a 
# large number of informative features; 

# lasso does better when there are only a few.

# Penalized methods can also work when p > n. 
# Feature selection via the lasso is particularly useful in this case.

# When the plot of the cross-validation error is very flat near its 
# minimum, λ_min may vary a lot between different choices of the folds. 
# In this case, it might be worth averaging over multiple 
# cross-validation scenarios.





