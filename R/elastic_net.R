# credit_elasticnet_example.R

# Tell R where our code and data lives. 
setwd(getwd())

# set RNG seed for repeatability.
set.seed(2023)

# load our data.
Credit = read.csv("./Datasets/Credit.csv",header=TRUE)
head(Credit)

# show dimensions of our dataset.
dim(Credit)

# show variable summary statistics.
# Note: gender, student, married, ethnicity are categorical or 
#       qualitative variables.
summary(Credit)

# Show correlated variables.
# Note: limit and rating are highly correlated.
plot(Credit[,-(7:10)],pch=46,col="blue")

# Linear Regression recap.
# Here’s how to fit the linear model in R.

# Note how the categorical variables have been recorded as indicator 
# or dummy variables, taking 0 or 1.
# For the Credit dataset, n = 400, p = 11
X <- model.matrix(balance ~., Credit)[,-1]
y <- Credit$balance
head(X)

# Linear model and summary.
linear.mod <- lm(y ~ X)
summary(linear.mod)



# Training error vs. Test error

# The test error should be contrasted with the training error, 
# which is the MSE between X_train * β_hat and y_train.

# The training error can be made arbitrarily small by making the model 
# more complex; this is seldom what we want.

# Here is a worked example for the test error in R for the Credit dataset.

# This is the test error for the model with all features included; 
# we could compare it to test errors from models with fewer features.
# It would also be good to plot by y_hat against y_test.

set.seed(987654312, sample.kind = "Rounding")

# create randomized 50/50 train/test split
train <- sample(1:nrow(X), nrow(X)/2)
test <- -train

# build a linear model
linear.mod <- lm(y[train] ~ X[train,])

# make predictions
linear.pred <- coef(linear.mod)[1] + X[test,] %*% coef(linear.mod)[-1]
# calculate MSE
mean((linear.pred - y[test])^2)
# MSE = 10446.33 for linear model using lm.


# ElasticNet Regression.

# A convenient package for doing penalized regression in R is glmnet.

# grid is a decreasing sequence of values for the tuning parameter λ.
# alpha between 0 and 1 means do ElasticNet regression.

# glmnet does penalized regression for each value of the tuning parameter
# glmnet automatically standardizes X

# See also ISLR Section 6.6.
library(caret)
library(glmnet)

set.seed(2022)

# create a grid that will allow us to investigate different models 
# with different combinations of alpha and lambda. 
# This is done using the “expand.grid” function. 
grid <- expand.grid(.alpha  = seq(0, 1, by = 0.1),
                    .lambda = 10^seq(5, -2, length = 100))
head(grid, 11)

# we will use caret for model training...we want to do cross-validation.
control <- trainControl(method = "cv", number = 10)

# build our models on the training set.
enet.train <- train(x         = X[train,], 
                    y         = y[train], 
                    method    = "glmnet",
                    trControl = control,
                    tuneGrid  = grid)
enet.train
# best alpha
enet.train$bestTune$alpha
# best lambda
enet.train$bestTune$lambda

# Smallest MSE found using alpha = 0.9 and lambda = 0.5857021 - for seed(2022).

# now train a single model using the holdout method, with the alpha and lambda values found using cv.
enet <- glmnet(x      = X[train,], 
               y      = y[train],  
               alpha  = enet.train$bestTune$alpha,
               lambda = enet.train$bestTune$lambda)

# get coefficients for the 'best' model.
enet.coef <- coef(enet, exact=T)
enet.coef

# make predictions on the test set. 
enet.pred.y <- predict(enet, newx = X[test,])

# calculate residuals - predicted - actuals
enet.resid <- enet.pred.y - y[test]
# calculate test MSE (compare with OLS, Ridge, LASSO)  MSE=10438.26
mean(enet.resid^2)

# ----------------------------------------------------------------------------------------------
# Now we will try cv.glmnet using the alpha value found previously and with a vector of
# lambda values.
set.seed(2022)

# define a vector of lambda values to try.
.lambda <- 10^seq(5, -2, length = 100)
.lambda

# ElasticNet use 10-fold CV on the training set. This only supports the specification of one
# alpha value at a time. If you wanted to try multiple alpha values you could use a loop and
# run a cv.glmnet for each value of alpha you wanted to try.
enet.cv <- cv.glmnet(X[train,],
                     y[train],
                     alpha  = enet.train$bestTune$alpha,
                     lambda = .lambda,
                     nfolds = 10,
                     thresh = 1e-12)
# plot CV results.
plot(enet.cv)

# ----------------------------------------------------------------------------------------------

# value of lambda that gives the most regularised model such that
# the CV error is within one standard error of the minimum. lambda.1se = 9.326033.
enet.cv$lambda.1se

# show coefficients for this model using lambda of lambda.1se.
# Note: some coefficients have been reduced to zero.
coef(enet.cv, s = "lambda.1se")

# make predictions using the less complex model "lambda.1se"
enet.y.cv.1se <- predict(enet.cv,
                         newx = X[test,], 
                         s    = "lambda.1se")

# calculate residuals - predicted - actuals                 
enet.cv.resid.1se <- enet.y.cv.1se - y[test]
# calculate test MSE (compare with OLS, Ridge, LASSO, "lambda.min"),   MSE = 10203.91
mean(enet.cv.resid.1se ^ 2)

# ----------------------------------------------------------------------------------------------

# optimal lambda value ie. lambda value giving the smallest MSE.lambda.min = 2.983647.
enet.cv$lambda.min

# show coefficients for this model using lambda of lambda.min.
coef(enet.cv, s = "lambda.min")

# now make predictions using the model with the smaller MSE, but more complex model "lambda.min" 
enet.y.cv.min  <- predict(enet.cv,
                          newx = X[test,], 
                          s    = "lambda.min")

# calculate residuals - predicted - actuals                 
enet.cv.resid.min <- enet.y.cv.min - y[test]
# calculate test MSE (compare with OLS, Ridge, LASSO, "lambda.1se"),  MSE = 10118.85
# We see that this MSE is the smallest of all the methods we have tried thus far.
mean(enet.cv.resid.min ^ 2)


# Some notes:

# 1. We see that using lambda.min gives us the smallest MSE (10118.85) as we would expect, whereas
#    the less complex model (lambda.1se) results in a slightly larger MSE (10203.91).

# 2. The predict function above (line 180) takes as input a model object of type cv.glmnet and
#    as such will invoke a function called predict.cv.glmnet. See ?predict.cv.glmnet for supported
#    parameters. 


# end of credit_elasticnet_example.R