# Machine learning models 

setwd(getwd())
set.seed(123)

library(MASS)
library(boot)
library(glmnet)
library(ggplot2)

# Load data 
library(readr)
load("Residen.RData")
res_data <- Residen

# Test train split
train_index <- sample(nrow(res_data), 0.7 * nrow(res_data))
train_data <- res_data[train_index, ]
test_data <- res_data[-train_index, ]

## a) Correlation between variables
library(corrgram)
corrgram(res_data, cor.method = 'pearson', abs=TRUE)
corrgram(res_data[,1:50], cor.method = 'pearson', abs=TRUE)
corrgram(res_data[,50:length(names(res_data))], cor.method = 'pearson', abs=TRUE)

## b) Linear regression model
res_lm <- lm(V104 ~ . - V105,
             data=train_data)

# Coefficients
summary(res_lm)$coefficients[summary(res_lm)$coefficients[,4] < 0.05, ]

# R squared
summary(res_lm)$r.squared

# residuals
summary(res_lm)$sigma

## c) Compare backwards and stepwise models
### i) Build backward selection model

### Backwards selection
start_time1 <- Sys.time()
res_back <- stepAIC(res_lm,
                    direction ="backward",
                    trace = FALSE)
end_time1 <- Sys.time()

### ii) Build step-wise selection model

### Stepwise selection
start_time2 <- Sys.time()
res_step <- stepAIC(res_lm,
                    direction="both",
                    trace = FALSE)
end_time2 <- Sys.time()


### iii) Compare model performances
### Outputs

# Coefficients backwards
summary(res_back)$coefficients[summary(res_back)$coefficients[,4] < 0.05, ]
# R squared backwards
summary(res_back)$r.squared

# Coefficients step-wise
summary(res_step)$coefficients[summary(res_step)$coefficients[,4] < 0.05, ]
# R squared step-wise
summary(res_step)$r.squared

# Residuals backwards
summary(res_back)$sigma

# Residuals step-wise
summary(res_step)$sigma

# Computation time backward
comp_back1 <- end_time1 - start_time1
print(comp_back1)

# Computation time stepwise
comp_step2 <- end_time2 - start_time2
print(comp_step2)

### Holdout mean squared error 

# Test MSE backward
predicted_back <- predict(res_back, newdata = test_data)
mse_back <- mean((test_data$V104 - predicted_back)^2)
print(mse_back)

# Test MSE step-wise
predicted_step <- predict(res_step, newdata = test_data)
mse_step <- mean((test_data$V104 - predicted_step)^2)
print(mse_step)

### Cross-validation mean squared error
## d) Compare ridge and LASSO regression

# Data in matrix
X <- model.matrix(V104 ~ ., res_data)
y <- res_data$V104

### i) Ridge model
set.seed(54)
grid_ridge <- 10^seq(3, -10, length=100)
start_time3 <- Sys.time()
suppressWarnings({
  ridge_model <- cv.glmnet(X[train_index,],
                         y[train_index],
                         alpha=0,
                         lambda=grid_ridge,
                         nfolds=10,
                         thresh=1e-12)
})
end_time3 <- Sys.time()

best_lambda <- ridge_model$lambda.min
best_lambda

plot(ridge_model)
abline(v=log(best_lambda), col='pink')

ridge_pred <- predict(ridge_model, s=best_lambda, newx=X[-train_index,])

start_time4 <- Sys.time()
ridge_model_out = glmnet(X,
                         y,
                         alpha=0,
                         lambda=grid_ridge,
                         thresh=1e-12)
end_time4 <- Sys.time()

predict(ridge_model_out, type="coefficients", s=best_lambda)

### ii) LASSO model
set.seed(65)
grid_lasso <- 10^seq(3, -10, length=100)
start_time5 <- Sys.time()
lasso_model <- cv.glmnet(X[train_index,], 
                         y[train_index], 
                         alpha=1, lambda=grid_lasso, 
                         nfolds=10, 
                         thresh=1e-12)
end_time5 <- Sys.time()

# Lambda
best_lambda <- lasso_model$lambda.min
best_lambda

plot(lasso_model)
abline(v=log(best_lambda), col='pink')

# Predictions
lasso_pred <- predict(lasso_model, s=best_lambda, newx=X[-train_index,])

start_time6 <- Sys.time()
lasso_model_out = glmnet(X, 
                         y, 
                         alpha=1, 
                         lambda=grid_lasso, 
                         thresh=1e-12)
end_time6 <- Sys.time()

predict(lasso_model_out, type="coefficients", s=best_lambda)

### iii) Compare model performances
### Computation time 

# Computation time ridge cross-validation
comp_step3 <- end_time3 - start_time3
print(comp_step3)

# Computation time ridge final
comp_step4 <- end_time4 - start_time4
print(comp_step4)

# Computation time LASSO cross-validation
comp_step5 <- end_time5 - start_time5
print(comp_step5)
# Computation time LASSO final
comp_step6 <- end_time6 - start_time6
print(comp_step6)

### Holdout MSE 

# Test error ridge
ridge_test_mse <- mean((ridge_pred - y[-train_index])^2)
ridge_test_mse

# Test error LASSO
lasso_test_mse <- mean((lasso_pred - y[-train_index])^2)
lasso_test_mse

### Cross-validation MSE
# CV MSE ridge
cv_mse_ridge <- ridge_model$cvm[which.min(ridge_model$cvm)]
cv_mse_ridge

# CV MSE LASSO
cv_mse_lasso <- lasso_model$cvm[which.min(lasso_model$cvm)]
cv_mse_lasso

# CV plot ridge
plot(ridge_model$lambda, 
     ridge_model$cvm, type='l', 
     xlab='Lambda values', 
     ylab='Cross-validation mean squared error')
abline(v=ridge_model$lambda.min, col='pink')
abline(v=ridge_model$lambda.1se, col='deepskyblue')
legend('bottomright', legend=c("Lambda min", "Lambdda 1 SE"), fill=c("pink", "deepskyblue"))

# CV plot LASSO
plot(lasso_model$lambda,
     lasso_model$cvm, type='l',
     xlab='Lambda values',
     ylab='Cross-validation mean squared error',
     title='LASSO cross-validation MSE and lambda plot')
abline(v=lasso_model$lambda.min, col='pink')
abline(v=lasso_model$lambda.1se, col='deepskyblue')
legend('bottomright', legend=c("Lambda min", "Lambdda 1 SE"), fill=c("pink", "deepskyblue"))


# Question 2
parkinsons <- read.csv("parkinsons.csv")

X <- model.matrix(UPDRS ~ ., parkinsons)[,-1]
y <- parkinsons$UPDRS

set.seed(13)

IndexTrain <- sample(1:nrow(X), 30)
IndexTest <- -IndexTrain

X <- scale(X)

## i) Linear model fitting training data
library(ggplot2)
par(mfrow=c(2,2))
ggplot(parkinsons, aes(UPDRS)) + geom_density(fill="aquamarine") + ggtitle("Non-transformed")
ggplot(parkinsons, aes(log(UPDRS))) + geom_density(fill="hotpink3") + ggtitle("Log-transform")
ggplot(parkinsons, aes(sqrt(UPDRS))) + geom_density(fill="gold") + ggtitle("Square root transform")
ggplot(parkinsons, aes(UPDRS^2)) + geom_density(fill="palegreen1") + ggtitle("Squared transform")

Q2a.linMod <- lm(sqrt(y[IndexTrain]) ~ X[IndexTrain,])

# Coefficients
summary(Q2a.linMod)$coefficients[summary(Q2a.linMod)$coefficients[,4] < 0.05, ]
# R squared
summary(Q2a.linMod)$r.squared
# Residuals
summary(Q2a.linMod)$sigma

## ii) Fit training data using LASSO
library(glmnet)
grid <- 10^seq(3, -1, length=100)

set.seed(42)
Q2b.lasso.cv <- cv.glmnet(X[IndexTrain,],
                          y[IndexTrain],
                          alpha=1,
                          lambda=grid,
                          nfolds=30,
                          thresh=1e-10)

Q2b.lam.opt <- Q2b.lasso.cv$lambda.min
Q2b.lam.opt

plot(Q2b.lasso.cv)
abline(v=log(Q2b.lam.opt), col='pink')

Q2b.lasso.pred <- predict(Q2b.lasso.cv, s=Q2b.lam.opt, newx=X[IndexTest,])

mean((Q2b.lasso.pred^2 - y[IndexTest])^2)

## iii) Final model
Q2c.lasso.mod = glmnet(X, 
                       y, 
                       alpha=1, 
                       lambda=grid, 
                       thresh=1e-10)

predict(Q2b.lasso.cv, type="coefficients", s=Q2b.lam.opt)

## iv) Repeat analysis with new split

#Splitting data set
set.seed(17)
Q2d.IndexTrain <- sample(1:nrow(X), 36)
Q2d.IndexTest <- -Q2d.IndexTrain

#Leave-one-out method to determine lambda
set.seed(42)
Q2d.lasso.cv <- cv.glmnet(X[Q2d.IndexTrain,],
                          y[Q2d.IndexTrain],
                          alpha=1,
                          lambda=grid,
                          nfolds=30,
                          thresh=1e-10)

#Investigate lambda
Q2d.lam.opt <- Q2d.lasso.cv$lambda.min
Q2d.lam.opt

#Plot lambda
plot(Q2d.lasso.cv)
abline(v=log(Q2d.lam.opt), col='pink')

#Test error
Q2d.lasso.pred <- predict(Q2d.lasso.cv, s=Q2d.lam.opt, newx=X[Q2d.IndexTest,])
mean((Q2d.lasso.pred^2 - y[Q2d.IndexTest])^2)

#Final model
Q2d.lasso.mod = glmnet(X, 
                       y, 
                       alpha=1, 
                       lambda=grid, 
                       thresh=1e-10)
predict(Q2d.lasso.cv, type="coefficients", s=Q2d.lam.opt)

# Question 3 
## a) Train an ElasticNet model using cross-validation to optimize alpha and lambda
### i) ElasticNet model
library(readr)
ins_data <- read_csv("insurance.csv")

train_index <- sample(nrow(ins_data), 0.7 * nrow(ins_data))
test <- -train_index

X <- model.matrix(charges ~., ins_data)
y <- ins_data$charges

library(caret)
library(glmnet)
grid <- expand.grid(.alpha  = seq(0, 1, by = 0.1), .lambda = 10^seq(5, -2, length = 100))

control <- trainControl(method = "cv", number = 10)

enet_train <- train(x         = X[train_index,],
                    y         = y[train_index],
                    method    = "glmnet",
                    trControl = control,
                    tuneGrid  = grid)
best_alpha <- enet_train$bestTune$alpha
best_lam <- enet_train$bestTune$lambda
print(best_alpha)
print(best_lam)

lambda_vec <- 10^seq(5, -2, length = 100)
enet_cv <- cv.glmnet(X[train_index,],
                     y[train_index],
                     alpha  = best_alpha,
                     lambda = lambda_vec,
                     nfolds = 10,
                     thresh = 1e-12)

plot(enet_cv)
abline(v=log(enet_cv$lambda.1se), col='pink')
abline(v=log(enet_cv$lambda.min), col='lightblue')

### ii) Lambda one standard error
coef(enet_cv, s = "lambda.1se")

predict_1se <- predict(enet_cv,
                         newx = X[test,],
                         s    = "lambda.1se")

res_1se <- predict_1se - y[test]
mean(res_1se ^ 2)

enet_cv$cvm[enet_cv$lambda == enet_cv$lambda.1se]

### iii) Lambda min
coef(enet_cv, s = "lambda.min")
predict_lmin  <- predict(enet_cv,
                          newx = X[test,],
                          s    = "lambda.min")
res_lmin <- predict_lmin - y[test]
mean(res_lmin ^ 2)
enet_cv$cvm[enet_cv$lambda == enet_cv$lambda.min]

### iv) Best model between lambda.min and lambda.1se
## b) Ridge and LASSO models 

### i) Ridge model
set.seed(89)
grid_ridge <- 10^seq(4, -10, length=100)
ridge_model <- cv.glmnet(X[train_index,],
                         y[train_index],
                         alpha=0,
                         lambda=grid_ridge,
                         nfolds=10,
                         thresh=1e-12)

best_lambda <- ridge_model$lambda.min
best_lambda

plot(ridge_model)
abline(v=log(best_lambda), col='pink')

ridge_pred <- predict(ridge_model, s=best_lambda, newx=X[-train_index,])

ridge_model_out = glmnet(X,
                         y,
                         alpha=0,
                         lambda=grid_ridge,
                         thresh=1e-12)

predict(ridge_model_out, type="coefficients", s=best_lambda)

# Test error ridge
ridge_test_mse <- mean((ridge_pred - y[-train_index])^2)
ridge_test_mse

### ii) LASSO model
set.seed(83)
grid_lasso <- 10^seq(4, -10, length=100)
lasso_model <- cv.glmnet(X[train_index,], 
                         y[train_index], 
                         alpha=1, lambda=grid_lasso, 
                         nfolds=10, 
                         thresh=1e-12)

# Lambda
best_lambda <- lasso_model$lambda.min
best_lambda

plot(lasso_model)
abline(v=log(best_lambda), col='pink')

# Predictions
lasso_pred <- predict(lasso_model, s=best_lambda, newx=X[-train_index,])

lasso_model_out = glmnet(X, 
                         y, 
                         alpha=1, 
                         lambda=grid_lasso, 
                         thresh=1e-12)


predict(lasso_model_out, type="coefficients", s=best_lambda)

# Test error LASSO
lasso_test_mse <- mean((lasso_pred - y[-train_index])^2)
lasso_test_mse


