# STAT448-23S1 - Week 5 - Lab 4.

# get your own working directory...
#getwd()
# put your own working directory here... 
#setwd("P:/Downloads/_MADS_2023/STAT448_23S1/Labs_23S1/Lab-5")

# Searching for the best model using forward, backward and stepwise search.

library(MASS)
library(ggplot2)

# uncomment the next line if you have not installed package LEAPS before.
#install.packages('leaps')
# for the regsubsets() function:
library(leaps)
# for the Hitters dataset:
library(ISLR)

# using the Boston house price dataset.
attach(Boston)

# display the variable names in the Boston dataset.
names(Boston)

# the help() function provides detailed information on a 
# dataset. Note: The output from this command will not 
# show up in the markdown pdf document (it will open a web page).
help(Boston)

# create a matrix of scatterplots using all variables in the Boston house price dataset.
pairs(Boston)


# set seed for reproducible results.
set.seed(2023)

# Sample the dataset. Returns a list of row indices. 80:20 split.
row.number <- sample(1:nrow(Boston), 0.8*nrow(Boston))

# create the train and test datasets.
train <- Boston[row.number,]
test <- Boston[-row.number,]

# show dimensions of the train and test sets.
dim(train)
dim(test)

# Let's make default model. Note the log transform.
model1 <- lm(log(medv)~., data=train)
summary(model1)

# partition the plot window, two rows and two columns.
par(mfrow=c(2,2))
# produce the diagnostic plots for model1.
plot(model1)

# backward search using t-values (alpha level 0.05)
model2 <- update(model1, ~.-age) 
summary(model2)

model3 <- update(model2, ~ . -indus)
summary(model3)

par(mfrow = c(2,2))
plot(model3)

# Automated backward search using AIC
help(stepAIC)

model4 <- stepAIC(model1, direction="backward")
summary(model4)

# first create null model.
model0 <- lm(log(medv)~1, data=train)
summary(model0)

# Automatic forward search using AIC
model5 <- stepAIC(model0, direction="forward", scope=list(upper=model1, lower=model0))
summary(model5)

# Automated stepwise search using AIC - both directions
model6 <- stepAIC(model0, direction="both", scope=list(upper=model1, lower=model0))
summary(model6)

# Automated exhaustive search
reg.out <- regsubsets(log(medv) ~ ., data = train, nvmax = 13)
summary(reg.out)

# Prediction try predictions from different models
pred1 <- predict(model5, newdata = test)

rmse <- sqrt(sum((exp(pred1) - test$medv)^2)/length(test$medv))
c(RMSE = rmse, R2 = summary(model5)$r.squared)


# some basic exploration of the Hitters dataset.
# show variable names
names(Hitters)
# show the dimension of the dataset
dim(Hitters)
# how many rows with NA values?
sum(is.na(Hitters$Salary))
# drop all the rows with NA values. DON'T do this in real life!!!
Hitters <- na.omit(Hitters)

# show dimensions after removing rows containing NA's
dim(Hitters)
# verify we have no more rows with NA's
sum(is.na(Hitters))




# fit a full model.
regfit.full <- regsubsets(Salary~., data = Hitters, nvmax = 19)
# save model output in an R object (variable)
reg.summary <- summary(regfit.full)
# show available output variables.
names(reg.summary)
# show the R-squared obtained  for each model.
reg.summary$rsq

par(mfrow=c(2,2))

plot(reg.summary$rss, xlab="Number of Variables", ylab="RSS", type="l")
plot(reg.summary$adjr2, xlab="Number of Variables", ylab="Adjusted RSq", type="l")

# find model number with the largest adjusted r-squared.
model_with_largest_adjr2 <- which.max(reg.summary$adjr2)
model_with_largest_adjr2
# add point to plot showing best model (in terms of ahusted r-squared)
points(model_with_largest_adjr2, reg.summary$adjr2[model_with_largest_adjr2], 
       col="red", cex=2, pch=20)

# Find model with best Mellows Cp (smallest value)
plot(reg.summary$cp, xlab="Number of Variables", ylab="Cp", type='l')
model_with_smallest_cp <- which.min(reg.summary$cp)
model_with_smallest_cp
points(model_with_smallest_cp, reg.summary$cp[model_with_smallest_cp], 
       col="red", cex=2, pch=20)

# Find model with the best BIC (smallest value)
model_with_smallest_bic <- which.min(reg.summary$bic)
model_with_smallest_bic
plot(reg.summary$bic, xlab="Number of Variables", ylab="BIC", type='l')
points(model_with_smallest_bic, reg.summary$bic[model_with_smallest_bic], 
       col="red", cex=2, pch=20)

# Some more plots
plot(regfit.full, scale="r2")
plot(regfit.full, scale="adjr2")
plot(regfit.full, scale="Cp")
plot(regfit.full, scale="bic")

# show coefficients for the best model based on BIC.
coef(regfit.full, model_with_smallest_bic)

# Forward and Backward Stepwise Selection
regfit.fwd <- regsubsets(Salary~., data = Hitters, 
                         nvmax = 19, method = "forward")
summary(regfit.fwd)

regfit.bwd <- regsubsets(Salary~., data = Hitters, 
                         nvmax = 19, method = "backward")
summary(regfit.bwd)

# show model coefficients for the 7th model (has the best 7 predictor variables) from each method.
coef(regfit.full, 7)
coef(regfit.fwd, 7)
coef(regfit.bwd, 7)

# Choosing Among Models - Holdout Method
# set seed for reproducibility
set.seed(2023)

# create a random train/test split 50:50.
train <- sample(c(TRUE,FALSE), nrow(Hitters), rep = TRUE)
test <- (!train)

# train models.
regfit.best <- regsubsets(Salary~., data = Hitters[train,], 
                          nvmax = 19)
# create a design matrix.
test.mat <- model.matrix(Salary~., data = Hitters[test,])

# Make Predictions
# create a vector to hold MSE's for each model.
mse <- rep(NA, 19)
for(i in 1:19){
  coefi         <- coef(regfit.best, id=i)
  pred          <- test.mat[,names(coefi)]%*%coefi
  mse[i] <- mean((Hitters$Salary[test] - pred)^2)
}

# show MSE vector. 
mse

# which model had the smallest MSE?
fit_with_smallest_mse <- which.min(mse)
fit_with_smallest_mse

# show model coefficients for model with the least mse.
coef(regfit.best, fit_with_smallest_mse)




# Choosing among models - CV Method

# regsubsets does not provide its own predict function so we have to provide one.
# define a  predict function (used further down).
predict.regsubsets <- function(object, newdata, id, ...){
  form  <- as.formula(object$call[[2]])
  mat   <- model.matrix(form, newdata)
  coefi <- coef(object, id=id)
  xvars <- names(coefi)
  mat[,xvars]%*%coefi
}

# try 10-fold cross-validation.
k <- 10
# set seed for reproducibility.
set.seed(2023)

# assign every row in Hitters a random fold number (1-10)
folds <- sample(1:k, nrow(Hitters), replace=TRUE)

# a matrix to hold MSE values for each model and each fold.
cv.mse <- matrix(NA, k, 19, dimnames=list(NULL, paste(1:19)))

# for each fold, build nvmax models.
for(j in 1:k){
  best.fit <- regsubsets(Salary~., data = Hitters[folds!=j,], 
                         nvmax = 19)
  # for every model created for the current fold, make predictions
  # on the test dataset and calculate the resultant mse.
  for(i in 1:19){
    pred <- predict(best.fit, Hitters[folds==j,], id = i)
    cv.mse[j,i] <- mean( (Hitters$Salary[folds==j] - pred)^2)
  }
}
# show mse values in cv.mse matrix
cv.mse

# calculate the mean CV MSE.
mean.cv.mse <- apply(cv.mse, 2, mean)
mean.cv.mse

# find fit with least MSE
fit_with_smallest_mse <- which.min(mean.cv.mse)
fit_with_smallest_mse

# plot mean cv errors for each model.
par(mfrow=c(1,1))
plot(mean.cv.mse, type='b')
# highlight the best.
points(fit_with_smallest_mse, mean.cv.mse[fit_with_smallest_mse],
       col="red", cex=2, pch=20)

# Finally, we build models using all the data and extract and show the coefficients from the best model.
reg.best <- regsubsets(Salary~., data=Hitters, nvmax=19)
coef(reg.best, fit_with_smallest_mse)

# The End.

