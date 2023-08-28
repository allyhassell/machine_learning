# STAT448-23S1 - Week 4 - Lab 3.

# get your own working directory...
#getwd()
# put your own working directory here... 
#setwd("P:/Downloads/_MADS_2023/STAT448_23S1/Labs_23S1/Lab-3")
# get your own working directory...
#getwd()

# Part 1.

# Linear regression single variable.
data <- data.frame(x1 = c(0, 1, 1), y = c(2, 2, 8))
data

# ScatterPlot.
# divide graph area into 1 row with 1 column.
par(mfrow=c(1, 1))
plot(data, xlab="x1", ylab="y", xlim=c(-3, 3), ylim=c(0, 10))

# Output vector y.
y <- c(2, 2, 8)

# Input vector x1.
x1 <- c(0, 1, 1)

# Intercept vector (it is simply the value at which the fitted line crosses the y-axis)
x0 <- rep(1, length(y))

# Let’s create our Y matrix.
Y <- as.matrix(data$y)
Y

# Let’s create our design matrix.
X <- as.matrix(cbind(x0, data$x1))
X

# Let’s compute the regression equation and find the coefficients.
beta <- solve(t(X) %*% X) %*% (t(X) %*% Y)
beta

# comparison with regression function in R.
fit <- lm(y ~ x1)
fit

# compare coefficients - those obtained by hand to those from lm(). 
beta

# lm() model coefficients.
fit$coefficients

# Plot the data and best regression line.
par(mfrow=c(1, 1))
plot(data, xlab="x1", ylab="y", xlim=c(-3, 3), ylim=c(0, 10))
# plot the regression line.
abline(beta, col="red")

# make predictions using the lm model.
Ypredict1 <- predict(fit, data.frame(data$x1))
Ypredict1

# We can also make predictions using our beta coefficients we 
# calculated by hand.
Ypredict2 <- X %*% beta
Ypredict2 

# Ypredict1 and Ypredict2 should be equal.
isTRUE(all.equal(matrix(Ypredict1), matrix(Ypredict2)))

# calculate the R-squared by hand, using the formula:
# R-squared = 1 - (RSS/TSS). Where RSS is the residual sum of squares 
# and TSS is the total sum of squares.

# First we calculate the residual sum of squares, the squared difference
# between the y's and and the predicted y's.
RSS <- sum((y - Ypredict2)^2)
RSS

# We then compute the total sum of squres, the squared difference
# between the y's and the mean value of y. The baseline prediction is
# the mean of y.
baseline <- mean(y)
SST <- sum((y - baseline)^2) 
SST

# R-squared.
RSquared <- 1 - (RSS / SST)
RSquared

# display model summary. compare regression coefficients and R-squared
# with those obtained by hand. They should be the same.
summary(fit)  


# Part 2.

# load required packages.
library(MASS)
library(ggplot2)

# using the Boston house price dataset.
attach(Boston)

# display the variable names in the Boston dataset.
names(Boston)

# the help() function provides detailed information on a dataset.
help(Boston)

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

# Explore the data. Does anything standout to you?
ggplot(Boston, aes(medv)) + geom_density(fill="blue")

# Why are we applying a transform to the response variable medv? 
# Has it helped?
ggplot(train, aes(log(medv))) + geom_density(fill="blue") # Log is better for this type of data 
ggplot(train, aes(sqrt(medv))) + geom_density(fill="blue")

# Let's make default model. Note the log transform.
model1 <- lm(log(medv)~., data=train)
summary(model1)

# partition the plot window, two rows and two columns.
par(mfrow=c(2,2))
# produce the diagnostic plots for model1.
plot(model1)

# remove the less significant features 
# (normal practice would be to remove these one at a time)
model2 <- update(model1, ~.-zn-indus-age) # statistically useful
summary(model2) 

par(mfrow=c(2,2))
# produce the diagnostic plots for model2.
plot(model2)

# Plot the residual plot with all predictors.
# Expect to see some warning messages.
attach(train)
require(gridExtra)
plot1 <- ggplot(train, aes(crim, residuals(model2))) + geom_point() + geom_smooth()
plot2 <- ggplot(train, aes(chas, residuals(model2))) + geom_point() + geom_smooth()
plot3 <- ggplot(train, aes(nox, residuals(model2))) + geom_point() + geom_smooth()
plot4 <- ggplot(train, aes(rm, residuals(model2))) + geom_point() + geom_smooth()
plot5 <- ggplot(train, aes(dis, residuals(model2))) + geom_point() + geom_smooth()
plot6 <- ggplot(train, aes(rad, residuals(model2))) + geom_point() + geom_smooth()
plot7 <- ggplot(train, aes(tax, residuals(model2))) + geom_point() + geom_smooth()
plot8 <- ggplot(train, aes(ptratio, residuals(model2))) + geom_point() + geom_smooth()
plot9 <- ggplot(train, aes(black, residuals(model2))) + geom_point() + geom_smooth()
plot10<- ggplot(train, aes(lstat, residuals(model2))) + geom_point() + geom_smooth()
grid.arrange(plot1,plot2,plot3,plot4,plot5,plot6,plot7,plot8,plot9,plot10,ncol=5,nrow=2)

# Lets make the default model and add square term to the model.
# Note the use of the I() function so we don't get interaction terms.
model3 <- lm(log(medv)~crim+chas+nox+rm+dis+rad+tax+ptratio+
               black+lstat+ I(crim^2)+ I(chas^2)+I(nox^2)+ I(rm^2)+ 
               I(dis^2)+I(rad^2)+ I(tax^2)+ I(ptratio^2)+ I(black^2)+ 
               I(lstat^2), data=train)
summary(model3)

# Removing the insignificant variables.
model4 <- update(model3,~.-nox-rad-tax-ptratio-black-
                   I(nox^2)-I(rad^2)-I(tax^2)-I(ptratio^2)-I(black^2))
summary(model4)
par(mfrow=c(2,2))
# produce the diagnostic plots for model4.
plot(model4)

# Make predictions on the test dataset and calculate the RMSE.
pred1 <- predict(model4, newdata = test)
rmse <- sqrt(sum((exp(pred1) - test$medv)^2)/length(test$medv))

# create a summary table of our results.
c(RMSE = rmse, R2 = summary(model4)$r.squared)

# Your turn now: In the second part of the lab, replicate the linear 
# algebra calculations using the evaporation dataset from lab 1 or 
# another dataset of your choosing. 

# Perform your analysis in an R-Markdown HTML or PDF report.

# Please feel free to consult with your tutors or fellow students if 
# you have any questions.

