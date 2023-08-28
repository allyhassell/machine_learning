# STAT448-23S1 - Week 2 - Lab 1.

# Import a package
library(readr)

# Read CSV file
evaporation <- read_csv("evaporation.csv")

# display the dataset in tabular form.
View(evaporation)

# To make it work, one must use name_of_dataset_variable$name_of_column
hist(evaporation$Evaporation, col="orange") 

# Attach the dataset to use the names of columns as variables
attach(evaporation)

# now it works
hist(Evaporation, col="orange") 
hist(Velocity, col="purple")

boxplot(Evaporation, col="orange")
boxplot(Velocity, col="purple")

# partition plot window into one row of two columns (one for each of the
# next two boxplots).
par(mfrow = c(1, 2))
boxplot(Evaporation, main="Evaporation", col="orange")
boxplot(Velocity, main="Velocity", col="purple")

# reset plot window.
par(mfrow = c(1, 1))
plot(Velocity, Evaporation, pch=16, col="red")

# compute correlation coefficient.
cor(Velocity, Evaporation)

# Build a simple linear model, using velocity to model evaporation rate.
regevap <- lm(Evaporation ~ Velocity)

# Print linear regression summary
summary(regevap)

# get model coefficients
coeff <- coefficients(regevap)

# build equation for the line : 
eq <- paste0("y = ", round(coeff[2], 6), "*x +", round(coeff[1], 6))

# plot the actual values, we can use the model equation as the title.
plot(Velocity, Evaporation, pch=16, col="red", main=eq)

# plot the regression line.
abline(lm(Evaporation~Velocity), col="blue")

# show the predicted values.
lines(sort(Velocity), fitted(regevap), col="green") 

# add a lowess smoother.
lines(lowess(Velocity, Evaporation), col="orange") 

# show predicted values.
regevap$fitted.values

# show residual values.
regevap$residuals

# Plot residual versus fitted/predicted plot
plot(regevap$fitted.values, regevap$residuals, pch=16, col='red')
abline(h=0, col="blue")

# You're turn - place the above code is a suitably formatted/commented
#               R-Markdown document.

# Further practice - Take the OLS regression challenge at 
#               https://data.world/nrippner/ols-regression-challenge
#               i.e. predict cancer mortality rates for US counties.
#               You will need to join data.world (free) to be able to 
#               download the data.

#               The following website 
# https://www.telusinternational.com/articles/10-open-datasets-for-linear-regression
#               where the OLS challenge example came from has a number
#               of very good regression examples.


# Note: you can use the R data() function to show all the inbuilt
#       datasets provided in R.

