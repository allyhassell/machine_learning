# STAT448-23S1 - Week 3 - Lab 2.

# get your own working directory...
getwd()
# put your own working directory here... 
#setwd("P:/your_stat448_dir/your_labs_dir/Lab_2_09032023")

# install package e1071 (run install.packages just the once if needed).
install.packages("e1071")
library(e1071)

# using the R supplied dataset cars. Show the first few rows.
head(cars)

# scatterplot
scatter.smooth(x=cars$speed, y=cars$dist, main="Dist ~ Speed")

# partition plot window into one row of two columns (one for each of the
# next two boxplots).
par(mfrow=c(1, 2))

# box plot for 'speed'
boxplot(cars$speed,
        main="Speed", sub=paste("Outlier rows: ", boxplot.stats(cars$speed)$out))
# box plot for 'distance'
boxplot(cars$dist,
        main="Distance", sub=paste("Outlier rows: ", boxplot.stats(cars$dist)$out))

# divide graph area in 2 columns
#par(mfrow=c(1, 2))

# density plot for 'speed'
plot(density(cars$speed),
     main="Density Plot: Speed",
     ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$speed), 2)))
polygon(density(cars$speed), col="red")

# density plot for 'dist'
plot(density(cars$dist),
     main="Density Plot: Distance",
     ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$dist), 2)))
polygon(density(cars$dist), col="red")

# calculate correlation between speed and distance
cor(cars$speed, cars$dist)

# build linear regression model on full data
linearMod <- lm(dist ~ speed, data=cars)
print(linearMod)
summary(linearMod)

# capture model summary as an object
modelSummary <- summary(linearMod)
# model coefficients
modelCoeffs <- modelSummary$coefficients
modelCoeffs

# get beta estimate for speed
beta.estimate <- modelCoeffs["speed", "Estimate"]
beta.estimate
# get std.error for speed
std.error <- modelCoeffs["speed", "Std. Error"]
std.error

# calc t statistic (compare with model summary).
t_value <- beta.estimate/std.error
t_value

# calc p Value (compare with model summary).
p_value <- 2*pt(-abs(t_value), df=nrow(cars)-ncol(cars))
p_value


# Holdout method - Create Training and Test data.
# set seed for reproducible results.
set.seed(2023)

# Get random row indices for training data. Have a 80:20 split.
trainingRowIndex <- sample(1:nrow(cars), 0.8*nrow(cars))

# Model training data
trainingData <- cars[trainingRowIndex, ]
# Test data
testData  <- cars[-trainingRowIndex, ]

# Build the model on the training data.
lmMod <- lm(dist ~ speed, data=trainingData)
# model summary
summary(lmMod)

# predict distance using test data.
distPred <- predict(lmMod, testData)
distPred

# set default plot window
par(mfrow=c(1, 1))

# plot predicted points.
plot(testData$speed, distPred, pch=16, col="red")
# plot actual points.
points(testData$speed, testData$dist, pch=16, col="green") 

# Your turn now - analyse the Anscombe quartet dataset by 
# regressing y1 on x1, y2 on x2 and so on. 
# Perform your analysis in an R-Markdown HTML or PDF report. 
# Please feel free to consult with your tutors, Chris and Dion, or fellow students if 
# you have any questions.
anscombe

