## STAT318/462 Lab4: Cross-Validation and the Bootstrap
#
# In this lab you will work through Section 5.3 of the course
# textbook, An Introduction to Statistical Learning (there is a
# link to this textbook on the Learn page). The R code from 
# Section 5.3 is given below. 


############## The Validation Set Approach ########################

library(ISLR)
attach(Auto,warn.conflicts = FALSE)
set.seed(10)
dim(Auto)
names(Auto)
Auto[1:4,]
plot(horsepower,mpg)

# Define the training sample
train=sample(392,196)
points(horsepower[train],mpg[train],col="red")

# Fit the linear model
lm.fit=lm(mpg~horsepower,data=Auto,subset=train)
mean((mpg-predict(lm.fit,Auto))[-train]^2)
abline(lm.fit)

# Fit a second order polynomial
lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)
horselims = range(horsepower)
horsepower.grid = seq(from=horselims[1],to=horselims[2])
preds=predict(lm.fit2,newdata=list(horsepower=horsepower.grid))
lines(horsepower.grid,preds,col="green")

# Fit a third order polynomial
lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)
preds=predict(lm.fit3,newdata=list(horsepower=horsepower.grid))
lines(horsepower.grid,preds,col="blue")


########### k-Fold Cross-Validation #################################


library(boot)
set.seed(1)
cv.error.10=rep(0,10)
for (i in 1:10){
 glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
 cv.error.10[i]=cv.glm(Auto,glm.fit,K=10)$delta[1]
 }
cv.error.10
plot(1:10,cv.error.10,type="b",col="red")


############## The Bootstrap ########################################


# A function to calculate the optimal value (minimum variance)
alpha = function(data){
 X = data$X
 Y = data$Y
 return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}
alpha(Portfolio)

#What is the standard error of alpha? 

alpha.fn=function(data,index){
 X=data$X[index]
 Y=data$Y[index]
 return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
 }
# check the function
alpha.fn(Portfolio,1:100)

set.seed(1)
# one iteration
alpha.fn(Portfolio,sample(100,100,replace=T))
# the bootstrap
boot.out=boot(Portfolio,alpha.fn,R=1000)
boot.out
plot(boot.out)


## Estimating the accuracy of a linear regression model #############

boot.fn=function(data,index){
return(coef(lm(mpg~horsepower, data=data,subset=index)))
}
boot.fn(Auto,1:392)
set.seed(1)
boot.fn(Auto,sample(392,392,replace=T))
boot.fn(Auto,sample(392,392,replace=T))

boot(Auto,boot.fn,100)
summary(lm(mpg~horsepower, data=Auto))$coef



