# Chapter 3 Lab: Linear Regression

library(MASS)
library(ISLR)

#---------Simple Linear Regression---------#

fix(Boston)
names(Boston)

#Will produce an Error
lm.fit=lm(medv~lstat)

#This is how you pull it properly
lm.fit=lm(medv~lstat,data=Boston)
attach(Boston)
lm.fit=lm(medv~lstat)

#Basic info about the model
lm.fit
#Detailed information
summary(lm.fit)
names(lm.fit)

#extrctor function
coef(lm.fit)
#Confidence Interval
confint(lm.fit)
predict(lm.fit,data.frame(lstat=(c(5,10,15))), interval="confidence")
predict(lm.fit,data.frame(lstat=(c(5,10,15))), interval="prediction")

#Plot the regression line
plot(lstat,medv)
abline(lm.fit)
abline(lm.fit,lwd=3)
abline(lm.fit,lwd=3,col="red")
plot(lstat,medv,col="red")
plot(lstat,medv,pch=20)
plot(lstat,medv,pch="+")
plot(1:20,1:20,pch=1:20)
#Divides into 2x2 grid panel
par(mfrow=c(2,2))
plot(lm.fit)

#Pull the residuals
plot(predict(lm.fit), residuals(lm.fit))
#R Studentized Residuals
plot(predict(lm.fit), rstudent(lm.fit))

#Leverage Statistics
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))



#---------Multiple Linear Regression---------#

#Predict using three predictors
lm.fit=lm(medv~lstat+age,data=Boston)
#Summary for all predictors
summary(lm.fit)

#Shorthand for all predictors
lm.fit=lm(medv~.,data=Boston)
summary(lm.fit)

library(car)
#Compute variance inflation
vif(lm.fit)

#Use all predictors except age
lm.fit1=lm(medv~.-age,data=Boston)
summary(lm.fit1)
lm.fit1=update(lm.fit, ~.-age)


#---------Interaction Terms---------#

#Create a reaction term between lstat, age, and lstat x Age as an interaction term as a predictor
summary(lm(medv~lstat*age,data=Boston))



#---------Non-linear Transformations of the Predictors---------#

lm.fit2=lm(medv~lstat+I(lstat^2))
summary(lm.fit2)

#Find how the quadratic fit is better than the linear fit
lm.fit=lm(medv~lstat)
anova(lm.fit,lm.fit2)
par(mfrow=c(2,2))
plot(lm.fit2)

#Create a cubit fit
lm.fit5=lm(medv~poly(lstat,5))
summary(lm.fit5)

#log transformed prediction
summary(lm(medv~log(rm),data=Boston))


#---------Qualitative Predictors---------#

fix(Carseats)
names(Carseats)
lm.fit=lm(Sales~.+Income:Advertising+Price:Age,data=Carseats)
summary(lm.fit)
attach(Carseats)
contrasts(ShelveLoc)


#---------Writing Functions---------#

LoadLibraries
LoadLibraries()
LoadLibraries=function(){
  library(ISLR)
  library(MASS)
  print("The libraries have been loaded.")
}
LoadLibraries
LoadLibraries()


#---------Lab 2 Problems---------#

#Problem 1
fix(Boston)
names(Boston)

rm(list=ls())
attach(Boston)
lm.fit=lm(medv~lstat)

lm.fit=lm(medv~lstat,data=Boston)

#Basic info about the model
lm.fit
#Detailed information
summary(lm.fit)
names(lm.fit)

#extrctor function
coef(lm.fit)
#Confidence Interval
confint(lm.fit)
predict(lm.fit,data.frame(lstat=(c(5,10,15))), interval="confidence")
predict(lm.fit,data.frame(lstat=(c(5,10,20))), interval="prediction")



#Problem 2 - 3

#Use all predictors except age
lm.fit1=lm(medv~.-age,data=Boston)
summary(lm.fit1)

which.max(rstudent(lm.fit1))
summary(rstudent(lm.fit1))
plot(rstudent(lm.fit1))

hatvalues(lm.fit1)
which.max(hatvalues(lm.fit1))
summary(hatvalues(lm.fit1))
plot(hatvalues(lm.fit1))


cooks.distance(lm.fit1)
which.max(cooks.distance(lm.fit1))
summary(cooks.distance(lm.fit1))
plot(cooks.distance(lm.fit1))


#Problem 4

model1= lm(medv~lstat*age,data=Boston)

summary(model1)$r.sq
summary(model1)$sigma

model2= lm(medv~lstat*black,data=Boston)

summary(model2)$r.sq
summary(model2)$sigma


#Problem 5
summary(lm(medv~log(rm),data=Boston))



#Problem 6

library(car)
fix(Carseats)
names(Carseats)
lm.fit=lm(Sales~.+Income:Advertising+Price:Age,data=Carseats)
summary(lm.fit)

lm.fit2=lm(Sales~.+Income:Advertising+Price:Age,data=Carseats,contrasts = list(ShelveLoc=contr.treatment(c("Bad", "Good", "Medium"), base = 3)))
contrasts = list(ShelveLoc=contr.treatment(c("Bad", "Good", "Medium"), base = 3)) 

summary(lm.fit2)
attach(Carseats)
contrasts(ShelveLoc)






