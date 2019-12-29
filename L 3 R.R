# Cross-Validation and the Bootstrap

#-------The Validation Set Approach-------#

library(ISLR)

#Select Random subset of 196 observations
set.seed(1)
train=sample(392,196)

#Fit linear model to only the observations in the training set
lm.fit=lm(mpg~horsepower,data=Auto,subset=train)

#Using predict function to estimate response for 392 observations. Use mean to calculate MSE
attach(Auto)
mean((mpg-predict(lm.fit,Auto))[-train]^2)

#Estimating the test error for the quadractic and cubic regressions
#Quadratic
lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)

#Cubic
lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)

#Set different training set and check out the errors on the validation set
set.seed(2)
train=sample(392,196)
lm.fit=lm(mpg~horsepower,subset=train)
mean((mpg-predict(lm.fit,Auto))[-train]^2)
lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)
lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)


#-------Leave-One-Out Cross-Validation-------#

#Linear regression using glm instead of lm
glm.fit=glm(mpg~horsepower,data=Auto)
coef(glm.fit)
lm.fit=lm(mpg~horsepower,data=Auto)
coef(lm.fit)

#Get the new library so that cv.glm() can be used
library(boot)
glm.fit=glm(mpg~horsepower,data=Auto)
cv.err=cv.glm(Auto,glm.fit)
#Delta vector contains the cross-validation results
cv.err$delta

#Intialize the vector with a for loop to run many iterations
cv.error=rep(0,5)
for (i in 1:5){
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error[i]=cv.glm(Auto,glm.fit)$delta[1]
}
cv.error


#-------k-Fold Cross-Validation-------#

#K-Fold using 10 (Common) for k in the auto data set
set.seed(17)
cv.error.10=rep(0,10)
for (i in 1:10){
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error.10[i]=cv.glm(Auto,glm.fit,K=10)$delta[1]
}
cv.error.10


#-------The Bootstrap-------#

#Perform bootstrap analysis by frist computing the statistic of interest and then sampling observations repeatedly without replacement
alpha.fn=function(data,index){
  X=data$X[index]
  Y=data$Y[index]
  return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}
#Estimate alpha using 100 observations
alpha.fn(Portfolio,1:100)

#Select 100 observations from 1 to 100 with replacement (New bootstrap data)
set.seed(1)
alpha.fn(Portfolio,sample(100,100,replace=T))

#bootstrap function for 1,000 bootstrap estimates for alpha
boot(Portfolio,alpha.fn,R=1000)


#-------Estimating the Accuracy of a Linear Regression Model-------#

#Use the bootstrap function to get the normal normal estimates for the linear regression model. Then apply to the full set of 392 observations
boot.fn=function(data,index)
  return(coef(lm(mpg~horsepower,data=data,subset=index)))
boot.fn(Auto,1:392)

#Bootstrap estimates for the intercept and slope terms by randomly sampling from the observations with replacement
set.seed(1)
boot.fn(Auto,sample(392,392,replace=T))
boot.fn(Auto,sample(392,392,replace=T))

#Compute the standard errors of 1,000 bootstrap estimates for the intercept and slope forms.
boot(Auto,boot.fn,1000)
#Use the summary function to compute the standard erroprs for the regression coefficients in a linear model
summary(lm(mpg~horsepower,data=Auto))$coef

#Compute the bootstrap standard error estimates and standard linear regression estimates that result from fitting the quadratic model to the data
boot.fn=function(data,index)
  coefficients(lm(mpg~horsepower+I(horsepower^2),data=data,subset=index))
set.seed(1)
boot(Auto,boot.fn,1000)
summary(lm(mpg~horsepower+I(horsepower^2),data=Auto))$coef





#-------- Lab R 3 Work --------#

#Problem 1
set.seed(3)
train=sample(392,196)

#Fit the models
#Linear
lm.fit=lm(mpg~horsepower,data=Auto,subset=train)

#Quadratic
lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)

#Cubic
lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)

#Calculate MSE of other 196 in the validation set.
#Linear
attach(Auto)
mean((mpg-predict(lm.fit,Auto))[-train]^2)

#Quad
lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)

#Cubic
lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)




#Problem 2

#Linear regression using glm instead of lm
glm.fit=glm(mpg~horsepower,data=Auto)
coef(glm.fit)
lm.fit=lm(mpg~horsepower,data=Auto)
coef(lm.fit)

#Get the new library so that cv.glm() can be used
library(boot)
glm.fit=glm(mpg~horsepower,data=Auto)
cv.err=cv.glm(Auto,glm.fit)
#Delta vector contains the cross-validation results
cv.err$delta

#Intialize the vector with a for loop to run many iterations
cv.error=rep(0,5)
for (i in 1:6){
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error[i]=cv.glm(Auto,glm.fit)$delta[1]
}
cv.error



#Problem 4
set.seed(17)
cv.error.10=rep(0,5)
for (i in 1:10){
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error.10[i]=cv.glm(Auto,glm.fit,K=10)$delta[1]
}
cv.error.10


#Problem 5


#Perform bootstrap analysis by frist computing the statistic of interest and then sampling observations repeatedly without replacement
set.seed(2)
alpha.fn=function(data,index){
  X=data$X[index]
  Y=data$Y[index]
  return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}
#Estimate alpha using 100 observations
alpha.fn(Portfolio,1:100)

#Select 100 observations from 1 to 100 with replacement (New bootstrap data)
set.seed(1)
alpha.fn(Portfolio,sample(100,100,replace=T))

#bootstrap function for 1,000 bootstrap estimates for alpha
boot(Portfolio,alpha.fn,R=1000)



#Problem 6

#Use the bootstrap function to get the normal normal estimates for the linear regression model. Then apply to the full set of 392 observations
boot.fn=function(data,index)
  return(coef(lm(mpg~horsepower,data=data,subset=index)))
boot.fn(Auto,1:392)

#Bootstrap estimates for the intercept and slope terms by randomly sampling from the observations with replacement
set.seed(1)
boot.fn(Auto,sample(392,392,replace=T))
boot.fn(Auto,sample(392,392,replace=T))

#Compute the standard errors of 1,000 bootstrap estimates for the intercept and slope forms.
boot(Auto,boot.fn,1000)
#Use the summary function to compute the standard erroprs for the regression coefficients in a linear model
summary(lm(mpg~horsepower,data=Auto))$coef

#Compute the bootstrap standard error estimates and standard linear regression estimates that result from fitting the quadratic model to the data
set.seed(2)
boot.fn=function(data,index)
  coefficients(lm(mpg~horsepower+I(horsepower^2),data=data,subset=index))
set.seed(1)
boot(Auto,boot.fn,1000)
summary(lm(mpg~horsepower+I(horsepower^2),data=Auto))$coef













