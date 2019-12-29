#type a function
funcname(input1,input2)

#Write a vector
x <- c(1,3,2,5)
x

#save a variable
x = c(1,6,2)
y = c(1,4,3)

#length of different vectors
length(x)
length(y)
x+y

#List all of the objects
ls()

#Delete unwanted objects
rm(da,m,Order_Item_Table_POTOOL,x,y)
ls()

#remove all objects at once
rm(list=ls())

#Create a simple matrix
x=matrix(data=c(1,2,3,4), nrow=2, ncol=2)
x

#Omitting nrow and ncol
x=matrix(c(1,2,3,4),2,2)
x

#Populate the Matrix by order of the rows
matrix(c(1,2,3,4),2,2, byrow=TRUE)

#Square root the matrix and raise it to the power of 2
sqrt(x)
x^2

#Generate a vector of random normal variables with an argument of "n" sample size
x=rnorm(50)

#Dictate the mean and standard deviation within the set of integers
y=x+rnorm(50,mean=50,sd=.1)

#Compute the correlation between x and y
cor(x,y)

#Create a arbitrary integer argument using set.seed and also use the exact same set of integers
set.seed(1303)
rnorm(50)

#Compute the mean and variance of a vector of numbers.
set.seed(3)
y=rnorm(100)
mean(y)
var(y)

#Find the standard deviation
#Using the variance
sqrt(var(y))
#Using sd
sd(y)

#--------------Graphics--------------#
#Plot a graph
x=rnorm(100)
y=rnorm(100)
plot(x,y)
plot(x,y,xlab="this is the x-axis",ylab="this is the y-axis",main="Plot of X vs Y")
#Create a pdf or save as a jpeg: jpeg()
pdf("Figure.pdf")
plot(x,y,col="green")

#Tell R that we are done creating the plot
dev.off()

#Create a sequence of numbers
x=seq(1,10)
x
#Shorthand for a sequence of numbers between 1 and 10
x=1:10
x

#Sequence of numbers equally spaced
y = seq(0,1,length = 10)

x=seq(-pi,pi,length=50)

#Create a contour plot
y=x
f=outer(x,y,function(x,y)cos(y)/(1+x^2))
contour(x,y,f)
contour(x,y,f,nlevels=45,add=T)
fa=(f-t(f))/2
contour(x,y,fa,nlevels=15)

#Create a similar contour plot where the colors depend on the z value
image(x,y,fa)
persp(x,y,fa)
persp(x,y,fa,theta=30)
persp(x,y,fa,theta=30,phi=20)
persp(x,y,fa,theta=30,phi=70)
persp(x,y,fa,theta=30,phi=40)

# Indexing Data

A=matrix(1:16,4,4)
A
A[2,3]

#Selecting multiple rows and columns at a time by providing vectors as indicies (Row then column)
A[c(1,3),c(2,4)]
A[1:3,2:4]
A[1:2,]
A[,1:2]

# R treats a signle row or column of a matrix as a vector
A[1,]

#Use of the negative sign in the index tells R to keep all rows or columns except those indicated in the index
A[-c(1,3),]
A[-c(1,3),-c(1,3,4)]

#Output the number of rows followed by the number of columns
dim(A)



#--------------Loading Data--------------#
#Read Data table into object
Auto=read.table("Auto.data", stringAsFactors=TRUE)
#Read the data frame in a table
fix(Auto)

#Read the table into a dataframe and add the header of the table as variables
#Also, let R know that anytime it sees a "?" there is a missing element
Auto=read.table("Auto.data",header=T,na.strings="?")
fix(Auto)

#Read a csv into R
Auto=read.csv("Auto.csv",header=T,na.strings="?")
fix(Auto)
dim(Auto)
Auto[1:4,]

#Omit rows with missing observations
Auto=na.omit(Auto)
dim(Auto)

#Check the variable names
names(Auto)

#--------------Additional Graphical and Numerical Summaries--------------#

#Plot variables in the data set
#(incorrect)
plot(cylinders, mpg)
#Correct
plot(Auto$cylinders, Auto$mpg)
attach(Auto)
plot(cylinders, mpg)

#Convert qauntitative variables to qualitative variables
cylinders=as.factor(cylinders)

#Plot the variables with different preferences
plot(cylinders, mpg)
plot(cylinders, mpg, col="red")
plot(cylinders, mpg, col="red", varwidth=T)
plot(cylinders, mpg, col="red", varwidth=T,horizontal=T)
plot(cylinders, mpg, col="red", varwidth=T, xlab="cylinders", ylab="MPG")

#Plot a histogram of the data
hist(mpg)
hist(mpg,col=2)
hist(mpg,col=2,breaks=15)

#Pairs() creates a scatterplot matrix
pairs(Auto)
pairs(~ mpg + displacement + horsepower + weight + acceleration, Auto)

#Print interactive aspects of the variables in the plot
plot(horsepower,mpg)
identify(horsepower,mpg,name)

#Shows numerical summary of each variable in the data set
summary(Auto)

#Prints a summary of one qualitative variable
summary(mpg)

#---------- Lab 1 Part 2 ---------- #

set.seed(3)
y = rnorm(100)
summary(y)

?plot

a=matrix(1:16,4,4)
a

a[,2]

str(Auto)




