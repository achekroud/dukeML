# Introduction to R
# Please try and be familiar with the following code before we start
# If you have used R and know all of this, feel free to stop reading.

# Basic Commands

x <- c(1,3,2,5) # make a vector called x
x               # print x
x = c(1,6,2)    # equals and the arrow are the same
x               # what is x now?
y = c(1,4,3)    # make another vector called y
length(x)       # get the length of x
length(y)       # get the length of y
x+y             # add two vectors of the same length
ls()		 # list contents of the environment
rm(x,y)		 # delete x and y from environment
ls()
rm(list=ls())   # delete everything
?matrix		 # get help for the matrix function
x = matrix(data = c(1,2,3,4), nrow=2, ncol=2)	# create a 2x2 matrix
x						# print the matrix
x=matrix(c(1,2,3,4),2,2)			# same thing
matrix(c(1,2,3,4),2,2,byrow=TRUE)		# create it by row instead of by column
sqrt(x)						# square root of each element in matrix
x^2						# square everything in the matrix
x=rnorm(50)					# make a vector, length 50, of gaussian data ~ mu= 0 var = 1
y = x + rnorm(50,mean=50,sd=.1)
cor(x,y)					# correlation between x and y vectors
set.seed(1303)					# fix the random number generator seed
rnorm(50)					# generate reproducibly-random data!
set.seed(3)					# a different seed
y=rnorm(100)					# 100 draws from gaussian noise
mean(y)						# mean of the samples we just made
var(y)						# variance of the samples we made
sqrt(var(y))					# calculate sd (the hard way)
sd(y)						# calculate sd the easy way

# Graphics

x = rnorm(100)
y = rnorm(100)
plot(x,y)					# xy scatter plot
plot(x,y,xlab="this is the x-axis",ylab="this is the y-axis",main="Plot of X vs Y")
pdf("Figure.pdf")				# save the figure as a pdf
plot(x,y,col="green")				# play with colour settings
dev.off()					# turns off (restarts) plotting environment
x = seq(1,10)					# try the sequence function
x
x = 1:10						# this is the same result
x
x=seq(-pi,pi,length=50)			# fancier sequences
y=x
f=outer(x,y,function(x,y)cos(y)/(1+x^2))	# fancy plotting
contour(x,y,f)
contour(x,y,f,nlevels=45,add=T)
fa=(f-t(f))/2
contour(x,y,fa,nlevels=15)
image(x,y,fa)
persp(x,y,fa)
persp(x,y,fa,theta=30)
persp(x,y,fa,theta=30,phi=20)
persp(x,y,fa,theta=30,phi=70)
persp(x,y,fa,theta=30,phi=40)

# Indexing Data

A = matrix(1:16,4,4)			# create 4x4 matrix
A
A[2,3]					# play around indexing it
A[c(1,3),c(2,4)]
A[1:3,2:4]
A[1:2,]
A[,1:2]
A[1,]
A[-c(1,3),]
A[-c(1,3),-c(1,3,4)]
dim(A)					# dimensions of the A matrix

# Loading Data

Auto = read.table("Auto.data")		# reading data is super easy in R 
Auto = read.table("Auto.data",header=T,na.strings="?") # take a look at the settings in ?read.table
Auto = read.csv("Auto.csv",header=T,na.strings="?")	# reading csvs is also easy
dim(Auto)						# dimensions of data frame
Auto[1:4,]						# subsetting data frame
Auto=na.omit(Auto)					# get rid of na’s
dim(Auto)
names(Auto)						# column names for the data frame

# Additional Graphical and Numerical Summaries (take a look through this if you like)

plot(cylinders, mpg)
plot(Auto$cylinders, Auto$mpg)
attach(Auto)
plot(cylinders, mpg)
cylinders=as.factor(cylinders)
plot(cylinders, mpg)
plot(cylinders, mpg, col="red")
plot(cylinders, mpg, col="red", varwidth=T)
plot(cylinders, mpg, col="red", varwidth=T,horizontal=T)
plot(cylinders, mpg, col="red", varwidth=T, xlab="cylinders", ylab="MPG")
hist(mpg)
hist(mpg,col=2)
hist(mpg,col=2,breaks=15)
pairs(Auto)
pairs(~ mpg + displacement + horsepower + weight + acceleration, Auto)
plot(horsepower,mpg)
identify(horsepower,mpg,name)
summary(Auto)
summary(mpg)