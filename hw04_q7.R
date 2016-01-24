#############################
# < Ziwei Meng >
# STAT W4240 
# Homework 04
# < Wednesday November 11 >
# <Problem 7>
# The following code analyzes the federalist papers
#############################

#################
# Setup
#################

# make sure R is in the proper working directory
# note that this will be a different path for every machine
setwd("D:/forR/W4240/DataSets")
#clear workspace
rm(list=ls())

############
##  a
############
library(ISLR)
attach(Auto)
mpg01 = as.numeric(mpg>=median(mpg))
Auto = data.frame(Auto,mpg01)

############
##   b
############
Auto$horsepower = as.numeric(Auto$horsepower)
all.vs.one.plot = function(Auto,mpg01){
  n = dim(Auto)[2]
  r = floor(sqrt(n))
  c = ceiling(n/r)
  par(mfrow=c(r,c))
  for (i in 1:n){
    plot(Auto[,i],mpg01)
  }
  par(mfrow=c(1,1))
}
all.vs.one.plot(Auto[,1:9],Auto[,10])
#(mpg),horsepower,weight,acceleration
par(mfrow=c(1,3))
boxplot(cylinders~mpg01,xlab='mpg01',ylab='cylinders')
boxplot(year~mpg01,xlab='mpg01',ylab='year')
boxplot(origin~mpg01,xlab='mpg01',ylab='origin')
par(mfrow=c(1,1))
#cylinders

#############
##    c
#############
ind.train = (year%%2 == 0)
ind.test = !ind.train
Auto.train = Auto[ind.train,]
Auto.test = Auto[ind.test,]
mpg01.test = mpg01[ind.test]



##############
##   d
##############
library(MASS)
m.lda = lda(mpg01~horsepower+weight+acceleration+cylinders,data=Auto,subset=ind.train)
pred.lda = predict(m.lda,Auto.test)
lda.err = mean(pred.lda$class != mpg01.test)
#12.09%

################
##  e
################
m.qda = qda(mpg01~horsepower+weight+acceleration+cylinders,data=Auto,subset=ind.train)
pred.qda = predict(m.qda,Auto.test)
qda.err = mean(pred.qda$class != mpg01.test)
#13.19%


################
##  f
################
m.glm = glm(mpg01~horsepower+weight+acceleration+cylinders,data=Auto,subset=ind.train,family=binomial)
p.glm = predict(m.glm,Auto.test,type='response')
pred.glm = as.numeric(p.glm > 0.5)
logis.err = mean(pred.glm != mpg01.test)
#12.09%


####################
##   g
####################
library(class)
Predictors = cbind(horsepower,weight,acceleration,cylinders)
train.set = Predictors[ind.train,]
test.set = Predictors[ind.test,]
train.mpg01 = mpg01[ind.train]

k.list = c(1,5,10,50,100)
set.seed(1)
knn.err = rep(0,5)
for (i in 1:5){
  pred.knn = knn(train.set,test.set,train.mpg01,k=k.list[i])
  knn.err[i] = mean(pred.knn!=mpg01.test)
  
}
which.min(knn.err)
#k=5