#############################
# < Ziwei Meng >
# STAT W4240 
# Homework 05
# < Monday November 30 >
#
#############################

#################
# Setup
#################

# make sure R is in the proper working directory
# note that this will be a different path for every machine
setwd("D:/forR/W4240/DataSets")
#clear workspace
rm(list=ls())
#load data
library(ISLR)
data("Hitters")
library(gbm)

#(a) Remove the observations for whom the salary information is unknown, and then log-transform the salaries.
Hitters = na.omit(Hitters)
Hitters$Salary = log(Hitters$Salary)



#(b) Create a training set consisting of the first 200 observations, and a test set consisting of the remaining observations.
Hitters.train = Hitters[1:200,]
Hitters.test = Hitters[201:263,]
Salary.train = Hitters$Salary[1:200]
Salary.test = Hitters$Salary[201:263]


#(c) Perform boosting on the training set with 1,000 trees for a range of values of the shrinkage parameter ¦Ë. Produce a plot with different shrinkage values on the x-axis and the corresponding training set MSE on the y-axis.
lambda.list = c(0.0003,0.001,0.003,0.01,0.03,0.1,0.3)
MSE.train = c(rep(-1,7))
MSE.test = c(rep(-1,7))
for (i in 1:7){
  set.seed(1)
  boost.hitter = gbm(Salary~.,data=Hitters.train,distribution="gaussian",n.trees=1000,shrinkage=lambda.list[i])
  yhat.train = predict(boost.hitter,newdata=Hitters.train,n.trees=1000)
  yhat.test = predict(boost.hitter,newdata=Hitters.test,n.trees=1000)
  MSE.train[i] = mean((yhat.train-Salary.train)^2)
  MSE.test[i] = mean((yhat.test-Salary.test)^2)
}
plot(lambda.list,MSE.train,type="b")

#(d) Produce a plot with different shrinkage values on the x-axis and the corresponding test set MSE on the y-axis.
plot(lambda.list,MSE.test,type="b")

#(e) Compare the test MSE of boosting to the test MSE that results from applying two of the regression approaches seen in Chapters 3 and 6.
min(MSE.test)
#linear regression
library(glmnet)
linear.Hitter = lm(Salary~.,data=Hitters.train)
linear.pred = predict(linear.Hitter,Hitters.test)
mean((linear.pred-Salary.test)^2)

#lasso
X.train = model.matrix(Salary~.,data=Hitters.train)
X.test = model.matrix(Salary~.,data=Hitters.test)
lasso.Hitter = glmnet(X.train,Salary.train,alpha=1)
lasso.pred = predict(lasso.Hitter,s=0.01,newx=X.test)
mean((lasso.pred-Salary.test)^2)



#(f) Which variables appear to be the most important predictors in the boosted model?
lambda = lambda.list[which.min(MSE.test)]
boost.hitter = gbm(Salary~.,data=Hitters.train,distribution = "gaussian", n.trees = 1000, shrinkage = lambda)
summary(boost.hitter)


#(g) Now apply bagging to the training set. What is the test set MSE for this approach?
library(randomForest)
set.seed(1)
bagging.hitter = randomForest(Salary ~ ., data = Hitters.train, mtry = 19, ntree = 1000)
yhat.bagging = predict(bagging.hitter, newdata = Hitters.test)
mean((yhat.bagging - Salary.test)^2)
