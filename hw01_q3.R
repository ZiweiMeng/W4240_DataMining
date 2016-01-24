#############################
# < Ziwei Meng >
# STAT W4240 
# Homework <HW 01> , Problem <Problem 3>
# < Wednesday, September 23 >
#############################
#set work path
setwd("D:/forR/W4240/DataSets")
#clear workspace
rm(list=ls())

#question a,load data
library(MASS)
Boston
?Boston
dim(Boston)

#explore data
pairs(Boston)
plot(nox~dis,data=Boston)
lines(lowess(Boston$nox~Boston$dis))
plot(medv~rm,data=Boston)
lines(lowess(Boston$medv~Boston$rm))
plot(lstat~rm,data=Boston)
lines(lowess(Boston$lstat~Boston$rm))

#does crime rate link to any predictors?
plot(crim~medv,data=Boston)
lines(lowess(Boston$crim~Boston$medv))
plot(crim~lstat,data=Boston)
lines(lowess(Boston$crim~Boston$lstat))


#Do any of the suburbs of Boston appear to have particularlyhigh crime rates? Tax rates? Pupil-teacher ratios?
summary(Boston$crim)
summary(Boston$tax)
summary(Boston$ptratio)

#How many of the suburbs in this data set bound the Charles river?
sum(Boston$chas)

#What is the median pupil-teacher ratio among the towns in this data set?
median(Boston$ptratio)

#Which suburb of Boston has lowest median value of owneroccupied homes? What are the values of the other predictors for that suburb, and how do those values compare to the overall ranges for those predictors?
ind<-which(Boston$medv==min(Boston$medv))
Boston[ind,]
summary(Boston)

#In this data set, how many of the suburbs average more than seven rooms per dwelling? More than eight rooms per dwelling?
length(Boston$rm[Boston$rm>7])
length(Boston$rm[Boston$rm>8])


