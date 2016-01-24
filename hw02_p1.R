#############################
# < Ziwei Meng >
# STAT W4240 
# Homework <HW 02> , Problem <Problem 1>
# < Wednesday, October 7 >
#############################
#set up workpath
setwd("D:/forR/W4240/DataSets")
#clear workspace
rm(list=ls())

#Load hw02 q1 p1.csv.
q1<-read.csv("hw02_q1_p1.csv")
#Find the column means and the row means for the data.
colMeans(q1)
rowMeans(q1)

#Center the data and and find the empirical covariance matrix, ^SIGMA
X<-scale(q1,center=TRUE,scale=FALSE)
SIGMA<-1./dim(X)[1]*t(X)%*%X

#Give the eigenvalues and associated eigenvectors of SIGMA
evalues<-eigen(SIGMA)$values
evectors<-eigen(SIGMA)$vectors

#Give all of the loadings and all of the scores for the data.
#W represents loadings and Y represents scores
W<-evectors
Y<-X%*%W

#Plot the proportion of variance captured against the number of components included.
reservedVariance <- function(evalues){
  k = length(evalues)
  reVa<-rep(0,k)
  for(i in 1:k){
    reVa[i] =  sum(evalues[1:i])/sum(evalues)
  }
  return(reVa)
}
reVa<-reservedVariance(evalues)
comp<-1:5
plot(reVa~comp,xlab="Proportion of Variance",ylab="Number of Components",type='b',ylim=c(0.9,1))                                                                          

#Load hw02_q1_p2.csv.This has 5 new observations in the original coordinates. Using the loadings obtained in (d), give the scores of these new 5 observations. [Hint: center these                                                              new observations with respect to the dataset you loaded in a.]
p2<-read.csv("hw02_q1_p2.csv")
new<-scale(p2,center=colMeans(q1),scale=F)
Ynew<-new%*%W

#Now, from the scores obtained in f, use only the first two scores to represent the new 5 observations. What are the coordinates of the projections in the original space (call it x')? What is their Euclidean distance from the original data points?
Ynew2<-Ynew[,1:2]
#plot(Ynew2[,1]~Ynew2[,2])
Xrev<-Ynew2%*%t(W[,1:2])
dis<- as.matrix(sqrt(rowSums(Xrev-new)^2))

#In what direction is d(x', x) for the 5 new points?
D<-Xrev-new
D%*%W[,1:2]
