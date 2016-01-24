#############################
# < Ziwei Meng >
# STAT W4240 
# Homework <HW 03> , Problem <Problem 3>
# < Wednesday, October 21 >
#############################
#set up workpath
setwd("D:/forR/W4240/DataSets")
#clear workspace
rm(list=ls())

#Load the data set hw03_q3.csv
KNN<-read.csv("hw03_q3.csv")
KNN.X<-KNN[,c(1,2)]
KNN.y<-KNN[,c(3)]

#Use the function dist() to produce a matrix of distances between all pairs of points. Distances should be computed for the two-dimensional input points x = [x1, x2] (y is the output variable). Print the results.
X.dist<- as.matrix(dist(KNN.X))


#Use the first data point as the testing set and the rest of the data as a training set. Implement kNN regression using the distance matrix from (a) for k = 1, 2, ...,10. This algorithm should predict the y value of the first data point (with some error). Compute the mean squared error for the testing set and the mean squared error for the training set for each value of k; denote these values as MSE_test and MSE_train.
#function to compute estimated y
predY <- function(id,ind,k,X.dist,KNN.y ){
  train.dist <- X.dist[,c(-ind)]
  y.train <- KNN.y[c(-ind)]
  dis = train.dist[id,]
  indk = which(dis<=sort(dis)[k])
  yk = y.train[indk]
  y.pred = 1./k*sum(yk)
  return(y.pred)
}
#get MSE for fixed ind and k
MSE <- function(ind,k,X.dist,KNN.y){
  y.test = KNN.y[ind]
  y.train = KNN.y[c(-ind)]
  #predict test y
  y.pred.test = predY(ind,ind,k,X.dist,KNN.y)
  #predict train y 
  y.pred.train = c(rep(0,19))
  ind.train = c(1:20)[-c(ind)]
  for (i in c(1:19)){
    
      y.pred.train[i] = predY(ind.train[i],ind,k,X.dist,KNN.y)
    
  }
  #compute MSE
  error.test.sq <- (y.pred.test - y.test)^2
  error.train.sq <- (y.pred.train - y.train)^2
  test.MSE <- (error.test.sq)/1
  train.MSE <- sum(error.train.sq)/19
  return(list(test.MSE,train.MSE))
}

#compute MSEk,1
test.MSE <- c(rep(0,10))
train.MSE <- c(rep(0,10))
for (k in c(1:10)){
  test.MSE[k] = MSE(1,k,X.dist,KNN.y)[[1]]
  train.MSE[k] = MSE(1,k,X.dist,KNN.y)[[2]]
}


#Rerun part (b). For each data point: use the ith data point as a testing set, the remaining data as a training set, and run kNN for k = 1:10 for observations i = 2:n. compute MSEk for test and train set.
MSE.matrix.test<-matrix(c(rep(0,20*10)),20,10)
MSE.matrix.train<-matrix(c(rep(0,20*10)),20,10)
for (i in c(1:20)){
  for (k in c(1:10)){
    MSE.matrix.test[i,k] = MSE(i,k,X.dist,KNN.y)[[1]]
    MSE.matrix.train[i,k] = MSE(i,k,X.dist,KNN.y)[[2]]
  }
}
MSEk.test<-apply(MSE.matrix.test,2,mean)
MSEk.train<-apply(MSE.matrix.train,2,mean)











