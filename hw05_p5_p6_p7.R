#############################
# < Ziwei Meng >
# STAT W4240 
# Homework 04
# < Monday November 30 >
# <Problem 5-7>
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


##################################################
# implement codes in hw04_q1
##################################################
######################
#get two dataframes
ham.train = cbind(dtm.hamilton.train,as.matrix(rep(1,dim(dtm.hamilton.train)[1])))
mad.train = cbind(dtm.madison.train,as.matrix(rep(0,dim(dtm.madison.train)[1])))
ham.test = cbind(dtm.hamilton.test,as.matrix(rep(1,dim(dtm.hamilton.test)[1])))
mad.test = cbind(dtm.madison.test,as.matrix(rep(0,dim(dtm.madison.test)[1])))
X.train = data.frame(rbind(ham.train,mad.train))
X.test = data.frame(rbind(ham.test,mad.test))
names(X.train)[1:4875] = as.vector(dict.full$word)
names(X.train)[4876] = 'y'
names(X.test)[1:4875] = as.vector(dict.full$word)
names(X.test)[4876] = 'y'


#################################
### Problem 5
################################
################################
#tree with gini
################
library(rpart)
library(rpart.plot)
gini.fit = rpart(y~.,data=X.train,parms=list(split="gini"))
prp(gini.fit)
y.gini = predict(gini.fit,X.test[,-4876])
y.gini = as.numeric(y.gini>0.5)
#correct rate
sum(y.gini==X.test$y)/length(X.test$y)
#false negative rate
sum(as.numeric(y.gini[1:16]!=X.test$y[1:16]))/sum(as.numeric(y.gini==0))
#false positive rate
sum(as.numeric(y.gini[17:27]!=X.test$y[17:27]))/sum(as.numeric(y.gini==1))

################################
#tree with information
######################
info.fit = rpart(y~.,data=X.train,parms=list(split="information"))
prp(info.fit)
y.info = predict(info.fit,X.test[,-4876])
y.info = as.numeric(y.info>0.5)
#correct rate
sum(y.info==X.test$y)/length(X.test$y)
#false negative rate
sum(as.numeric(y.info[1:16]!=X.test$y[1:16]))/sum(as.numeric(y.info==0))
#false positive rate
sum(as.numeric(y.info[17:27]!=X.test$y[17:27]))/sum(as.numeric(y.info==1))

############################################################################


############################################################################
##################################
### Problem 6
##################################
x.train = cbind(scale(X.train[,1:4875]),X.train[,4876])
x.train[is.na(x.train)] = 0
colnames(x.train)[1:4875] = as.vector(dict.full$word)
colnames(x.train)[4876] = "y"
x.test = cbind(scale(X.test[,1:4875]),X.test[,4876])
x.test[is.na(x.test)] = 0
colnames(x.test)[1:4875] = as.vector(dict.full$word)
colnames(x.test)[4876] = "y"

##############
## Ridge #####
##################################
library(glmnet)
set.seed(1)
lambda.list = c(0.001,0.003,0.01,0.03,0.1,0.3,1,3,10)
ridge.fit = glmnet(x.train[,1:4875],x.train[,4876],alpha=0,lambda=lambda.list)
cv.ridge = cv.glmnet(x.train[,1:4875],x.train[,4876],alpha=0,lambda=lambda.list)
plot(cv.ridge)
bestlam = cv.ridge$lambda.min
ridge.pred = predict(ridge.fit,s=bestlam,newx=x.test[,1:4875])
y.ridge = as.numeric(ridge.pred>=0.5)
#correct rate
sum(y.ridge==x.test[,4876])/length(x.test[,4876])
#false negative rate
sum(as.numeric(y.ridge[1:16]!=x.test[1:16,4876]))/sum(as.numeric(y.ridge==0))
#false positive rate
sum(as.numeric(y.ridge[17:27]!=x.test[17:27,4876]))/sum(as.numeric(y.ridge==1))
ridge.coef = predict(ridge.fit,type="coefficients",s=bestlam)
rownames(ridge.coef)[sort(as.vector(ridge.coef),index.return=T,decreasing=T)$ix[1:10]]
#[1] "(Intercept)" "upon"        "sens"        "kind"        "easi"        "intend"     
[7] "afford"      "make"        "alway"       "calcul" 
ridge.coef[sort(as.vector(ridge.coef),index.return=T,decreasing=T)$ix[1:10]]
#[1] 0.700000000 0.003675316 0.002337117 0.002142435 0.002049872 0.001924922 0.001849706 [8] 0.001838927 0.001774191 0.001749666

###########################
#####lasso
#############################
set.seed(1)
lambda.list = c(0.001,0.003,0.01,0.03,0.1,0.3,1,3,10)
lasso.fit = glmnet(x.train[,1:4875],x.train[,4876],alpha=1,lambda=lambda.list)
cv.lasso = cv.glmnet(x.train[,1:4875],x.train[,4876],alpha=1,lambda=lambda.list)
plot(cv.lasso)
bestlam = cv.ridge$lambda.min
lasso.pred = predict(lasso.fit,s=bestlam,newx=x.test[,1:4875])
y.lasso = as.numeric(lasso.pred>=0.5)
#correct rate
sum(y.lasso==x.test[,4876])/length(x.test[,4876])
#false negative rate
sum(as.numeric(y.lasso[1:16]!=x.test[1:16,4876]))/sum(as.numeric(y.lasso==0))
#false positive rate
sum(as.numeric(y.lasso[17:27]!=x.test[17:27,4876]))/sum(as.numeric(y.lasso==1))
lasso.coef = predict(lasso.fit,type="coefficients",s=bestlam)
rownames(lasso.coef)[sort(as.vector(lasso.coef),index.return=T,decreasing=T)$ix[1:10]]
#[1] "(Intercept)" "sens"        "safe"        "argu"        "upon"        "ground"     
[7] "matter"      "make"        "mere"        "judicatur" 
lasso.coef[sort(as.vector(lasso.coef),index.return=T,decreasing=T)$ix[1:10]]
#[1] 0.700000000 0.025724651 0.015675319 0.015319466 0.009574332 0.009043811 0.007696787 [8] 0.007612432 0.006665943 0.006255554




########################################################
############## Problem 7
#########################################################
#####################
### mutual information
######################
library(infotheo)
X = rbind(X.train[,1:4875],X.test[,1:4875])
colnames(X) = as.vector(dict.full$word)
y = c(X.train[,4876],X.test[,4876])
Info.list = rep(0,dim(X.tra)[2])
for (i in 1:4875){
  Info.list[i] = mutinformation(X[,i],y)
}

###############different n##############

n = c(200,500,1000,2500)

X1.train = X.train[,c(sort(Info.list,decreasing=T,index.return=T)$ix[1:200],4876)]
X2.train = X.train[,c(sort(Info.list,decreasing=T,index.return=T)$ix[1:500],4876)]
X3.train = X.train[,c(sort(Info.list,decreasing=T,index.return=T)$ix[1:1000],4876)]
X4.train = X.train[,c(sort(Info.list,decreasing=T,index.return=T)$ix[1:2500],4876)]
X.train.list = list(X1.train,X2.train,X3.train,X4.train)
X1.test = X.test[,c(sort(Info.list,decreasing=T,index.return=T)$ix[1:200],4876)]
X2.test = X.test[,c(sort(Info.list,decreasing=T,index.return=T)$ix[1:500],4876)]
X3.test = X.test[,c(sort(Info.list,decreasing=T,index.return=T)$ix[1:1000],4876)]
X4.test = X.test[,c(sort(Info.list,decreasing=T,index.return=T)$ix[1:2500],4876)]
X.test.list = list(X1.test,X2.test,X3.test,X4.test)


###################
#gini 
###################

Performance = matrix(rep(0,48),16,3)
colnames(Performance) = c("correct rate","false negative","false positive")
for (i in 1:4){
  X.train = X.train.list[[i]]
  X.test = X.test.list[[i]]
  gini.fit = rpart(y~.,data=X.train,parms=list(split="gini"))
  #prp(gini.fit)
  y.gini = predict(gini.fit,X.test[,-4876])
  y.gini = as.numeric(y.gini>0.5)
  #correct rate
  Performance[i,1] = sum(y.gini==X.test$y)/length(X.test$y)
  #false negative rate
  Performance[i,2] = sum(as.numeric(y.gini[1:16]!=X.test$y[1:16]))/sum(as.numeric(y.gini==0))
  #false positive rate
  Performance[i,3] = sum(as.numeric(y.gini[17:27]!=X.test$y[17:27]))/sum(as.numeric(y.gini==1))
}


####################
#info split
#####################
for (i in 1:4){
  X.train = X.train.list[[i]]
  X.test = X.test.list[[i]]
  info.fit = rpart(y~.,data=X.train,parms=list(split="information"))
  prp(info.fit)
  y.info = predict(info.fit,X.test[,-4876])
  y.info = as.numeric(y.info>0.5)
  #correct rate
  Performance[i+4,1] = sum(y.info==X.test$y)/length(X.test$y)
  #false negative rate
  Performance[i+4,2] = sum(as.numeric(y.info[1:16]!=X.test$y[1:16]))/sum(as.numeric(y.info==0))
  #false positive rate
  Performance[i+4,3] = sum(as.numeric(y.info[17:27]!=X.test$y[17:27]))/sum(as.numeric(y.info==1))
}



###################################
### ridge 
####################################
for (i in 1:4){
  X.train = X.train.list[[i]]
  X.test = X.test.list[[i]]
  X.train = as.matrix(X.train)
  X.test = as.matrix(X.test)
  n = dim(X.train)[2]
  set.seed(1)
  lambda.list = c(0.001,0.003,0.01,0.03,0.1,0.3,1,3,10)
  c = n-1
  ridge.fit = glmnet(X.train[,1:c],X.train[,n],alpha=0,lambda=lambda.list)
  cv.ridge = cv.glmnet(X.train[,1:(n-1)],X.train[,n],alpha=0,lambda=lambda.list)
  #plot(cv.ridge)
  bestlam = cv.ridge$lambda.min
  ridge.pred = predict(ridge.fit,s=bestlam,newx=X.test[,1:(n-1)])
  y.ridge = as.numeric(ridge.pred>=0.5)
  #correct rate
  Performance[i+8,1] = sum(y.ridge==X.test[,n])/length(X.test[,n])
  #false negative rate
  Performance[i+8,2] = sum(as.numeric(y.ridge[1:16]!=X.test[1:16,n]))/sum(as.numeric(y.ridge==0))
  #false positive rate
  Performance[i+8,3] = sum(as.numeric(y.ridge[17:27]!=X.test[17:27,n]))/sum(as.numeric(y.ridge==1))
}




###################################
###lasso
###################################
for (i in 1:4){
  X.train = X.train.list[[i]]
  X.test = X.test.list[[i]]
  X.train = as.matrix(X.train)
  X.test = as.matrix(X.test)
  n = dim(X.train)[2]
  set.seed(1)
  lambda.list = c(0.001,0.003,0.01,0.03,0.1,0.3,1,3,10)
  lasso.fit = glmnet(X.train[,1:(n-1)],X.train[,n],alpha=1,lambda=lambda.list)
  cv.lasso = cv.glmnet(X.train[,1:(n-1)],X.train[,n],alpha=1,lambda=lambda.list)
  #plot(cv.lasso)
  bestlam = cv.ridge$lambda.min
  lasso.pred = predict(lasso.fit,s=bestlam,newx=X.test[,1:(n-1)])
  y.lasso = as.numeric(lasso.pred>=0.5)
  #correct rate
  Performance[i+12,1] = sum(y.lasso==X.test[,n])/length(X.test[,n])
  #false negative rate
  Performance[i+12,2] = sum(as.numeric(y.lasso[1:16]!=X.test[1:16,n]))/sum(as.numeric(y.lasso==0))
  #false positive rate
  Performance[i+12,3] = sum(as.numeric(y.lasso[17:27]!=X.test[17:27,n]))/sum(as.numeric(y.lasso==1))
}

n.list = c(200,500,1000,2500)

plot(n.list,Performance[1:4,1],main="Correct Rate",xlab="n",ylab="correct rate",type="b")
lines(n.list,Performance[5:8,1],type="b",lty=2,col="red")
lines(n.list,Performance[9:12,1],type="b",col="blue")
lines(n.list,Performance[13:16,1],type="b",lty=2,col="green")
legend(1900,1.35,c("Gini","Information","Ridge","Lasso"),lty=c(1,2,1,2),col=c("black","red","blue","green"))


plot(n.list,Performance[1:4,2],main="False Negative",xlab="n",ylab="false negative",type="b")
lines(n.list,Performance[5:8,2],type="b",lty=2,col="red")
lines(n.list,Performance[9:12,2],type="b",col="blue")
lines(n.list,Performance[13:16,2],type="b",lty=2,col="green")
legend(1900,0.95,c("Gini","Information","Ridge","Lasso"),lty=c(1,2,1,2),col=c("black","red","blue","green"))

plot(n.list,Performance[1:4,3],main="False Positive",xlab="n",ylab="false positive",type="b",ylim=c(0,0.15))
lines(n.list,Performance[5:8,3],type="b",lty=2,col="red")
lines(n.list,Performance[9:12,3],type="b",col="blue")
lines(n.list,Performance[13:16,3],type="b",lty=2,col="green")
legend(1900,0.05,c("Gini","Information","Ridge","Lasso"),lty=c(1,2,1,2),col=c("black","red","blue","green"),)


