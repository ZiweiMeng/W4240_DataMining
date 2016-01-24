#############################
# < Ziwei Meng >
# STAT W4240 
# Homework 04
# < Wednesday November 11 >
# <Problem 1-4>
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

###############do this for ONCE!##########
#get and create necessary directories
cleandir.create = function(paths){
  dir_list = dir(paths)
  for (i in 1:length(dir_list)){
    dirname = dir_list[i]
    path = sprintf('FederalistPapers/%s_clean',dirname)
    dir.create(path)
  }
  return(dir_list)
}
#do this for once!!!!!
fol = 'FederalistPapers'
dir_list = cleandir.create(fol )

for (i in 1:length(dir_list)){
  dir_list[i] = sprintf('FederalistPapers/%s',dir_list[i])
}
#######################


# first include the relevant libraries
# note that a loading error might mean that you have to
# install the package into your R distribution.
# Use the package installer and be sure to install all dependencies
library(tm)

#################
# Problem 1a
#################


###now do pre-work on each of the folders
###use functions in template(modified)
source('hw04.R')
cleancorpus = function(dir_list){
  for (i in 1:length(dir_list)){
    preprocess.directory(dir_list[i])
  }
}
cleancorpus(dir_list)


##########################################

#################
# Problem 1b
#################

##########################################
# To read in data from the directories:
# Partially based on code from C. Shalizi
read.directory <- function(dirname) {
  # Store the infiles in a list
  infiles = list();
  # Get a list of filenames in the directory
  filenames = dir(dirname,full.names=TRUE);
  for (i in 1:length(filenames)){
    infiles[[i]] = scan(filenames[i],what="",quiet=TRUE);
  }
  return(infiles)
}

hamilton.train = read.directory('FederalistPapers/fp_hamilton_train_clean')
hamilton.test = read.directory('FederalistPapers/fp_hamilton_test_clean')
madison.train = read.directory('FederalistPapers/fp_madison_train_clean')
madison.test = read.directory('FederalistPapers/fp_madison_test_clean')
##########################################

#################
# Problem 1c
#################

##########################################
# Make dictionary sorted by number of times a word appears in corpus 
# (useful for using commonly appearing words as factors)
# NOTE: Use the *entire* corpus: training, testing, spam and ham
make.sorted.dictionary.df <- function(infiles){
  # This returns a dataframe that is sorted by the number of times 
  # a word appears
  
  # List of vectors to one big vetor
  dictionary.full <- unlist(infiles) 
  # Tabulates the full dictionary
  tabulate.dic <- tabulate(factor(dictionary.full)) 
  # Find unique values
  dictionary <- unique(dictionary.full) 
  # Sort them alphabetically
  dictionary <- sort(dictionary)
  dictionary.df <- data.frame(word = dictionary, count = tabulate.dic)
  sort.dictionary.df <- dictionary.df[order(dictionary.df$count,decreasing=TRUE),];
  return(sort.dictionary.df)
}

full.list = c(hamilton.test,hamilton.train,madison.test,madison.train)
dict.full = make.sorted.dictionary.df(full.list)
##########################################

#################
# Problem 1d
#################

##########################################
# Make a document-term matrix, which counts the number of times each 
# dictionary element is used in a document
make.document.term.matrix <- function(infiles,dictionary){
  # This takes the text and dictionary objects from above and outputs a 
  # document term matrix
  num.infiles <- length(infiles);
  num.words <- nrow(dictionary);
  # Instantiate a matrix where rows are documents and columns are words
  dtm <- mat.or.vec(num.infiles,num.words); # A matrix filled with zeros
  for (i in 1:num.infiles){
    num.words.infile <- length(infiles[[i]]);
    infile.temp <- infiles[[i]];
    for (j in 1:num.words.infile){
      ind <- which(dictionary == infile.temp[j])[[1]];
      # print(sprintf('%s,%s', i , ind))
      dtm[i,ind] <- dtm[i,ind] + 1;
    }
  }
  return(dtm);
}

dtm.hamilton.train = make.document.term.matrix(hamilton.train,dict.full)
dtm.hamilton.test = make.document.term.matrix(hamilton.test,dict.full)
dtm.madison.train = make.document.term.matrix(madison.train,dict.full)
dtm.madison.test = make.document.term.matrix(madison.test,dict.full)
##########################################

#################
# Problem 1e
#################

##########################################
make.log.pvec <- function(dtm,mu){
  # Sum up the number of instances per word
  pvec.no.mu <- colSums(dtm)
  # Sum up number of words
  n.words <- sum(pvec.no.mu)
  # Get dictionary size
  dic.len <- length(pvec.no.mu)
  # Incorporate mu and normalize
  log.pvec <- log(pvec.no.mu + mu) - log(mu*dic.len + n.words)
  return(log.pvec)
}

D = dim(dict.full)[1]
mu = 1/D
make.logp.all <- function(dtm.hamilton.train,dtm.madison.train,mu){
  logp.hamilton.train = make.log.pvec(dtm.hamilton.train,mu)
  
  logp.madison.train = make.log.pvec(dtm.madison.train,mu)
  
  return(list(logp.hamilton.train,logp.madison.train))
} 
logp.hamilton.train = make.logp.all(dtm.hamilton.train,dtm.madison.train,mu)[[1]]

logp.madison.train = make.logp.all(dtm.hamilton.train,dtm.madison.train,mu)[[2]]

##########################################

####################
#  Problem 2
####################

################################################
naive.bayes <- function(logp.hamilton.train, logp.madison.train,log.prior.hamilton,log.prior.madison,dtm.test){
  n = dim(dtm.test)[1]
  logp.hamilton.train = as.matrix(logp.hamilton.train)
  logp.madison.train = as.matrix(logp.madison.train)
  log.prior.m = t(matrix(rep(c(log.prior.hamilton,log.prior.madison),n),2,n))
  logp.train = cbind(logp.hamilton.train,logp.madison.train)
  dtm.test.logp = dtm.test%*%logp.train+log.prior.m
  naive.class = as.matrix(as.numeric(dtm.test.logp[,1]>=dtm.test.logp[,2]))
    #'1' means hamilton and '0'means madison
  return(naive.class)
}
m1 = dim(dtm.hamilton.train)[1]
m2 = dim(dtm.madison.train)[1]
log.prior.hamilton = log(m1/(m1+m2))
log.prior.madison = log(m2/(m1+m2))

###############################################################

########################
#   Problem 3
########################

###############################################################

accuracy.repo = function(logp.hamilton.train, logp.madison.train,log.prior.hamilton,log.prior.madison,test.list){
  dtm.hamilton.test = test.list[[1]]
  dtm.madison.test = test.list[[2]]
  ham.test.class = naive.bayes(logp.hamilton.train, logp.madison.train,log.prior.hamilton,log.prior.madison,dtm.hamilton.test)
  madi.test.class = naive.bayes(logp.hamilton.train, logp.madison.train,log.prior.hamilton,log.prior.madison,dtm.madison.test)
  corr.rate = (sum(ham.test.class)+sum(1-madi.test.class))/(dim(ham.test.class)[1]+dim(madi.test.class)[1])
  #ham.corr.rate = sum(ham.test.class)/dim(ham.test.class)[1]
  #madi.corr.rate = sum(1-madi.test.class)/dim(madi.test.class)[1]
  tru.pos = sum(ham.test.class)/dim(ham.test.class)[1]
  tru.neg = sum(1-madi.test.class)/dim(madi.test.class)[1]
  fals.pos = 1 - tru.neg
  fals.neg = 1 - tru.pos
  repo = list(corr.rate,tru.pos,tru.neg,fals.pos,fals.neg)
  names(repo) = c('correct.rate','true.positive','true.negative','false.positive','false.negative')
  return(repo)
}

accru.repo = accuracy.repo(logp.hamilton.train, logp.madison.train,log.prior.hamilton,log.prior.madison,list(dtm.hamilton.test,dtm.madison.test))
accru.repo

#########################################################################

###################################
#    Problem 4
###################################

#########################################################################
divideSet<-function(dtm.hamilton.train,dtm.madison.train,sed){
  n.ham = dim(dtm.hamilton.train)[1]
  n.madi = dim(dtm.madison.train)[1]
  train.mat = rbind(dtm.hamilton.train,dtm.madison.train)
  
  #use 4/5 of the data for training, 1/5 for testing
  #ntrain.ham = floor(n.ham*4/5)
  #ntest.ham = n.ham - ntrain.ham
  #ntrain.madi = floor(n.madi*4/5)
  #ntest.madi = n.madi - ntrain.madi
  set.seed(sed)
  ind.train = sample(1:(n.ham+n.madi),0.8*(n.ham+n.madi))
  
  ind.train.ham = ind.train[ind.train<=n.ham]
  ind.train.madi = ind.train[ind.train>n.ham]-n.ham
  ind.test.ham = c(1:n.ham)[-ind.train.ham]
  ind.test.madi = c(1:n.madi)[-ind.train.madi]
  
  #ind.test.ham = c(1:n.ham)[-ind.train.ham]
  #ind.train.madi = sample(1:n.madi,ntrain.madi)
  #ind.test.madi = c(1:n.madi)[-ind.train.madi]
  return(list(ind.train.ham,ind.test.ham,ind.train.madi,ind.test.madi))
}

cv.accur.repo = function(dtm.hamilton.train,dtm.madison.train,sed,mu){
  ind.divi.set = divideSet(dtm.hamilton.train,dtm.madison.train,sed)
  cv.ham.train = dtm.hamilton.train[ind.divi.set[[1]],]
  cv.ham.test = dtm.hamilton.train[ind.divi.set[[2]],]
  cv.madi.train = dtm.madison.train[ind.divi.set[[3]],]
  cv.madi.test = dtm.madison.train[ind.divi.set[[4]],]
  m1 = dim(cv.ham.train)[1]
  m2 = dim(cv.madi.train)[1]
  log.prior.hamilton = log(m1/(m1+m2))
  log.prior.madison = log(m2/(m1+m2))
  test.list = list(cv.ham.test,cv.madi.test)
  logp.hamilton.train = make.logp.all(cv.ham.train,cv.madi.train,mu)[[1]]
  logp.madison.train = make.logp.all(cv.ham.train,cv.madi.train,mu)[[2]]
  accru.repo = accuracy.repo(logp.hamilton.train, logp.madison.train,log.prior.hamilton,log.prior.madison,test.list)
  return(accru.repo)
}

#D = dim(dict.full)[1]
#mu = 1/D
mu.list = c(0.1/D,1/D,10/D,100/D,1000/D)
seed.list = c(1,5,10,15,20)

cv.repo.mats = function(dtm.hamilton.train,dtm.madison.train,seed.list,mu.list){
  cv.corr.rate.mat = matrix(rep(0,25),5,5)
  cv.fals.neg.mat = matrix(rep(0,25),5,5)
  cv.fals.pos.mat = matrix(rep(0,25),5,5)
  for (i in 1:5){
    for (j in 1:5){
      seed = seed.list[[i]]
      mu = mu.list[[j]]
      cv.corr.rate.mat[i,j] = cv.accur.repo(dtm.hamilton.train,dtm.madison.train,seed,mu)$correct.rate
      cv.fals.neg.mat[i,j] = cv.accur.repo(dtm.hamilton.train,dtm.madison.train,seed,mu)$false.negative
      cv.fals.pos.mat[i,j] = cv.accur.repo(dtm.hamilton.train,dtm.madison.train,seed,mu)$false.positive
    }
  }
  three.repo = list(cv.corr.rate.mat,cv.fals.neg.mat,cv.fals.pos.mat)
  names(three.repo) = c('correct.rate','false.negative','false.positive')
  return(three.repo)
}
three.mats = cv.repo.mats(dtm.hamilton.train,dtm.madison.train,seed.list,mu.list)

plot.three.graphs = function(three.mats,mu.list){
  ave.cv.corr.rate = colMeans(three.mats[[1]])
  ave.cv.fals.neg = colMeans(three.mats[[2]])
  ave.cv.fals.pos = colMeans(three.mats[[3]])
  par(mfrow=c(1,3))
  plot(ave.cv.corr.rate~mu.list,xlab='mu',ylab='correct rate',type='b',lty=2,pch=17)
  plot(ave.cv.fals.neg~mu.list,xlab='mu',ylab='false negative',type='b',lty=2,pch=17)
  plot(ave.cv.fals.pos~mu.list,xlab='mu',ylab='false positive',type='b',lty=2,pch=17)
  par(mfrow=c(1,1))
}
plot.three.graphs(three.mats,mu.list)

#############################

#the best of mu seems to be the largest mu

###################################

full.accur.repo = function(dtm.hamilton.train,dtm.madison.train,dtm.hamilton.test,dtm.madison.test,mu){
  m1 = dim(dtm.hamilton.train)[1]
  m2 = dim(dtm.madison.train)[1]
  log.prior.hamilton = log(m1/(m1+m2))
  log.prior.madison = log(m2/(m1+m2))
  test.list = list(dtm.hamilton.test,dtm.madison.test)
  logp.hamilton.train = make.logp.all(dtm.hamilton.train,dtm.madison.train,mu)[[1]]
  logp.madison.train = make.logp.all(dtm.hamilton.train,dtm.madison.train,mu)[[2]]
  accru.repo = accuracy.repo(logp.hamilton.train, logp.madison.train,log.prior.hamilton,log.prior.madison,test.list)
  return(accru.repo)
}
full.three.mats = function(dtm.hamilton.train,dtm.madison.train,dtm.hamilton.test,dtm.madison.test,mu.list){
  corr.rate.vec = rep(0,length(mu.list))
  fals.neg.vec = rep(0,length(mu.list))
  fals.pos.vec = rep(0,length(mu.list))
  for (i in 1:length(mu.list)){
    corr.rate.vec[i] = full.accur.repo(dtm.hamilton.train,dtm.madison.train,dtm.hamilton.test,dtm.madison.test,mu.list[i])[[1]]
    fals.neg.vec[i] = full.accur.repo(dtm.hamilton.train,dtm.madison.train,dtm.hamilton.test,dtm.madison.test,mu.list[i])[[5]]
    fals.pos.vec[i] = full.accur.repo(dtm.hamilton.train,dtm.madison.train,dtm.hamilton.test,dtm.madison.test,mu.list[i])[[4]]
  }
  three.g = list(as.matrix(corr.rate.vec),as.matrix(fals.neg.vec),as.matrix(fals.pos.vec))
  names(three.g) = c('correct rate','false negative','false positive')
  return(three.g)
}

three.full = full.three.mats(dtm.hamilton.train,dtm.madison.train,dtm.hamilton.test,dtm.madison.test,mu.list)
#
par(mfrow=c(1,3))
plot(three.full[[1]]~mu.list,xlab='mu',ylab='correct rate',type='b',lty=2,pch=17)
plot(three.full[[2]]~mu.list,xlab='mu',ylab='false negative',type='b',lty=2,pch=17)
plot(three.full[[3]]~mu.list,xlab='mu',ylab='false positive',type='b',lty=2,pch=17)
par(mfrow=c(1,1))
#
###############################

cv.corr.rates = colMeans(three.mats[[1]])
cv.fals.neg = colMeans(three.mats[[2]])
cv.fals.pos = colMeans(three.mats[[3]])

dif.corr.rates = (cv.corr.rates-three.full[[1]])/three.full[[1]]
dif.fals.neg = (cv.fals.neg-three.full[[2]])/three.full[[2]]
dif.fals.neg[1:4] = 0
dif.fals.pos = (cv.fals.pos-three.full[[3]])/three.full[[3]]
dif.corr.rates
dif.fals.neg
dif.fals.pos
##############################
#################
# End of Script
#################


