#############################
# < Ziwei Meng >
# STAT W4240 
# Homework <HW 03> , Problem <Problem 4>
# < Wednesday, October 21 >
#############################
#set up workpath
setwd("D:/forR/W4240/DataSets")
#clear workspace
rm(list=ls())

#Load the views P00A+000E+00, P00A+005E+10, P00A+005E-10, and P00A+010E+00 for all subjects in the CroppedYale directory. Convert each photo to a vector; store the collection as a matrix where each row is a photo. Give this matrix the name face matrix 6a. Record the subject number and view of each row of face matrix 6a in a data frame. The subject numbers will be used as our data labels.
#Load this library
library(pixmap)
# get directory structure
dir_list_1 = dir(path="CroppedYale/",all.files=FALSE)
dir_list_2 = dir(path="CroppedYale/",all.files=FALSE,recursive=TRUE)
pic_list <- c(1:length(dir_list_1))
view_list <- c('P00A+000E+00' , 'P00A+005E+10' , 'P00A+005E-10','p00A+010E+00')
face_matrix_6a = vector()
for ( i in 1:length(pic_list) ){
  for ( j in 1:length(view_list) ){
    this_filename = sprintf("CroppedYale/%s/%s_%s.pgm", dir_list_1[pic_list[i]] , dir_list_1[pic_list[i]] , view_list[j])
    this_face = read.pnm(file = this_filename)
    this_face_matrix = getChannels(this_face)
    this_face_vector = as.vector(this_face_matrix)
    face_matrix_6a = rbind( face_matrix_6a , this_face_vector )  
  }
}

#data frame for indices
faces.dat<- data.frame(matrix(c(1:152),4,38))
names(faces.dat)<-c(1:38)
row.names(faces.dat)<-view_list

#get training and test set
fm_6a_size = dim(face_matrix_6a)
#use 4/5 of the data for training, 1/5 for testing
ntrain_6a = floor(fm_6a_size[1]*4/5)
ntest_6a = fm_6a_size[1] - ntrain_6a
set.seed(1)
ind_train_6a = sample(1:fm_6a_size[1],ntrain_6a)
ind_test_6a = c(1:fm_6a_size[1])[-ind_train_6a]


#What are the first 5 files (rows) in the training set? What are the first 5 files in the testing set? Specify their subject and view indices.
Ind_subj<-function(ind,faces.dat){
  n = length(ind)
  ind_subj = matrix(c(rep(0,n*2)),n,2)
  for (i in c(1:n)){
    ind_subj[i,] = as.vector(which(faces.dat==ind[i],arr.ind=T))
  }  
  return(ind_subj)
}
indsubj_test_6a = Ind_subj(ind_test_6a,faces.dat)
indsubj_train_6a = Ind_subj(ind_train_6a,faces.dat)

indsubj_test_6a_5 = indsubj_test_6a[1:5,]
indsubj_train_6a_5 = indsubj_train_6a[1:5,]




#Do PCA on your training set and use the first 25 scores to represent your data. Specifically, create the mean face from the training set, subtract off the mean face, and run prcomp() on the resulting image matrix. Project your testing data onto the first 25 loadings so that it is also represented by the first 25 scores. Do not rescale the scores. Use 1NN classification in the space of the first 25 scores to identify the subject for each testing observation. In class we discussed doing kNN classification by majority vote of the neighbors; in the 1NN case, there is simply one vote. How many subjects are identified correctly? How many incorrectly? Plot any subject photos that are misidentified next to the 1NN photo prediction.
library(class)
kPCA <- function(face_matrix,ind_train,ind_test,subj_test,subj_train){
  face_train = face_matrix[ind_train,]
  face_test = face_matrix[ind_test,]
  face_mean = colMeans(face_train)
  
  face_pca = prcomp(face_train,center = face_mean)
  W = face_pca$rotation
  scores_train = face_pca$x[,1:25]
  scores_test = ((face_test - face_mean)%*%W)[,1:25]
  pred_subj_test = knn(scores_train,scores_test,subj_train)
  corr = (pred_subj_test==subj_test)
  ncorr = sum(corr)
  return(list(pred_subj_test,corr,ncorr))
}
subj_test_6a =  indsubj_test_6a[,2] 
subj_train_6a = indsubj_train_6a[,2]
kpca_6a = kPCA(face_matrix_6a,ind_train_6a,ind_test_6a,subj_test_6a,subj_train_6a)

plot(pixmapGrey(matrix(face_matrix_6a[12,],192,168)))
plot(pixmapGrey(matrix(face_matrix_6a[40,],192,168)))



#Rerun parts (a) and (b) using the views P00A-035E+15, P00A-050E+00, P00A+035E+15, and P00A+050E+00 for all subjects in the CroppedYale directory. Give this matrix the name face matrix 6c. For each image, record the subject number and view in a data frame.
getMatrix<-function(view_list){
  dir_list_1 = dir(path="CroppedYale/",all.files=FALSE)
  dir_list_2 = dir(path="CroppedYale/",all.files=FALSE,recursive=TRUE)
  pic_list <- c(1:length(dir_list_1))
  
  
  face_matrix = vector()
  for ( i in 1:length(pic_list) ){
    for ( j in 1:length(view_list) ){
      this_filename = sprintf("CroppedYale/%s/%s_%s.pgm", dir_list_1[pic_list[i]] , dir_list_1[pic_list[i]] , view_list[j])
      this_face = read.pnm(file = this_filename)
      this_face_matrix = getChannels(this_face)
      this_face_vector = as.vector(this_face_matrix)
      face_matrix = rbind( face_matrix , this_face_vector )  
    }
  }
  return(face_matrix)
}

view_list <- c('P00A-035E+15' , 'P00A-050E+00' , 'P00A+035E+15','p00A+050E+00')
face_matrix_6c = getMatrix(view_list)


#data frame for indices
getDataframe<-function(view_list){
  faces.dat<- data.frame(matrix(c(1:152),4,38))
  names(faces.dat)<-c(1:38)
  row.names(faces.dat)<-view_list
  return(faces.dat)
}
faces.dat_6c = getDataframe(view_list)


#get training and test set
divideSet<-function(face_matrix,sed){
  fm_size = dim(face_matrix)
  #use 4/5 of the data for training, 1/5 for testing
  ntrain = floor(fm_size[1]*4/5)
  ntest = fm_size[1] - ntrain
  set.seed(sed)
  ind_train = sample(1:fm_size[1],ntrain)
  ind_test = c(1:fm_size[1])[-ind_train]
  return(list(ind_train,ind_test))
}
ind_train_6c = divideSet(face_matrix_6c,2)[[1]]
ind_test_6c = divideSet(face_matrix_6c,2)[[2]]

subj_train_6c = Ind_subj(ind_train_6c,faces.dat_6c)[,2]
subj_test_6c = Ind_subj(ind_test_6c,faces.dat_6c)[,2]

kpca_6c = kPCA(face_matrix_6c,ind_train_6c,ind_test_6c,subj_test_6c,subj_train_6c)

plot(pixmapGrey(matrix(face_matrix_6c[3,],192,168)))
plot(pixmapGrey(matrix(face_matrix_6c[71,],192,168)))


#Rerun part (c) with 10 diffierent training and testing divides. Display the number of faces correctly identified and the number incorrectly identified for each. What do these numbers tell us?
Ncorr<-function(face_matrix,faces.dat,sed){
  ind_train = divideSet(face_matrix,sed)[[1]]
  ind_test = divideSet(face_matrix,sed)[[2]]
  
  subj_train = Ind_subj(ind_train,faces.dat)[,2]
  subj_test = Ind_subj(ind_test,faces.dat)[,2]
  
  ntest = dim(face_matrix)[1] - floor(dim(face_matrix)[1]*4/5)
  
  kpca = kPCA(face_matrix,ind_train,ind_test,subj_test,subj_train)
  ncorr = kpca[[3]]
  nincor = ntest - ncorr
  return(list(ncorr,nincor))
}

Corrate = matrix(c(rep(0,20)),10,2)
for (i in c(1:10)){
  for (j in c(1:2)){
    Corrate[i,j] = Ncorr(face_matrix_6c,faces.dat_6c,i)[[j]]
  }
}





