#############################
# < Ziwei Meng >
# STAT W4240 
# Homework <HW 02> , Problem <Problem 2>
# < Wednesday, October 7 >
#############################
#set up workpath
setwd("D:/forR/W4240/DataSets")
#clear workspace
rm(list=ls())

#Load this library
library(pixmap)

#Load the views P00A+000E+00, P00A+005E+10, P00A+005E-10, and P00A+010E+00 for all subjects. Convert each photo to a matrix (using getChannels) and then to a vector; store the collection as a matrix where each row is a photo.
#What is the size of this matrix?
# get directory structure
dir_list_1 = dir(path="CroppedYale/",all.files=FALSE)
dir_list_2 = dir(path="CroppedYale/",all.files=FALSE,recursive=TRUE)

pic_list <- c(1:length(dir_list_1))
view_list <- c('P00A+000E+00' , 'P00A+005E+10' , 'P00A+005E-10','p00A+010E+00')
faces_matrix = vector()
for ( i in 1:length(pic_list) ){
  # initialize an empty row of faces data
  # inner loop over views
  for ( j in 1:length(view_list) ){
    # compile the correct file name
    # note that dir_list_1[pic_list[2]] should be "yaleB17" if pic_list[2] is B17
    this_filename = sprintf("CroppedYale/%s/%s_%s.pgm", dir_list_1[pic_list[i]] , dir_list_1[pic_list[i]] , view_list[j])
    # you can print out each name to help debug the code
    #print(this_filename)
    # load the data
    this_face = read.pnm(file = this_filename)
    this_face_matrix = getChannels(this_face)
    #convert matrix to vector
    this_face_vector = as.vector(this_face_matrix)
   # append the vector to the face_matrix
    faces_matrix = rbind( faces_matrix , this_face_vector )	
  }
}
dim(faces_matrix)

#Compute a "mean face", which is the average for each pixel across all of the faces. Display the mean face as a photo in the original size and save a copy as .png.
mean_face_vector = colMeans(faces_matrix)
mean_face_matrix = matrix(mean_face_vector,192,168)
mean_face = pixmapGrey(mean_face_matrix)
plot(mean_face)
filename = 'mean_face.png'
dev.copy(device=png, file=filename, height=600, width=800)
dev.off()

#Subtract "mean faces" off each of the faces. Then, use prcomp() to find the principal components of your image matrix. Plot the number of components on the x-axis against the proportion of the variance explained on the y-axis.
faces_adjust = scale(faces_matrix,center=mean_face_vector,scale = FALSE)
faces_comps<-prcomp(faces_adjust)
ncomp = 1:dim(faces_matrix)[1]
faces_eigenvalues<-faces_comps$sdev^2
ExplainedVariance <- function(evalues){
  k = length(evalues)
  reVa<-rep(0,k)
  for(i in 1:k){
    reVa[i] =  sum(evalues[1:i])/sum(evalues)
  }
  return(reVa)
}
ExVa<-ExplainedVariance(faces_eigenvalues)
plot(ExVa~ncomp,xlab="Proportion of Explained Variance",ylab="Number of Components",type='b')

#Each principal component is a picture, which are called "eigenfaces." Display the first 9 eigenfaces in a 3-by-3 grid. What image components does each describe? (Note: pixmapGrey()                        is fairly                                                                         exible and will automatically rescale data to have min 0 and max 1. You can do this                                                                        manually or allow pixmapGrey() to do it.)
index = t(matrix(c(1:9),3,3))
eigenfaces9_matrix<-faces_comps$rotation[,1:9]
reshape_9<- vector()
for (i in 1:3){
  this_row = vector()
  for (j in 1:3){
    this_eigenface = matrix(eigenfaces9_matrix[,index[i,j]],192,168)
    this_row = cbind(this_row,this_eigenface)
  }
  reshape_9 = rbind(reshape_9,this_row)
}
eigenfaces_9 = pixmapGrey(reshape_9)
plot(eigenfaces_9)

#Use the eigenfaces to reconstruct yaleB05 P00A+010E+00.pgm. #Starting with the mean face, add in one eigenface at a time until you reach 24 eigenfaces. Save the results in a 5-by-5 grid.
p = length(mean_face_vector)
scores = faces_comps$x[20,]
reconstr_25faces_matrix = matrix(rep(0,25*p),25,p)
reconstr_25faces_matrix[1,] = mean_face_vector
for (i in 2:25){
  reconstr_25faces_matrix[i,] = mean_face_vector+scores[1:i-1]%*%t(faces_comps$rotation[,1:i-1])
 
}
index = t(matrix(c(1:25),5,5))
reshape_matrix = vector()
for (i in 1:5){
  this_row = vector()
  for (j in 1:5){
    mixed_eigenface = matrix(reconstr_25faces_matrix[index[i,j],],192,168)
    this_row = cbind(this_row,mixed_eigenface)
  }
  reshape_matrix = rbind(reshape_matrix,this_row)
}
reshape25 = pixmapGrey(reshape_matrix)
plot(reshape25)
#Again, starting with the mean face, add in five eigenfaces at a time until you reach 120 eigenfaces. Save the results in a 5-by-5 grid.
matrix_120 = matrix(rep(0,25*p),25,p)
matrix_120[1,] =mean_face_vector
scores = faces_comps$x[20,]
for (i in 2:25){
  matrix_120[i,] = mean_face_vector + scores[1:(5*(i-1))]%*%t(faces_comps$rotation[,1:(5*(i-1))])
}

#
index = t(matrix(c(1:25),5,5))
reshape_matrix2 = vector()
for (i in 1:5){
  this_row = vector()
  for (j in 1:5){
    mixed_eigenface = matrix(matrix_120[index[i,j],],192,168)
    this_row = cbind(this_row,mixed_eigenface)
  }
  reshape_matrix2 = rbind(reshape_matrix2,this_row)
}
reshape120 = pixmapGrey(reshape_matrix2)
plot(reshape120)

#Remove the pictures of subject 01 from your image matrix (there should be four pictures of him) and recenter the data. Rerun prcomp() to get new principal components. Use these to reconstruct yaleB01 P00A+010E+00.pgm. Do this by subtracting off the mean face and projecting the remaining image onto the principal components.
this_face = faces_matrix[4,]
new_faces_matrix = faces_matrix[-c(1:4),]
cnew_faces_matrix = scale(new_faces_matrix,center=TRUE,scale=FALSE)
new_prcomp = prcomp(cnew_faces_matrix)
#new_eigenvalues = new_prcomp$sdev^2
new_eigenvectors = new_prcomp$rotation
#new_scores = new_prcomp$x
new_mean_face = colMeans(new_faces_matrix)
this_reconstr = (this_face - new_mean_face)%*%new_eigenvectors%*%t(new_eigenvectors) + new_mean_face
plot(pixmapGrey(matrix(this_reconstr,192,168)))
plot(pixmapGrey(matrix(faces_matrix[4,],192,168)))


