#############################
# < Ziwei Meng >
# STAT W4240 
# Homework <HW 01> , Problem <Problem 4>
# < Wednesday, September 23 >
#
# The following code loads the eigenfaces data and
# performs a set of simple loading and plotting functions
#############################

#################
# Setup
#################

# make sure R is in the proper working directory
# note that this will be a different path for every machine
setwd("D:/forR/W4240/DataSets")

# first include the relevant libraries
# note that a loading error might mean that you have to
# install the package into your R distribution.  From the
# command line, type install.packages("pixmap")
library(pixmap)

#################
# Problem 1a
#################

# paste or type in the given code here
face_01 = read.pnm(file = "CroppedYale/yaleB01/yaleB01_P00A-005E+10.pgm")

# now plot the data
plot(face_01)
# give it a nice title
title('hw01_01a: the first face')
# save the result
filename = 'hw01_01a.png'
dev.copy(device=png, file=filename, height=600, width=800)
dev.off()

# extract the class and size

#----- START YOUR CODE BLOCK HERE -----#
class(face_01)
face_01

#----- END YOUR CODE BLOCK HERE -----#

#################
# Problem 1b
#################

# make face_01 into a matrix with the given command
face_01_matrix = getChannels(face_01)

# load a second face
face_02 = read.pnm(file = "CroppedYale/yaleB02/yaleB02_P00A-005E+10.pgm")
face_02_matrix = getChannels(face_02)

# combine two faces into a single data matrix and make that a pixmap
faces_matrix = cbind( face_01_matrix , face_02_matrix )
faces = pixmapGrey( faces_matrix )

# plot to verify
plot(faces)

# find min and max values 

#----- START YOUR CODE BLOCK HERE -----#
min(faces_matrix)
max(faces_matrix)

#----- END YOUR CODE BLOCK HERE -----#

#################
# Problem 1c
#################

# get directory structure
dir_list_1 = dir(path="CroppedYale/",all.files=FALSE)
dir_list_2 = dir(path="CroppedYale/",all.files=FALSE,recursive=TRUE)

# find lengths

#----- START YOUR CODE BLOCK HERE -----#
length(dir_list_1)
length(dir_list_2)

#----- END YOUR CODE BLOCK HERE -----#

#################
# Problem 1d
#################

# the list of pictures (note the absence of 14 means that 31 corresponds to yaleB32)
pic_list = c( 05 , 11 , 31 )
view_list = c(  'P00A-005E+10' , 'P00A-005E-10' , 'P00A-010E+00')

# preallocate an empty list
pic_data = vector("list",length(pic_list)*length(view_list))
# initialize an empty matrix of faces data
faces_matrix = vector()

#----- START YOUR CODE BLOCK HERE -----#
#get a list to contain three submatrices
l_faces <- list()
for (i in 1:3){
  l_faces[[i]]<-vector()
}

#get three submatrices by cbind
for (i in 1:3){
  for (j in 1:3){
    filename=sprintf("CroppedYale/%s/%s_%s.pgm",dir_list_1[pic_list[i]],dir_list_1[pic_list[i]],view_list[j])
    face = read.pnm(file = filename)
    face_matrix = getChannels(face)
    l_faces[[i]] <- cbind( l_faces[[i]] , face_matrix )
  }
}

#get the whole matrix by rbind three submatrices
for (i in 1:3){
  faces_matrix <- rbind(faces_matrix,l_faces[[i]]) 
}
#----- END YOUR CODE BLOCK HERE -----#

# now faces_matrix has been built properly.  plot and save it.
faces = pixmapGrey(faces_matrix)
plot(faces)
# give it a nice title
title('hw01_01d: 3x3 grid of faces')
# save the result
filename = 'hw01_01d.png'
dev.copy(device=png, file=filename, height=600, width=800)
dev.off()

#################
# End of Script
#################



