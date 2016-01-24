#############################
# < Ziwei Meng >
# STAT W4240 
# Homework <HW 06> , Problem <Problem 5>
# < Wednesday, December 9 >
#############################
#set work path
setwd("D:/Rworkspace")
#clear workspace
rm(list=ls())

###########################
#On the basis of this dissimilarity matrix, sketch the dendrogram that results from hierarchically clustering these four observations using complete linkage. Be sure to indicate on the plot the height at which each fusion occurs, as well as the observations corresponding to each leaf in the dendrogram.
D = matrix(c(0,0.3,0.4,0.7,0.3,0,0.5,0.8,0.4,0.5,0,0.45,0.7,0.8,0.45,0),4,4)
D = as.dist(D)
clus = hclust(D,method="complete")
plot(clus)

#Repeat (a), this time using simple linkage clustering.
clus2 = hclust(D, method = "single")
plot(clus2)

#It is mentioned in the chapter that at each fusion in the dendrogram, the position of the two clusters being fused can be swapped without changing the meaning of the dendrogram. Draw a dendrogram that is equivalent to the dendrogram in (a), for which two or more of the leaves are repositioned, but for which the meaning of the dendrogram is the same.
clus3 = hclust(D, method = "complete")
plot(clus3, labels = c(2,1,4,3))






