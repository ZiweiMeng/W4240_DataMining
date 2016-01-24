#############################
# < Ziwei Meng >
# STAT W4240 
# Homework <HW 06> , Problem <Problem 6>
# < Wednesday, December 9 >
#############################
#set work path
setwd("D:/Rworkspace")
#clear workspace
rm(list=ls())

#####################
X = matrix(c(1,1,0,5,6,4,4,3,4,1,2,0),6,2)
plot(X[,1],X[,2],pch=4)


########################
set.seed(59)
label = sample(2, nrow(X), replace = T)
label

pchl = 3*label - 2
plot(X[,1], X[,2], pch = pchl, cex = 2)

#######################

c1 = c(mean(X[label == 1, 1]), mean(X[label == 1, 2]))
c2 = c(mean(X[label == 2, 1]), mean(X[label == 2, 2]))
plot(X[,1], X[,2], pch = pchl, cex = 2)
points(c1[1], c1[2], col = "red", pch = 1,cex=2.2)
points(c2[1], c2[2], col = "red", pch = 4,cex=2.2)

#######################################


label1 = c(1, 1, 1, 2, 2, 2)
plot(X[,1], X[,2], pch = (3*label1-2), cex = 2)
points(c1[1], c1[2], col = "red", pch = 1,cex=2.5)
points(c2[1], c2[2], col = "red", pch = 4,cex=2.5)

########################################


c3 = c(mean(X[label1 == 1, 1]), mean(X[label1 == 1, 2]))
c4 = c(mean(X[label1 == 2, 1]), mean(X[label1 == 2, 2]))
plot(X[,1], X[,2], pch = (3*label1-2), cex = 2)
points(c3[1], c3[2], col = "red", pch = 1,cex=2.2)
points(c4[1], c4[2], col = "red", pch = 4,cex=2.2)
#########################################


plot(X[,1], X[,2], pch = (3*label1-2), col = (2*label1), cex = 2)


