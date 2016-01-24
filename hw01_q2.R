#############################
# < Ziwei Meng >
# STAT W4240 
# Homework <HW 01> , Problem <Problem 2>
# < Wednesday, September 23 >
#############################
#set work path
setwd("D:/forR/W4240/DataSets")
#clear workspace
rm(list=ls())
#load data
auto <- read.csv("Auto.csv",header=T,na.string="?")
auto<-na.omit(auto)

#question a:Which of the predictors are quantitative, and which are qualitative?
type_list <- c(rep(0,length(names(auto))))
for ( i in 1:length(names(auto))){
  type_list[i]<-class(auto[,i])
}
print(type_list)
auto$origin<-as.factor(auto$origin)

#get range of each quantitative predictor
for (i in 1:7){
  print(range(auto[,i]))
}

#get mean and sd of each quantitative predictor
auto_matrix<-as.matrix(auto[1:7])
for (i in 1:7){
  print(mean(auto_matrix[,i]))
  print(sd(auto_matrix[,i]))
}

#Now remove the 10th through 85th observations. What is the range, mean, and standard deviation of each predictor in the subset of the data that remains?
auto_matrix_part<-auto_matrix[-c(10:85),]

for (i in 1:7){
  print(range(auto_matrix_part[,i]))
}
for (i in 1:7){
  print(mean(auto_matrix_part[,i]))
}
for (i in 1:7){
  print(sd(auto_matrix_part[,i]))
}

#explore data
plot(displacement~horsepower,data=auto)
lines(lowess(auto$displacement~auto$horsepower))
plot(displacement~weight,data=auto)
lines(lowess(auto$displacement~auto$weight))
plot(weight~horsepower,data=auto)
lines(lowess(auto$weight~auto$horsepower))

plot(mpg~horsepower,data=auto)
lines(lowess(auto$mpg~auto$horsepower))
plot(mpg~displacement,data=auto)
lines(lowess(auto$mpg~auto$displacement))
plot(mpg~weight,data=auto)
lines(lowess(auto$mpg~auto$weight))

