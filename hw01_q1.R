#############################
# < Ziwei Meng >
# STAT W4240 
# Homework <HW 01> , Problem <Problem 1>
# < Wednesday, September 23 >
#############################
#set work path
setwd("D:/forR/W4240/DataSets")
#clear workspace
rm(list=ls())

#question a
#load data
college<-read.csv("College.csv",header=T)

#question b
#set first column as row names
fix(college)
rownames(college)<-college[,1]
fix(college)
college<-college[,-1]
fix(college)

#summary each variable
summary(college)

#get pairs of first 10 columns
pairs(college[,1:10])

#Use the plot() function to produce side-by-side boxplots of Outstate versus Private.
plot(Outstate~Private,data=college)

#study Elite
Elite <- rep("No",nrow(college ))
Elite[college$Top10perc >50]=" Yes"
Elite <- as.factor (Elite)
college <- data.frame(college ,Elite)
summary(college$Elite)
plot(Outstate~Elite,data=college)

#study with hist
par(mfrow=c(2,2))
hist(college$Apps,breaks=seq(80,49000,700))
hist(college$Accept,breaks=seq(70,27000,500))
hist(college$Outstate,breaks=seq(2000,22000,1000))
hist(college$Expend,breaks=seq(3000,57000,2000))

#explore the data
pairs(college[,2:4])
par(mfrow=c(1,1))
plot(Top10perc~Top25perc,data=college)
plot(Outstate~Room.Board,data=college)
plot(Outstate~Top25perc,data=college)
plot(F.Undergrad ~Enroll,data=college)
