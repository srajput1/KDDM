#Sourabh Rajput 
#homework 3
#10431188




rm(list=ls())

#Question 2

#reading the csv data
ReadCancerFile <- read.csv("C:/Users/rajpu/Desktop/KDDM/Homework 3/breast-cancer-wisconsin.data.csv")

ReadCancerFile[ReadCancerFile=='?'] <- NA
ReadCancerFile

#deleting the empty value rows
c1 <- na.omit(ReadCancerFile)

#selecting 1st row and every 5th row
TestData <- c1[c(T,F,F,F,F),]

#selecting rest of data
TrainDeta <- c1[c(F,T,T,T,T),]


#using knn=1
k1 <- knn(TrainDeta[,2:10], TestData[,2:10], TrainDeta[,11], k=1)
k1

#prediction for knn=1
table1 <- table(Prediction=k1, Actual=TestData[,11])

accuracy1 <- (sum(diag(table1)))/sum(table1)

error1 <-(1-accuracy1)*100

error1

#using knn=2
k2 <- knn(TrainDeta[,2:10], TestData[,2:10], TrainDeta[,11], k=2)
k2

#prediction for knn=2
table2 <- table(Prediction=k2, Actual=TestData[,11])

accuracy2 <- (sum(diag(table2)))/sum(table2)

error2 <-(1-accuracy2)*100

error2

#using knn=5
k5 <- knn(TrainDeta[,2:10], TestData[,2:10], TrainDeta[,11], k=5)
k5

#prediction for knn=1
table5 <- table(Prediction=k5, Actual=TestData[,11])

accuracy5 <- (sum(diag(table5)))/sum(table5)

error5 <-(1-accuracy5)*100

error5


 #using knn=10
k10 <- knn(TrainDeta[,2:10], TestData[,2:10], TrainDeta[,11], k=10)
k10

#prediction for knn=10
table10 <- table(Prediction=k10, Actual=TestData[,11])

accuracy10 <- (sum(diag(table10)))/sum(table10)
error10 <-(1-accuracy10)*100
error10
