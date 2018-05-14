#Sourabh Rajput
#CS 513
#10431188

rm(list=ls())

dsn<-read.csv("C:/Users/rajpu/Desktop/KDDM/breast-cancer-wisconsin.data.csv")

dsn$Class <- factor(dsn$Class)
dsn$F1 <- factor(dsn$F1)
dsn$F2 <- factor(dsn$F2)
dsn$F3 <- factor(dsn$F3)
dsn$F4 <- factor(dsn$F4)
dsn$F5 <- factor(dsn$F5)
dsn$F6 <- factor(dsn$F6)
dsn$F7 <- factor(dsn$F7)
dsn$F8 <- factor(dsn$F8)
dsn$F9 <- factor(dsn$F9)
index<-sort(sample(nrow(dsn),round(.25*nrow(dsn))))


training<-dsn[-index,]

test<-dsn[index,]

#install.packages("C50", repos="http://R-Forge.R-project.org")

install.packages("C50")

library('C50')

#View(dsn)

#library(C50)

C50_class <- C5.0(Class~F1+F2+F3+F4+F5+F6+F7+F8+F9,data=training)

summary(C50_class )

plot(C50_class)

C50_predict<-predict( C50_class ,test , type="class" )

table(actual=test[,11],C50=C50_predict)

wrong<- (test[,11]!=C50_predict)

c50_rate<-sum(wrong)/length(test[,4])

c50_rate

accuracy <- (1-c50_rate)*100
accuracy

