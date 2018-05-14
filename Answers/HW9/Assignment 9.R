#Sourabh Rajput
#KDDM
#10431188


rm(list=ls())

#loading iris data

data("iris")

View(iris)

#selecting every 5th element

index <- seq (5,nrow(iris),by=5)

#test and training data

test<-iris[index,]


train<-iris[-index,]

library("e1071")

#SVM


svm_model <- svm(Species ~ ., data=train)
summary(svm_model)

pred <- predict(svm_model,test)

wrong<- (test[,5]!=pred)

svm1<-sum(wrong)/length(test[,5])
svm1

accuracy <- (1-svm1)*100
accuracy

