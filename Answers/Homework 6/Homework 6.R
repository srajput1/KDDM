#SOurabh Raput
#CS 513
#10431188


#Naive Bayes

rm(list = ls())


library(class) 
#install.packages('e1071')
library(e1071)

dsn <- read.csv("C:/Users/rajpu/Desktop/KDDM/breast-cancer-wisconsin.data.csv",header = T)


class(dsn)

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

nb_all <- naiveBayes(Class ~F1+F2+F3+F4+F5+F6+F7+F8+F9, data =dsn)

category_all<-predict(nb_all,dsn  )
category_all


table(nb_all=category_all,Class=dsn$Class)
NB_wrong<-sum(category_all!=dsn$Class)
NB_error_rate<-NB_wrong/length(category_all)
NB_error_rate

#Random Forest

set.seed(123)

index<-sort(sample(nrow(dsn),round(.30*nrow(dsn))))
training<-dsn[-index,]
test<-dsn[index,]

#install.packages("randomForest")
library(randomForest)

fit <- randomForest(Class~F1+F2+F3+F4+F5+F6+F7+F8+F9,data=training, importance=TRUE, ntree=1000)

importance(fit)
varImpPlot(fit)
Prediction <- predict(fit, test)
table(actual=test[,11],Prediction)


wrong<- (test[,11]!=Prediction )
error_rate<-sum(wrong)/length(wrong)
error_rate 
accuracy <- (1-error_rate)*100
accuracy

