#Sourabh Rajput
#CS 513
#10431188


remove(list=ls())

bc<-read.csv("C:/Users/Desktop/KDDM/breast-cancer-wisconsin.data.csv",na.strings = "?")
bc2<-na.omit(bc)


bc2_dist<-dist( bc2[,-c(1,11)])
hclust_resutls<-hclust(bc2_dist,method="average")
hclust_resutls<-hclust(bc2_dist)
hclust_2<-cutree(hclust_resutls,2)
table(hclust_2,bc2[,11])


kmeans_2<- kmeans(bc2[,-c(1,11)],2,nstart = 10)
kmeans_2$cluster
table(kmeans_2$cluster,bc2[,11])



