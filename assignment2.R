#PartA: Perceptron
#import data
data(iris)
data1<-iris
#drop data with label of virginica
subdata<-data1[-which(data1$Species=='virginica'),]
subdata$Species<-as.character(subdata$Species)
#convert lable to binary form
subdata$Species[which(subdata$Species=='versicolor')]<--1
subdata$Species[which(subdata$Species=='setosa')]<-1

subdata<-as.matrix(subdata)
subdata=matrix(as.numeric(subdata),nrow=nrow(subdata))
#****************************************
#create random dataset
randomset<-matrix(nrow=nrow(subdata),ncol=ncol(subdata))
#create index list to store the random index of subdata
index<-c(1,2:nrow(subdata))
index<-sample(index,length(index))
#store instances with the order of random indexes
for (i in 1:length(index)){
  randomset[i,]<-subdata[index[i],]
}

#****************************************
#perceptron funtion
perceptron<- function(data,n,b,niters){
  x <- data[,-dim(data)[2]]
  y <- data[,dim(data)[2]]

  len <- length(x[,1])
#initialize w as 0
  w <- rep(0,dim(x)[2])

  i <- 1  
 #initialize a counter to limit the iters
  counts=0
  while(i <= len){
    counts=counts+1
    if((y[i] * (x[i,] %*% w + b) )<= 0){
      ## update w and b
      w <- w + n * y[i] * x[i,]
      b <- b + n * y[i]
      i <- 1 
    }
    else{
      i <- i + 1 
    }
#set max iters
    if (counts==niters){break}
  }
  return(list(w=w,b=b,counts=counts))
}
#****************************************************

perceptron(subdata,0.1,-0.5,10000)

perceptron(randomset,0.1,-0.5,10000)

#PartB 3
setwd("C:/Users/Admin/Desktop/DA/datamining/exercise/excercise2/datamining-exercise2")
paperdata <- read.table("ai2013_papers.csv",header=T, sep=",")
papertrain<-paperdata[2:12]
papertrain<-scale(papertrain)
kc <- kmeans(papertrain, 8)
fitted(kc) 
#DBSCAN
library(cluster)
library(fpc)
ds <- dbscan(papertrain, 0.95,3,showplot=T,method="raw")  
ds
#hcluster
library(cluster)
d<-dist(papertrain)
hc = hclust(d,method = "average")
summary(hc)
hcout=cutree(hc,k=8)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
kms = silhouette(kc$cluster,dist(papertrain))
summary(kms)

dss = silhouette(ds$cluster,dist(papertrain))
summary(dss)
hcss = silhouette(out,dist(papertrain))
summary(hcss)

table1<-table(paperdata[,13],kc$cluster)
table1

table2<-table(paperdata[,13],ds$cluster)
table2

table3<-table(paperdata[,13],hcout)
table3







