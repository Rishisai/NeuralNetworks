rm(list = ls())

MyData=read.csv('http://archive.ics.uci.edu/ml/machine-learning-databases/00422/wifi_localization.txt', sep="\t", header=FALSE)
colnames(MyData)<-c("s1","s2","s3","s4","s5","s6","s7","R")

MyData<-MyData[MyData[,8]<3,] #select only cases where the room is 1 or 2
MyData$R=as.factor(MyData$R) #covert to factor

MyData$R=class.ind(MyData$R) # this is the strange requirement of the nnet package 

set.seed(1000)

library(caret)
library(nnet)

Train_Index=sample(nrow(MyData),round(nrow(MyData)*.8))

TrainData=MyData[Train_Index,]
TestData=MyData[-Train_Index,]

model_nn <- nnet(R ~., data=TrainData, size = 5, maxit = 10000, trace = FALSE, softmax=TRUE)
predicts_nn<-predict(model_nn,TestData)<0.5*1 #convert probs to predictions

table(predicts_nn,TestData$R) #show confusion matrix

######################################################################
MyData=read.csv('http://archive.ics.uci.edu/ml/machine-learning-databases/00422/wifi_localization.txt', sep="\t", header=FALSE)
colnames(MyData)<-c("s1","s2","s3","s4","s5","s6","s7","R")

MyData<-MyData[MyData[,8]<3,] #select only cases where the room is 1 or 2
MyData$R=as.factor(MyData$R) #covert to factor
TrainData=MyData[Train_Index,]
TestData=MyData[-Train_Index,]

glm_model<-glm(R ~., data=TrainData,family=binomial(link='logit'))
predicted_glm<-predict(glm_model,TestData, type = 'response')>0.5*1
table(predicted_glm,TestData$R) #show confusion matrix


