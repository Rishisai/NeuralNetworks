rm(list = ls())
set.seed(1000)

library(mlbench)
library(caret)
library(nnet)
data("BostonHousing")

Train_Index=sample(nrow(BostonHousing),round(nrow(BostonHousing)*.8))

TrainData=BostonHousing[Train_Index,]
TestData=BostonHousing[-Train_Index,]

###########Trying Linear Regression ################
lm_model=lm(medv~.,data=TrainData)
predicts_lm=predict(lm_model,newdata=TestData)
R2_lm=cor(predicts_lm,TestData$medv)^2
print(R2_lm)

###########Tryng ANN ###############################
#Let's first scale the dat 
preProcValues <- preProcess(TrainData, method = c("center", "scale"))
trainTransformed <- predict(preProcValues, TrainData)
testTransformed <- predict(preProcValues, TestData)

model_nn<-nnet(medv~.,data=trainTransformed,size=5, linout=TRUE)
predicts_nn<-predict(model_nn,testTransformed,type = "raw")

R2_nn=cor(predicts_nn,testTransformed$medv)^2
print(R2_nn)

#################Trying ANN on unscaled data ##############

model_nn2<-nnet(medv~.,data=TrainData,size=5, linout=TRUE)
predicts_nn2<-predict(model_nn2,TestData,type = "raw")

R2_nn2=cor(predicts_nn2,TestData$medv)^2
print(R2_nn2)

