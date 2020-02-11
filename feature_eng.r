rm(list = ls())
set.seed(2018)
x <- seq(-10, 10, length.out = 100)
y <- seq(-10, 10, length.out = 100)
MyData <- expand.grid(x = x, y = y)
MyData$z <-as.factor(sqrt(MyData$x^2+MyData$y^2)>5)

Index_Temp=sample(nrow(MyData), 0.05*nrow(MyData))
MyData$z[Index_Temp]<-FALSE      

Index_Temp=sample(nrow(MyData), 0.05*nrow(MyData))
MyData$z[Index_Temp]<-TRUE
       
library(ggplot2)
ggplot(MyData, aes(x, y)) +  geom_raster(aes(fill = z))+theme_bw()



trainIndex<-sample(nrow(MyData), 0.1 *nrow(MyData))
MyDataTrain=MyData[trainIndex,]


ggplot(MyDataTrain, aes(x, y))+geom_point(aes(colour = factor(z)))+ theme_bw()

GLM_Model<-glm(z~.,data=MyDataTrain, family = "binomial")
MyData$Predicted_z_glm<-predict(GLM_Model,MyData, type="response")
summary(MyData$Predicted_z_glm)

ggplot(MyData, aes(x, y)) +  geom_raster(aes(fill = Predicted_z_glm>0.5))+theme_bw()
library(pROC)
roc(MyData$z, MyData$Predicted_z_glm)

MyData$Predicted_z_glm<-NULL
library(rpart)
Tree_Model<-rpart (z~.,data=MyDataTrain, method='class')

library(rattle)
fancyRpartPlot(Tree_Model)

T=predict(Tree_Model,MyData)
MyData$Predicted_z_tree=T[,2]

ggplot(MyData, aes(x, y)) +  geom_raster(aes(fill = Predicted_z_tree>0.5))+theme_bw()
roc(MyData$z, T[,2])
MyData$Predicted_z_tree<-NULL


MyData$d=sqrt(MyData$x^2+MyData$y^2)
MyDataTrain=MyData[trainIndex,]

GLM_Model<-glm(z~.,data=MyDataTrain, family = "binomial")
MyData$Predicted_z_glm<-predict(GLM_Model,MyData, type="response")
ggplot(MyData, aes(x, y)) +  geom_raster(aes(fill = Predicted_z_glm>0.5))+theme_bw()
roc(MyData$z, MyData$Predicted_z_glm)
MyData$Predicted_z_glm<-NULL

Tree_Model<-rpart (z~.,data=MyDataTrain, method='class')
fancyRpartPlot(Tree_Model)

T=predict(Tree_Model,MyData)
MyData$Predicted_z_tree=T[,2]
ggplot(MyData, aes(x, y)) +  geom_raster(aes(fill = Predicted_z_tree>0.5))+theme_bw()
roc(MyData$z, T[,2])
MyData$d<-NULL
MyData$Predicted_z_tree<-NULL

#####################################################################################################
MyData$T=sqrt(MyData$x^2+MyData$y^2)

MyData$z1 <-(MyData$T>5) 
MyData$z2 <- (MyData$T <8)
MyData$z<-MyData$z2& MyData$z1
MyData$z1<-NULL
MyData$z2<-NULL
MyData$T<-NULL

Index_Temp=sample(nrow(MyData), 0.05*nrow(MyData))
MyData$z[Index_Temp]<-FALSE      

Index_Temp=sample(nrow(MyData), 0.05*nrow(MyData))
MyData$z[Index_Temp]<-TRUE

library(ggplot2)
ggplot(MyData, aes(x, y)) +  geom_raster(aes(fill = z))+theme_bw()
trainIndex<-sample(nrow(MyData), 0.1 *nrow(MyData))
MyDataTrain=MyData[trainIndex,]


ggplot(MyDataTrain, aes(x, y))+geom_point(aes(colour = factor(z)))+ theme_bw()

GLM_Model<-glm(z~.,data=MyDataTrain, family = "binomial")
MyData$Predicted_z_glm<-predict(GLM_Model,MyData, type="response")
summary(MyData$Predicted_z_glm)

ggplot(MyData, aes(x, y)) +  geom_raster(aes(fill = Predicted_z_glm>0.5))+theme_bw()
roc(MyData$z, MyData$Predicted_z_glm)

MyData$Predicted_z_glm<-NULL
Tree_Model<-rpart (z~.,data=MyDataTrain, method='class')
fancyRpartPlot(Tree_Model)

T=predict(Tree_Model,MyData)
MyData$Predicted_z_tree=T[,2]

ggplot(MyData, aes(x, y)) +  geom_raster(aes(fill = Predicted_z_tree>0.5))+theme_bw()
roc(MyData$z, T[,2])
MyData$Predicted_z_tree<-NULL


MyData$d=sqrt(MyData$x^2+MyData$y^2)
MyDataTrain=MyData[trainIndex,]

GLM_Model<-glm(z~.,data=MyDataTrain, family = "binomial")
MyData$Predicted_z_glm<-predict(GLM_Model,MyData, type="response")
ggplot(MyData, aes(x, y)) +  geom_raster(aes(fill = Predicted_z_glm>0.5))+theme_bw()
roc(MyData$z, MyData$Predicted_z_glm)
MyData$Predicted_z_glm<-NULL

Tree_Model<-rpart (z~.,data=MyDataTrain, method='class')
fancyRpartPlot(Tree_Model)

T=predict(Tree_Model,MyData)
MyData$Predicted_z_tree=T[,2]
ggplot(MyData, aes(x, y)) +  geom_raster(aes(fill = Predicted_z_tree>0.5))+theme_bw()
roc(MyData$z, T[,2])
MyData$d<-NULL
MyData$Predicted_z_tree<-NULL


#######################################################################################
MyData$z2<-NULL
