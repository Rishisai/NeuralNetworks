
rm(list = ls())
MyData=read.csv('https://s3-ap-south-1.amazonaws.com/av-blog-media/wp-content/uploads/2017/09/07122416/cereals.csv')
# Random sampling
library(neuralnet)
library(caret)

samplesize = 0.60 * nrow(MyData)
set.seed(2018)
index = sample( seq_len ( nrow ( MyData ) ), size = samplesize )

# Create training and test set
datatrain = MyData[ index, ]
datatest = MyData[ -index, ]

maxx = apply(datatrain , 2 , max)
minn = apply(datatrain, 2 , min)
trainTransformed = as.data.frame(scale(datatrain, center = minn, scale = maxx - minn))
testTransformed= as.data.frame(scale(datatest, center = minn, scale = maxx - minn))



#preProcValues <- preProcess(datatrain, method = c("center", "scale"))
#trainTransformed <- predict(preProcValues, datatrain)
#testTransformed <- predict(preProcValues, datatest)

NN = neuralnet(rating ~ calories + protein + fat + sodium + fiber, trainTransformed, hidden = 3 , linear.output = T )
plot(NN)

#predict_testNN = compute(NN, testTransformed) # You will get an error. The compute function exactly the very same attributes used to train the model. You need to drop the rating which was the target.

predict_testNN = compute(NN, testTransformed[,c(1:5)])
predict_testNN_unscaled = (predict_testNN$net.result * (max(datatrain$rating) - min(datatrain$rating))) + min(datatrain$rating)

plot(datatest$rating, predict_testNN_unscaled, col='blue', pch=16, ylab = "predicted rating NN", xlab = "real rating")
abline(0,1)

RMSE.NN = (sum((datatest$rating - predict_testNN_unscaled)^2) / nrow(datatest)) ^ 0.5

###########################################################################################################################

lm_model=lm(rating ~ calories + protein + fat + sodium + fiber,data=datatrain)
predict_testlm=predict(lm_model,datatest)

plot(datatest$rating, predict_testlm, col='red', pch=16, ylab = "predicted rating LM", xlab = "real rating")
abline(0,1)

RMSE.lm = (sum((datatest$rating - predict_testlm)^2) / nrow(datatest)) ^ 0.5
summary(lm_model)

####################################################
#Cross Validations using Caret Package

set.seed(2018)
numFolds <- trainControl(method = 'cv', number = 10, verboseIter = TRUE) # 10 fold CV
fit2 <- train(rating ~ calories + protein + fat + sodium + fiber, data = trainTransformed, method = 'neuralnet', trControl = numFolds)
fit2

fit3 <- train(rating ~  calories + protein + fat + sodium + fiber, data = trainTransformed, method = 'neuralnet', trControl = numFolds, tuneGrid=expand.grid(layer1=c(3,7,5), layer2=c(0,3), layer3=c(0,2)))
fit3
plot(fit3)  


numFolds <- trainControl(method = 'repeatedcv', number = 10, verboseIter = TRUE,repeats = 10) # 10 FOLD CV REPEATED 10 TIMES
fit4 <- train(rating ~ calories + protein + fat + sodium + fiber, data = trainTransformed, method = 'neuralnet', trControl = numFolds, tuneGrid=expand.grid(layer1=c(3,7,5), layer2=c(0,3), layer3=c(0,2)))
fit4


numFolds <- trainControl(method = 'LOOCV', verboseIter = TRUE)  # Leave One Out Cross Validation
fit5 <- train(rating ~ calories + protein + fat + sodium + fiber, data = trainTransformed, method = 'neuralnet', trControl = numFolds, tuneGrid=expand.grid(layer1=c(3,7,5), layer2=c(0,3), layer3=c(0,2)))
fit5

# Trying the unscaled data 
numFolds <- trainControl(method = 'cv', number = 10, verboseIter = TRUE)  # Leave One Out Cross Validation
fit6 <- train(rating ~ calories + protein + fat + sodium + fiber, data = datatrain, method = 'neuralnet', trControl = numFolds, tuneGrid=expand.grid(layer1=c(3,7,5), layer2=c(0,3), layer3=c(0,2)))
fit6



# Fitting the model without searching 
numFolds <- trainControl(method = 'none')  # Leave One Out Cross Validation
fit7 <- train(rating ~ calories + protein + fat + sodium + fiber, data = datatrain, method = 'neuralnet', trControl = numFolds, tuneGrid= data.frame(layer1=5, layer2=0, layer3=0))
fit7
summary(fit7)
