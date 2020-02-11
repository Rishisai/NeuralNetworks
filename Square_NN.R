rm(list = ls())
htmlwidgets
library(nnet)
library(neuralnet)
set.seed(2018)

x=runif(500,min=0,max=100)
y=sqrt(x)

myData=data.frame(x=x,y=y)
fit_nn = neuralnet(y ~x ,data =  myData,  hidden=10,threshold = 0.01)
plot(fit_nn)

testData=data.frame(x=(1:10)^2)

estimates=compute(fit_nn,testData)

print(data.frame(input=testData$x, expected_output=sqrt(testData$x), nn_estimate=estimates$net.result))