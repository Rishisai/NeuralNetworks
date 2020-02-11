rm(list = ls())
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"
wdbc <- read.csv(url, header = FALSE)
set.seed(2018)

colnames(wdbc)[2] <- "Malignant.Benign"
#wdbc$Malignant.Benign <- as.numeric(wdbc$Malignant.Benign == 'M')

wdbc_vars<-wdbc[,3:32]

maxx = apply(wdbc_vars , 2 , max)
minn = apply(wdbc_vars, 2 , min)
wdbc_varst_transformed = as.data.frame(scale(wdbc_vars, center = minn, scale = maxx - minn))

wdbc_transformed=cbind(wdbc_varst_transformed,wdbc[2])

n <- paste(names(wdbc_transformed[1:30]), collapse = ' + ')
f <- as.formula(c("Malignant.Benign ~ " , n))

samplesize = 0.80 * nrow(wdbc_transformed)
set.seed(2018)
index = sample( seq_len ( nrow ( wdbc_transformed ) ), size = samplesize )

# Create training and test set
datatrain = wdbc_transformed[ index, ]
datatest = wdbc_transformed[ -index, ]



numFolds <- trainControl(method = 'repeatedcv', number = 10, verboseIter = TRUE,repeats = 2, classProbs = TRUE,summaryFunction = twoClassSummary) # 10 FOLD CV REPEATED 10 TIMES
nn_fit <- train(f, data = datatrain, method = 'nnet', trControl = numFolds, metric = "ROC")
nn_fit

predicted_values=predict(nn_fit,datatest)

table(predicted_values,datatest$Malignant.Benign)


numFolds <- trainControl(method = 'repeatedcv', number = 10, verboseIter = TRUE,repeats = 2, classProbs = TRUE,summaryFunction = twoClassSummary) # 10 FOLD CV REPEATED 10 TIMES
nn_fit2 <- train(f, data = datatrain, method = 'nnet', trControl = numFolds, metric = "ROC", tuneGrid=expand.grid(size=c(3,5,7), decay=c(0.01,0.1,0.2,0.3)))
nn_fit2
varImp(nn_fit2)

############################################################


glm_fit <- glm(f, data = datatrain, family = 'binomial')
prob_glm=predict(glm_fit,datatest,type = 'response')
table(prob_glm>0.5,datatest$Malignant.Benign)
summary(glm_fit)
################################################################
library(rpart)
dt_fit <- rpart(f, data = datatrain, method="class")
prob_rpart=predict(dt_fit,datatest)
head(prob_rpart)
table(prob_rpart[,2]>0.5,datatest$Malignant.Benign)
library(rattle)
fancyRpartPlot(dt_fit)

numFolds <- trainControl(method = 'repeatedcv', number = 10, verboseIter = TRUE,repeats = 2, classProbs = TRUE,summaryFunction = twoClassSummary) # 10 FOLD CV REPEATED 10 TIMES
dt_fit2 <- train(f, data = datatrain, method = 'rpart', trControl = numFolds, metric = "ROC", )
dt_fit2

dt_fit3 <- train(f, data = datatrain, method = 'rpart', trControl = numFolds, metric = "ROC", tuneGrid=expand.grid(cp=seq(0.001,0.1,by=0.004)) )
dt_fit3
prob_rpart3=predict(dt_fit3,datatest)
head(prob_rpart3)
table(prob_rpart3,datatest$Malignant.Benign)
plot(dt_fit3)

dt_fit4 <- train(f, data = datatrain, method = 'rpart', trControl = numFolds, metric = "ROC", tuneGrid=expand.grid(cp=seq(0.0001,0.01,by=0.0001)) )
dt_fit4
prob_rpart4=predict(dt_fit4,datatest)
head(prob_rpart4)
table(prob_rpart4,datatest$Malignant.Benign)
plot(dt_fit4)
summary(dt_fit4)
varImp(dt_fit4)
fancyRpartPlot(dt_fit4) # you will get an error here becuase dt_fit4 is a caret object now and not rpart object

##################################################
library(corrplot)
corrplot(cor(wdbc_varst_transformed))
