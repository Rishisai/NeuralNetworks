rm(list = ls())
set.seed(1000)
         
########################## Function to plot Decision Boundries #################################
decisionplot <- function(model, data, class = NULL, predict_type = "class",
                         resolution = 100, showgrid = TRUE, ...) {
  
  if(!is.null(class)) cl <- data[,class] else cl <- 1
  data <- data[,1:2]
  k <- length(unique(cl))
  
  plot(data, col = as.integer(cl)+1L, pch = as.integer(cl)+1L, ...)
  
  # make grid
  r <- sapply(data, range, na.rm = TRUE)
  xs <- seq(r[1,1], r[2,1], length.out = resolution)
  ys <- seq(r[1,2], r[2,2], length.out = resolution)
  g <- cbind(rep(xs, each=resolution), rep(ys, time = resolution))
  colnames(g) <- colnames(r)
  g <- as.data.frame(g)
  
  ### guess how to get class labels from predict
  ### (unfortunately not very consistent between models)
  p <- predict(model, g, type = predict_type)
  if(is.list(p)) p <- p$class
  p <- as.factor(p)
  
  if(showgrid) points(g, col = as.integer(p)+1L, pch = ".")
  
  z <- matrix(as.integer(p), nrow = resolution, byrow = TRUE)
  contour(xs, ys, z, add = TRUE, drawlabels = FALSE,
          lwd = 2, levels = (1:(k-1))+.5)
  
  invisible(z)
}

############################################################

library(mlbench)
MyData <- mlbench.circle(100)
MyData <- cbind(x=as.data.frame(MyData$x), factor(MyData $classes))
colnames(MyData) <- c("x", "y", "class")
plot(MyData[,1:2], col = MyData[,3])

library(nnet)
model <- nnet(class ~ ., data=MyData, size = 20, maxit = 10000, trace = FALSE)
decisionplot(model, MyData, class = "class", main = "NN (20)")
  
library("rpart")
model <- rpart(class ~ ., data=MyData)
decisionplot(model, MyData, class = "class", main = "DT")      

model <- rpart(class ~ ., data=MyData, control = rpart.control( minsplit = 1))      
decisionplot(model, MyData, class = "class", main = "DT")    

model <- glm(class ~., data = MyData, family=binomial(link='logit'))
class(model) <- c("lr", class(model))
predict.lr <- function(object, newdata, ...)
predict.glm(object, newdata, type = "response") > .5
decisionplot(model, MyData, class = "class", main = "Logistic Regression")    

