rm(list = ls())
library(rgl)
set.seed(2018)

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
# This function is used to generate random points

Random.Unit <-function(n, dim, threshold) { 
  points <- runif(n * dim)
  points <- matrix(points, ncol = dim)
  label <- ifelse(apply(points, 1, sum) < threshold, -1, 1)
  return(cbind(label, x0 = rep(1, n), points))
}

############################################################
# this function is used to plot the points and the decision line
Plot2D <- function(points, a, b) { 
  plot(points[, 3:4], xlab = "X", ylab = "Y",
       pch = ifelse(points[, 1] == 1, 2, 8),
       col = ifelse(points[, 1] == 1, "blue", "red"))
  abline(a, b) #the decision line is identified by the bias and the slope of the line (a an b)
}

################################################################
# Classify is our simple classification rule for the perceptron.We simply 
# return the sign of the dot-product of our observations and weights
Classify <- function(x, weights) {
  return(sign(x %*% weights))
}
##########################################################################
##########################################################################
##########################################################################

THRESHOLD <- 0.75 # just as a enaxmple of how we want to classify points
pts <- Random.Unit(1000, 2, THRESHOLD) # creat 1000 samples/+*
Plot2D(pts, THRESHOLD, -1) # plot them 
Alpha=0.2

# Let us test Precepton Algorthim #############

data=pts
w <- c( runif(ncol(data) - 1)) #completely random weights to start with
w0<-w
n <- nrow(data) # number of observations
label <- data[ , 1] # get the labels, the first column
obs <- data[ , 2:ncol(data)] # get the data. The first column of the data is all 1 becuase of the bias - x0
misclassfied <- TRUE # Set the misclassified flag to TRUE to start the while loop 
iteration=0  # set the number of iterations to 0

while (misclassfied) { # start the loop and continue until there is no misclassified observation
  iteration=iteration+1 #increase the number of iterations 
  print(paste0("Alpha is " , Alpha, " This is iteration number: ", iteration)) #print alpha and the number of iterations
  misclassfied <- FALSE # set the misclasification flag to false until we see otherwise
  for (i in 1:n) { # start scanning the observations one by one
    if (label[i] * Classify(obs[i , ], w) <= 0) { # this is if i-th observation is misclassified(note that if the label and the result of the classification are both 1 or are both -1, their product will be 1, if they are different, however, (e.g. one of them 1 the other -1), the result would be -1) 
      w <- w + Alpha* c(label[i]-Classify(obs[i , ], w)) * obs[i , ] #update the weights using the DELTA RULE
      misclassfied <- TRUE # set the misclassification flag to True
    }                   
    if (i%%20==0 ){ # check if the mod of the i to 20 is zero , ie. i=20,40,60,80 ...to check the progress
      #invisible(readline(prompt="Press [enter] to continue")) #puase waiting for pressing the enter key
      #Plot2D(pts, -w[1]/w[3], -w[2]/ w[3]) # plot the decision boundry
    }
  }
}
print("Algorithm Converged!") 

# At this point the learning has converged *we are outside the while loop)
# Let us plot the final decision boundry

Plot2D(pts, -w[1]/w[3], -w[2]/ w[3])
w/sqrt(sum(w^2))


###################### Compare with Logestic Regression ########################

MyDataframe<-data.frame(x=obs[,2],y=obs[,3],z=as.factor(label))
mdl <- glm( z ~ . , data = MyDataframe , family=binomial)
slope <- coef(mdl)[2]/(-coef(mdl)[3])
intercept <- coef(mdl)[1]/(-coef(mdl)[3]) 
#Plot2D(pts, intercept, slope)
abline(a = intercept, b = slope, col = "blue")


######################### Compare with Decision Tree ####################
library("rpart")
MyData$Target=as.factor(MyData$Target)
model <- rpart(Target ~ . ,data =  MyData )
decisionplot(model, MyData, class = "Target", main = "DT")
library(rattle)
fancyRpartPlot(model)


#################################################################################################
#Trying the effect of Adaptive Learning Rate
w<-w0
misclassfied <- TRUE # Set the misclassified flag to TRUE to start the while loop 
iteration=0  # set the number of iterations to 0
Counter=0
Alpha=1

while (misclassfied) { # start the loop and continue until there is no misclassified observation
  iteration=iteration+1 #increase the number of iterations 
  print(paste0("Alpha is " , Alpha, " This is iteration number: ", iteration)) #print alpha and the number of iterations
  misclassfied <- FALSE # set the misclasification flag to false until we see otherwise
  for (i in 1:n) { # start scanning the observations one by one
    Counter=Counter+1
    Alpha=min(1,400/Counter)
    if (label[i] * Classify(obs[i , ], w) <= 0) { # this is if i-th observation is misclassified(note that if the label and the result of the classification are both 1 or are both -1, their product will be 1, if they are different, however, (e.g. one of them 1 the other -1), the result would be -1) 
      w <- w + Alpha* c(label[i]-Classify(obs[i , ], w)) * obs[i , ] #update the weights using the DELTA RULE
      misclassfied <- TRUE # set the misclassification flag to True
    }                   
    if (i%%20==0 ){ # check if the mod of the i to 20 is zero , ie. i=20,40,60,80 ...to check the progress
      #invisible(readline(prompt="Press [enter] to continue")) #puase waiting for pressing the enter key
      #Plot2D(pts, -w[1]/w[3], -w[2]/ w[3]) # plot the decision boundry
    }
  }
}
print("Algorithm Converged!") 

# At this point the learning has converged *we are outside the while loop)
# Let us plot the final decision boundry

Plot2D(pts, -w[1]/w[3], -w[2]/ w[3])
w/sqrt(sum(w^2)) 


##############################################################################################
pts[1:100,1]=pts[1:100,1]*-1 # let us swap the label of 100 random points and see if the algorithm converges 
Plot2D(pts, THRESHOLD, -1)

data=pts
w <- c( runif(ncol(data) - 1))
n <- nrow(data)
label <- data[ , 1]
obs <- data[ , 2:ncol(data)]
misclassfied <- TRUE
iteration=0

#NOTE: The algorithm will NOT converge so you have to pre ESC to stop R-studio ###

while (misclassfied) { # start the loop and continue until there is no misclassified observation
  iteration=iteration+1 #increase the number of iterations 
  print(paste0("This is iteration number: ", iteration)) #print the number of iterations
  misclassfied <- FALSE # set the misclasification flag to false until we see otherwise
  for (i in 1:n) { # start scanning the observations one by one
    if (label[i] * Classify(obs[i , ], w) <= 0) { # this is if i-th observation is misclassified(note that if the label and the result of the classification are both 1 or are both -1, their product will be 1, if they are different, however, (e.g. one of them 1 the other -1), the result would be -1) 
      w <- w + Alpha* c(label[i]-Classify(obs[i , ], w)) * obs[i , ] #update the weights using the DELTA RULE
      misclassfied <- TRUE # set the misclassification flag to True
    }                   
    #if (i%%20==0 ){ # check if the mod of the i to 20 is zero , ie. i=20,40,60,80 ...to check the progress
    # invisible(readline(prompt="Press [enter] to continue")) #puase waiting for pressing the enter key
    # Plot2D(pts, -w[1]/w[3], -w[2]/ w[3]) # plot the decision boundry
  }
}

pts[1:100,1]=pts[1:100,1]*-1 # Change the pts to original set up

#################################################################################################

Plot2D(pts, THRESHOLD, -1) # plot points 

pts[1,1]=pts[1:1,1]*-1 # Lets change only one observation and see if converge
Plot2D(pts, THRESHOLD, -1) # plot them 

data=pts
w <- c( runif(ncol(data) - 1))
n <- nrow(data)
label <- data[ , 1]
obs <- data[ , 2:ncol(data)]
misclassfied <- TRUE
iteration=0

#NOTE: The algorithm will NOT converge so you have to pre ESC to stop R-studio ###

while (misclassfied) { # start the loop and continue until there is no misclassified observation
  iteration=iteration+1 #increase the number of iterations 
  print(paste0("This is iteration number: ", iteration)) #print the number of iterations
  misclassfied <- FALSE # set the misclasification flag to false until we see otherwise
  for (i in 1:n) { # start scanning the observations one by one
    if (label[i] * Classify(obs[i , ], w) <= 0) { # this is if i-th observation is misclassified(note that if the label and the result of the classification are both 1 or are both -1, their product will be 1, if they are different, however, (e.g. one of them 1 the other -1), the result would be -1) 
      w <- w + Alpha* c(label[i]-Classify(obs[i , ], w)) * obs[i , ] #update the weights using the DELTA RULE
      misclassfied <- TRUE # set the misclassification flag to True
    }                   
    #if (i%%20==0 ){ # check if the mod of the i to 20 is zero , ie. i=20,40,60,80 ...to check the progress
    # invisible(readline(prompt="Press [enter] to continue")) #puase waiting for pressing the enter key
    # Plot2D(pts, -w[1]/w[3], -w[2]/ w[3]) # plot the decision boundry
  }
}

pts[1,1]=pts[1:1,1]*-1  # Change the pts to original set up

# So even with one single observation which makes not to be linearly seprable the Perceptron learning does not work

######################################################################33
library(nnet)
library(neuralnet)

Plot2D(pts, THRESHOLD, -1) # plot points

MyData=data.frame(X=pts[,3],Y=pts[,4], Target=as.factor(pts[,1]))
MyData=data.frame(X=pts[,3],Y=pts[,4], Target=pts[,1])
#fitnn1 = nnet(Target ~ X + Y ,data =  MyData, size=0, skip=TRUE, linout=TRUE)
fitnn1 = neuralnet(Target ~ X + Y ,data =  MyData,  hidden=0, threshold = 0.01)

plot(fitnn1)

summary(fitnn1)
w<-unlist(fitnn1$weights)
w/sqrt(sum(w^2)) 
