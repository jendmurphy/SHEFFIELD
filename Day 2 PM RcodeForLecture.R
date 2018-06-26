####################################################################
# R code for the SVM lecture. It contains the practical tasks that 
# the students will do at different points of the lecture
#
# Adnane Ez-zizi, 26/06/2017
####################################################################

#################################################
# Installing necessary libraries 
#################################################

install.packages(c("e1071", 
                   "kernlab",
                   "caret", 
                   "parallel",
                   "foreach",
                   "doParallel",
                   "tictoc"))

#################################################
# Loading necessary libraries 
#################################################

### Set the working directory
setwd('~/PostDoc Sheffield/Teaching/ML workshop/SVM')

### Load libraries
library(e1071)    # To run SVMs
library(kernlab)	# Another SVM package
library(caret)		# To train SVM and other ML algorithms 
library(parallel) # For parallel processing						
library(foreach)  # For parallel processing						
library(doParallel) # For parallel processing		
library(tictoc) # To measure run time

####################################################################
# to run in parallel (optional)
####################################################################

threads = detectCores()					
cl = makeCluster(threads-1)				
registerDoParallel(cl)	

#################################################
# Sec.1: Maximum margin classifier with linearly 
#         separable classes 
#################################################

### Simulating a dataset that is linearly separable classes 
set.seed(10)
x = matrix(rnorm(20*2), ncol = 2)
y = c(rep(-1, 10), rep(1, 10) )
# Increase the mean of X in class y=1 by 2 to better separate the 
# two classes 
x[y==1, ] = x[y==1, ] + 2 

### Ploting the data and checking that the two classes are linearly
### seperable
plot(x, col = 3+y, pch = 19, xlab = "X1", ylab = "X2") 
grid()
legend(-1.7, 3, pch=c(19,19), legend=c("Y = -1", "Y = 1"), 
       bty="o", col=c(2,4), box.col="black", cex=.8)
# => Red points corresponds to y=-1 while blue points corresponds
#    to y=1

# First, we create a dataframe out of the matrix X and the 
# resp Y before fitting the maximum margin classifier
dat = data.frame(x = x, y = as.factor(y)) 

# Model fit 
svmfit.10 = svm(y~., data = dat, kernel = "linear", cost = 10, scale = FALSE) 
# => scale=FALSE tells the svm() function to not standarize 
#    each feature
### Important note:
# Here I did not standarize X, but it is safer to standarise 
# continuous predictors before using SVM . If you want to 
# standarise X, then simply use "scale = TRUE" when calling 
# the fuction svm(). If you do so, then the plot function 
# below, which displays the svm output needs to be modified 
# accordingly. It is also important to transform the 
# response into factor before applying SVM, otherwise svm() 
# would perform regression intead of classification. 

# Get some basic information about the Maximum margin classifier
summary(svmfit.10) 
# Plot the obtained SV classier   
plot(svmfit.10, dat) 
# => Not a very nice plot (e.g., X2 on the horizontal axis). 
#    Can be used to obtain a quick plot. The support vectors are 
#    plotted as crosses (there are two here)

# Display the indices of the support vectors (2 SVs here)
svmfit.10$index 

### Making a nicer plot to display the SV classifier
plot_SVM = function(data, sv.fit, n = 49){
  
  # First, we make a grid of values for X1 and X2 (i.e. produces
  # the coordinates of n by n points on a lattice covering the 
  # domain of X)
  grange = apply(data[,c(1:2)], 2, range) # get the range of each of the variables x1 and x2
  x1 = seq(from = grange[1,1], to = grange[2,1], length = n) # make a grid of length n on the direction of x1
  x2 = seq(from = grange[1,2], to = grange[2,2], length = n) # make a grid of length n on the direction of x2
  xgrid = expand.grid(X1 = x1, X2 = x2) # Make the lattice
  xgrid = data.frame(x.1 = xgrid[, 1], x.2 = xgrid[, 2])
  ygrid = predict(sv.fit, xgrid)
  
  # Second, we make the plot using the constructed grid
  plot(xgrid, col = c("red", "blue")[as.numeric(ygrid)], 
       pch = 19, cex = 0.1, xlab = "X1", ylab = "X2")
  x = as.matrix(data[,c(1:2)])
  y = as.numeric(as.character(data[,3]))
  # Color the two classes differently (red for -1 and blue for 1)
  points(x, col = y+3, pch = 19, cex = 1.3) 
  # To highlight the support vectors (cex=2.1 to increase the points 
  # size)
  points(x[sv.fit$index, ], pch = 5, cex = 2.1) 

  # To add the decision boundary and the margins
  # First, compute the estimates of the coeffs beta's 
  # (i.e. intercept and slopes of the decision boundary)
  beta = drop( t(sv.fit$coefs)%*%x[sv.fit$index, ] )
  beta0 = sv.fit$rho
  abline(beta0/beta[2], -beta[1]/beta[2], lwd=2) # Plot the decision boundary
  abline((beta0-1)/beta[2], -beta[1]/beta[2], lty = 2, col = 4, lwd=2) # Plot the upper margin
  abline((beta0+1)/beta[2], -beta[1]/beta[2], lty = 2, col = 2, lwd=2) # Plot the lower margin

}
# Run the plot
plot_SVM(dat, svmfit.10, 49)
# => Red region is where the predicted response from the SV classifier 
#    is Y=-1

#################################################
# Sec.2: Soft margin classifier with linearly 
#         separable classes 
#################################################

### Simulating a new dataset that is hardly linearly separable
set.seed(10)
x = matrix(rnorm(20*2), ncol = 2)
y = c(rep(-1, 10), rep(1, 10) )
# Increase the mean of X in class y=1 by 1 to better separate the 
# two classes 
x[y==1, ] = x[y==1, ] + 1 

### Ploting the data and checking that the two classes are linearly
### seperable
plot(x, col = 3+y, pch = 19, xlab = "X1", ylab = "X2") 
grid()
legend(-1.7, 3, pch=c(19,19), legend=c("Y = -1", "Y = 1"), 
       bty="o", col=c(2,4), box.col="black", cex=.8)
# => Red points corresponds to y=-1 while blue points corresponds
#    to y=1

# Create a dataframe out of the matrix X and the resp Y
dat2 = data.frame(x = x, y = as.factor(y)) 

### Fitting a support vector classifier using a large cost (=100), 
### which is equivalent to having a small C value (thus the classifier
### would be less disposed to tolarate classification errors)
svmfit.100 = svm(y~., data = dat2, kernel = "linear", 
                 cost = 100, scale = FALSE) 

### Fitting a support vector classifier using a small cost (=0.1), 
### which is equivalent to having a large C value (thus the classifier
### would be more disposed tolarate classification errors)
svmfit.01 = svm(y~., data = dat2, kernel = "linear", 
               cost = 0.1, scale = FALSE) 

# Generate the plots for the new SV classifiers and put them  
# side-by-side 
par(mfrow=c(1,2))    
# First plot
plot_SVM(dat2, svmfit.01, 49)
title(main = "cost = 0.1 (i.e. C = 10)")
# Second plot
plot_SVM(dat2, svmfit.100, 49)
title(main = "cost = 100 (i.e. C = 0.01)")
par(mfrow=c(1,1))

###########################################################
# Sec.3: Choosing the cost parameter using cross-validation
###########################################################

### Simulating a larger dataset 
N = 200
set.seed(10)
x = matrix(rnorm(N*2), ncol = 2)
y = c(rep(-1, N/2), rep(1, N/2) )
# Increase the mean of X in class y=1 by 1 to better separate the 
# two classes 
x[y==1, ] = x[y==1, ] + 1 

### Ploting the data and checking that the two classes are linearly
### seperable
plot(x, col = 3+y, pch = 19, xlab = "X1", ylab = "X2") 
grid()
legend(-2.3, 3.7, pch=c(19,19), legend=c("Y = -1", "Y = 1"), 
       bty="o", col=c(2,4), box.col="black", cex=.8)
# => Red points corresponds to y=-1 while blue points corresponds
#    to y=1

# Create a dataframe out of the matrix X and the resp Y
dat3 = data.frame(x = x, y = as.factor(y)) 

# Divide the data into a traing and a test set
set.seed(1)
Ind_train = sample(N, N/2) # sample 100 training indices 
dat3.train = dat3[Ind_train, ]
dat3.test = dat3[-Ind_train, ]

### 1st method: using tune()
?tune
# We will use CV to decide which values of cost to use
set.seed(1)
tune.out = tune(svm, y~., data = dat3.train, kernel = "linear", 
                ranges = list(cost = c(0.1, 0.5, 1, 2, 5, 10, 50, 100)))
# => 10 is the default number of folds in cross-validation

# Display the results of the CV 
summary(tune.out)

# Display a summary of outputs for the best model
best_svm = tune.out$best.model # Select the best model (cost=0.5, i.e. C=2)
summary(best_svm) 
# Parameters:
#   SVM-Type:  C-classification 
# SVM-Kernel:  radial 
#       cost:  0.5 
#      gamma:  0.5

# Evaluation of the best SVM model on the test data
table(true = dat3.test$y, pred = predict(best_svm, newdata = dat3.test))
test.error = (7+13)/100 # Test error rate
test.error # the test error is 20%

### Plot the obtained SVM 
# plot function no longer displaying the support vectors as
# there are many in our case
plot_SVM = function(data, sv.fit, n = 49){
  
  # First, we make a grid of values for X1 and X2 (i.e. produces
  # the coordinates of n by n points on a lattice covering the 
  # domain of X)
  grange = apply(data[,c(1:2)], 2, range) # get the range of each of the variables x1 and x2
  x1 = seq(from = grange[1,1], to = grange[2,1], length = n) # make a grid of length n on the direction of x1
  x2 = seq(from = grange[1,2], to = grange[2,2], length = n) # make a grid of length n on the direction of x2
  xgrid = expand.grid(X1 = x1, X2 = x2) # Make the lattice
  xgrid = data.frame(x.1 = xgrid[, 1], x.2 = xgrid[, 2])
  ygrid = predict(sv.fit, xgrid)
  
  # Second, we make the plot using the constructed grid
  plot(xgrid, col = c("red", "blue")[as.numeric(ygrid)], 
       pch = 19, cex = 0.1, xlab = "X1", ylab = "X2")
  x = as.matrix(data[,c(1:2)])
  y = as.numeric(as.character(data[,3]))
  # Color the two classes differently (red for -1 and blue for 1)
  points(x, col = y+3, pch = 19, cex = 1.3) 
  
  # To add the decision boundary and the margins
  # First, compute the estimates of the coeffs beta's 
  # (i.e. intercept and slopes of the decision boundary)
  beta = drop( t(sv.fit$coefs)%*%x[sv.fit$index, ] )
  beta0 = sv.fit$rho
  abline(beta0/beta[2], -beta[1]/beta[2], lwd=2) # Plot the decision boundary
  abline((beta0-1)/beta[2], -beta[1]/beta[2], lty = 2, col = 4, lwd=2) # Plot the upper margin
  abline((beta0+1)/beta[2], -beta[1]/beta[2], lty = 2, col = 2, lwd=2) # Plot the lower margin
  
}
plot_SVM(dat3.test, best_svm)

### 2nd method: using train() from the caret package. train() 
### allows parallelisation

# Running CV
set.seed(1)
tic()
svm.fit = train(y ~ ., data = dat3.train,
                method = "svmLinear2",
                preProcess = c("center", "scale"),
                tuneGrid = data.frame(cost = c(0.1, 0.5, 1, 2, 5, 10, 50, 100)),
                #tuneLength = 10,
                trControl = trainControl(method = "cv"))
toc()
svm.fit	# print results
# The final value used for the model was cost = 0.1

# Evaluation of the best SVM model on the test data
table(true = dat3.test$y, pred = predict(svm.fit, newdata = dat3.test))
test.error = (8+12)/100 # Test error rate
test.error # the test error is 20%

###################################################
# Sec.4: Support vector machines with non-linearly 
#         separable classes 
###################################################

# Simulating the non-linearly seperable data that we will use 
set.seed(1)
x = matrix(rnorm(200*2), ncol = 2)
x[1:100, ] = x[1:100,] + 2
x[101:150, ] = x[101:150, ] - 2
y = c(rep(1, 150), rep(-1, 50))

# Checking that the two simulated class are not linearly separable
plot(x, col = 3+y, pch = 19, xlab = "X1", ylab = "X2") 
grid()
legend(-4, 4.8, pch=c(19,19), legend=c("Y = -1", "Y = 1"), 
       bty="o", col=c(2,4), box.col="black", cex=.8) 

# Create a dataframe out of the matrix X and the resp Y
dat4 = data.frame(x = x, y = as.factor(y)) 

### Fitting a support vector classifier using cost=1, 
svmfit_gamma_1_cost_1 = svm(y~., data = dat4, kernel = "radial",  gamma = 1, cost = 1) 
# Note that the predictors were standarized

# To get the indices of the support vectors (63 SVs)
svmfit_gamma_1_cost_1$index 

# Plot the obtained SVM
plot(svmfit_gamma_1_cost_1, dat4) 
# Not very nice plot (e.g., X2 on the horizontal axis). 
# Can be used to obtain a quick plot. The support vectors 
# are plotted as crosses (there are 37 here)

### Making a nicer plot to display the SVM output 
plot_SVM_nonLinear = function(data, svm.fit, n = 100){
  
  # First, we make a grid of values for X1 and X2 (i.e. produces
  # the coordinates of n by n points on a lattice covering the 
  # domain of X)
  grange = apply(data[,c(1:2)], 2, range) # get the range of each of the variables x1 and x2
  x1 = seq(from = grange[1,1], to = grange[2,1], length = n) # make a grid of length n on the direction of x1
  x2 = seq(from = grange[1,2], to = grange[2,2], length = n) # make a grid of length n on the direction of x2
  xgrid = expand.grid(X1 = x1, X2 = x2) # Make the lattice
  xgrid = data.frame(x.1 = xgrid[, 1], x.2 = xgrid[, 2])
  ygrid = predict(svm.fit, xgrid)
  
  # Second, we make the plot using the constructed grid
  plot(xgrid, col = c("red", "blue")[as.numeric(ygrid)], 
       pch = 19, cex = 0.1, xlab = "X1", ylab = "X2")
  x = as.matrix(data[,c(1:2)])
  y = as.numeric(as.character(data[,3]))
  points(x, col = y+3, pch = 19, cex = 1.3) # Color the two classes differently (red for -1 and blue for 1)
}
# Run the plot for the SVM we built previously
plot_SVM_nonLinear(dat4, svmfit_gamma_1_cost_1, n = 100)
# Red region is where the predicted response from the SVM  
# is Y=-1

# Compute the error rate on the whole data (quite high here)
ypred = predict(svmfit_gamma_1_cost_1, dat4)
table(predict = ypred, truth = dat4$y) # Evaluation of the SVM on the training data 
error = 20/200 # Training error rate
error # the training error (10%) is quite high here


###########################################################
# Sec.5: Choosing the parameter values of an SVM using 
#        cross-validation
###########################################################

### 1st method: using tune()

# Divide the data into a traing and a test set
set.seed(1)
Ind_train = sample(200, 100) # sample 100 training indices 
dat4.train = dat4[Ind_train, ]
dat4.test = dat4[-Ind_train, ]

# We will use CV to decide which values of cost and gamma to use
set.seed(1)
tune.out = tune(svm, y~., data = dat4.train, kernel = "radial", 
                ranges = list(cost = c(0.1, 1, 10, 100, 1000), 
                              gamma = c(0.5, 1, 2, 3, 4)))
# 10 is the default number of folds in cross-validation

# Display the results of the CV 
summary(tune.out)

# Display a summary of outputs for the best model
best_svm = tune.out$best.model # Select the best model (cost = 1 and gamma = 2)
summary(best_svm) 
# Parameters:
#   SVM-Type:  C-classification 
# SVM-Kernel:  radial 
#       cost:  100 
#      gamma:  0.5

# Evaluation of the best SVM model on the test data
table(true = dat4.test$y, pred = predict(best_svm, newdata = dat4.test))
test.error = 16/100 # Test error rate
test.error # the test error is 16%

# Plot the obtained SVM 
plot_SVM_nonLinear(dat4.test, best_svm, n = 100)

### 2nd method: using train() from the caret package. train() 
### allows parallelisation

# Running CV
set.seed(1)
svm.fit = train(y ~ ., data = dat4.train,
                method = "svmRadial", # Sigma will be estimated from the training set directly. Try svmRadialSigma to tune sigma manually 
                preProcess = c("center", "scale"),
                tuneLength = 10,
                trControl = trainControl(method = "cv"))
svm.fit	# print results
# The final values used for the model were sigma = 1.53 (held constant) and C = 0.5

# Evaluation of the best SVM model on the test data
table(true = dat4.test$y, pred = predict(svm.fit, newdata = dat4.test))
test.error = 14/100 # Test error rate
test.error # the test error is 14%

# Plot the obtained SVM 
plot_SVM_nonLinear(dat4.test, svm.fit, n = 100)

