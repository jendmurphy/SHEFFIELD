####################################################################
# SVM code for the practical application in R. 
#
# Example and R code taken from the supporting material of Kirchner 
# and Signorino (2018) (the Survey Practice special issue)
#
# Here you will conduct SVM classification and Logistic regression for 
# the survey response example.  
#
# If one's computer has at least four cores, we recommend initialising 
# a parallel environment as below. caret's train() command will 
# automatically recognise the parallel environment and use it during 
# cross validation. To run these commands without parallelisation, 
# delete the six lines just after "# to run in parallel." 
#
# Kirchner, A., & Signorino, C. S. (2018). Using Support Vector Machines
# for Survey Research. Survey Practice, 11(1), 2715.
####################################################################
rm(list=ls())
library(tictoc)         # To measure run time
library(kernlab)				# SVM package
library(caret)					# needed for training SVM and for 
								        # confusionMatrix()

####################################################################
# to run in parallel
####################################################################
library(parallel) 						
library(foreach)						
library(doParallel)						
threads=detectCores()					
cl=makeCluster(threads-1)				
registerDoParallel(cl)					

####################################################################
# load data and create test and training data sets.
####################################################################
# change to your path
#setwd("~/PostDoc Sheffield/Teaching/ML workshop/SVM/R code") 

# load the dataset and define the training and test sets
load("SPData2.Rda")

# Divide the data into a traing and a test set






####################################################################
# formula:  response variable ~ covariates
####################################################################
f = newrespond20 ~ age + sex  + wborace + hispanic2 + region + educ3 + incgrp4 + telstat + wrkcata  + ratcat2

####################################################################
# svm.  train() and trainControl() are part of the caret package.
####################################################################
# Running CV






### Evaluation of the performance of the svm model 
# predicted responses
r.svm = predict(svm.fit, newdata = dtest, type = "raw")		
CM.svm = confusionMatrix(r.svm, dtest$newrespond20)
# Contingency table
CM.svm$table 
# Contingency table in the form of marginal proportions
prop.table(CM.svm$table,2) 
# Overall accuracy
CM.svm$overall[1] 

####################################################################
# logistic regression
####################################################################
# Model fit


		

### Evaluation of the performance of the logistic regression model






####################################################################
# Stop running in parallel
####################################################################
stopCluster(cl)
