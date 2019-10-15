#################################################################
# PROBLEM 1: Regression in big-n
# Updated: 2019-06-08
# Author: Anna Carlsson
#################################################################

# Note that this notebook only contains a small part of the analysis.
# The rest can be found in the Jupyter notebook named problem_1.ipynb

##### SET WORKING DIRECTORY AND LOAD PACKAGES #####
setwd("/Users/anna/Dokument/GitHub/MVE440/Exam/Datasets")
library(dplyr)
library(ggplot2)
library(gplots)
library(ggpubr)
library(relaimpo)
library(glmnet)

#set.seed(5532) # For reproducible results

##### Import data #####
load("exercise1.RData")

features <- XB1
response <- yB1
data <- as.data.frame(cbind(features,response))

##### Relative importance #####
# Fit multivariate linear model
data <- data.frame(scale(data))
ols.sat <- lm(response ~ ., data=data)
summary(ols.sat)

# Compute relative importance
relaimp <- calc.relimp(ols.sat, type = c("lmg"), rela = FALSE)
relaimp <- relaimp@lmg
