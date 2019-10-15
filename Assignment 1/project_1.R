#################################################################
# PROJECT 1: Redundancy and classification (RF version)
# Updated: 2019-06-01
# Author: Anna Carlsson
#################################################################

##### Set working directory and load packages #####
setwd("/Users/anna/Dokument/GitHub/MVE440/Exam/Datasets/")
library(ggplot2)
library(gplots)
library(randomForest)
library(mnormt)
library(ggpubr)

palette <- c(
  "#08589e","#2b8cbe","#4eb3d3","#7bccc4","#a8ddb5",  "#ccebc5", "#e0f3db", "#f7fcf0","#cc5260","#FCF4C7","#19605D")

set.seed(33) # For reproducible results

##### PART 1: Classification and correlated features #####
# This section includes code for dataset generation and computations for 
# the part of the project regarding classification accuracy

# Define data generation function
sim_data_regression <- function(n, r) {
  R <- matrix(c(1,r,r,r,r,1,r,r,r,r,1,r,r,r,r,1),4,4)
  X1 <- rmnorm(n, varcov = R)
  X2 <- rnorm(n)
  X3 <- rnorm(n)
  X4 <- rnorm(n)
  X5 <- rnorm(n)
  X6 <- rnorm(n)
  X7 <- rnorm(n)
  Y <- 2*X1[,1] - 1*X1[,3] + 3*X2 - 2*X4 + rnorm(n)
  db <- data.frame(Y=Y, X1=X1[,1], X2=X1[,2], X3=X1[,3], X4=X1[,4], X5=X2, X6=X3, X7=X4, X8=X5, X9=X6, X10=X7)
}

# Correlation coefficients to try
r <- c(0.75, 0.85, 0.90, 0.95, 0.99)

# Set number of observations in training and test
n_train <- 300
n_test <- 100
N <- 300 # number of trees to grow in forest

# Create dataframes to save MSE values for all trees
mse_all <- NULL
mse_wt <- NULL

for (coeff in r){
  # Generate training and test data
  data_train <- as.data.frame(sim_data_regression(n_train, coeff))
  data_test <- as.data.frame(sim_data_regression(n_test, coeff)) 
  
  # Grow forests (with/without correlated variables)
  forest_all <- randomForest(y=data_train[,1],x=data_train[,-1],
                             ytest=data_test[,1],xtest=data_test[,-1], 
                             ntree=N, proximity=T, keep.forest=TRUE, importance=TRUE) 
  
  forest_wt <- randomForest(y=data_train[,1],x=data_train[,-c(1,2,4)],
                            ytest=data_test[,1],xtest=data_test[,-c(1,2,4)], 
                            ntree=N, proximity=T, keep.forest=TRUE, importance=TRUE) 
  
  mse_all <- cbind(mse_all,forest_all$mse)
  mse_wt <- cbind(mse_wt,forest_wt$mse)
}

mse_all <- as.data.frame(mse_all)
mse_wt <- as.data.frame(mse_wt)

##### PART 2: Variable importance and correlated features #####
# This section includes code for dataset generation and computations for 
# the part of the project regarding variable importance measures

# Define functions for computing variable importance measures as function of corr. coeff.
impact_dec_acc <- function(r,n,n_datasets){
  imp <- matrix(NA,4,n_datasets)
  R <- matrix(c(1,r,r,1),2,2)
  for(s in 1:n_datasets){
    print(c(s,r))
    X1 <- rmnorm(n = n,varcov=R)
    X2 <- rnorm(n)
    X3 <- rnorm(n)
    Y <- 2*X1[,1] + 2*X3 + rnorm(n)
    db <- data.frame(Y=Y, X1=X1[,1], X2=X1[,2], X3=X2, X4=X3)
    RF <- randomForest(Y~.,data=db,importance=TRUE)
    imp[,s] <- randomForest::importance(RF, type=1)}
  apply(imp,1,mean)}

impact_dec_impur <- function(r,n,n_datasets){
  imp <- matrix(NA,4,n_datasets)
  R <- matrix(c(1,r,r,1),2,2)
  for(s in 1:n_datasets){
    print(c(s,r))
    X1 <- rmnorm(n = n,varcov=R)
    X2 <- rnorm(n)
    X3 <- rnorm(n)
    Y <- 2*X1[,1] + 2*X3 + rnorm(n)
    db <- data.frame(Y=Y, X1=X1[,1], X2=X1[,2], X3=X2, X4g=X3)
    RF <- randomForest(Y~.,data=db,importance=TRUE)
    imp[,s] <- randomForest::importance(RF, type=2)}
  apply(imp,1,mean)}

#C <- c(-0.999,-0.99,seq(-0.9,-0.65,by=0.05),seq(-0.6,0,by=1),seq(0,.6,by=.1),seq(.65,.9,by=.05),.99,.999)
C <- c(-0.999,seq(-0.99,0.99,by=0.01),0.999)

n_datasets <- 300
n <- 300

VI_1 <- matrix(NA,4,length(C))
for(i in 1:length(C)){
  VI_1[,i] <- impact_dec_acc(C[i],n,n_datasets)}

VI_2 <- matrix(NA,4,length(C))
for(i in 1:length(C)){
  VI_2[,i] <- impact_dec_impur(C[i],n,n_datasets)}


##### PLOT GENERATION PART #####
# This section includes code for plotting all figures in the report

# Plot random forests comparisons
rf_plot_1 <- ggplot() +
  geom_point(
    aes(x = rep.int(1:N, 1), y = mse_all[,2], colour = palette[1]), size = 1,show.legend = TRUE) +
  geom_point(
    aes(x = rep.int(1:N, 1), y = mse_wt[,2], colour = palette[3]), size = 1,show.legend = TRUE) +
  labs(title = "OOB error vs number of trees (c=0.85)",
       x = "Number of trees",
       y = "Out-of-bag error",
       colour = "Variables") + 
  theme(
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    legend.position = c(0.85, 0.8),
    legend.background = element_rect(fill=alpha('white', 0.9))) +
  scale_colour_manual(values = palette[c(1,5)], labels = c('With correlated','Without correlated'), guide = 'legend',name='Variables')

rf_plot_2 <- ggplot() +
  geom_point(
    aes(x = rep.int(1:N, 1), y = mse_all[,3], colour = palette[1]), size = 1,show.legend = TRUE) +
  geom_point(
    aes(x = rep.int(1:N, 1), y = mse_wt[,3], colour = palette[3]), size = 1,show.legend = TRUE) +
  labs(title = "OOB error vs number of trees (c=0.90)",
       x = "Number of trees",
       y = "Out-of-bag error",
       colour = "Variables") + 
  theme(
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    legend.position = c(0.85, 0.8),
    legend.background = element_rect(fill=alpha('white', 0.9))) +
  scale_colour_manual(values = palette[c(1,5)], labels = c('With correlated','Without correlated'), guide = 'legend',name='Variables')

rf_plot_3 <- ggplot() +
  geom_point(
    aes(x = rep.int(1:N, 1), y = mse_all[,4], colour = palette[1]), size = 1,show.legend = TRUE) +
  geom_point(
    aes(x = rep.int(1:N, 1), y = mse_wt[,4], colour = palette[3]), size = 1,show.legend = TRUE) +
  labs(title = "OOB error vs number of trees (c=0.95)",
       x = "Number of trees",
       y = "Out-of-bag error",
       colour = "Variables") + 
  theme(
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    legend.position = c(0.85, 0.8),
    legend.background = element_rect(fill=alpha('white', 0.9))) +
  scale_colour_manual(values = palette[c(1,5)], labels = c('With correlated','Without correlated'), guide = 'legend',name='Variables')

rf_plot_4 <- ggplot() +
  geom_point(
    aes(x = rep.int(1:N, 1), y = mse_all[,5], colour = palette[1]), size = 1,show.legend = TRUE) +
  geom_point(
    aes(x = rep.int(1:N, 1), y = mse_wt[,5], colour = palette[3]), size = 1,show.legend = TRUE) +
  labs(title = "OOB error vs number of trees (c=0.99)",
       x = "Number of trees",
       y = "Out-of-bag error",
       colour = "Variables") + 
  theme(
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    legend.position = c(0.85, 0.8),
    legend.background = element_rect(fill=alpha('white', 0.9))) +
  scale_colour_manual(values = palette[c(1,5)], labels = c('With correlated','Without correlated'), guide = 'legend',name='Variables')


ggarrange(rf_plot_1, rf_plot_2, rf_plot_3, rf_plot_4, ncol=2, nrow=2)

# Plot variable importance as function of corr. coeff.
corr_plot_1 <- ggplot() +
  geom_line(
    aes(x = C, y = VI_1[1,],colour = palette[1]),
    size = 1,show.legend = TRUE) +
  geom_line(
    aes(x = C, y = VI_1[2,],colour = palette[4]),
    size = 1,show.legend = TRUE) +
  geom_line(
    aes(x = C, y = VI_1[3,],colour = palette[5]),
    size = 1,show.legend = TRUE) +
  geom_line(
    aes(x = C, y = VI_1[4,],colour = palette[7]),
    size = 1,show.legend = TRUE) +
  ggtitle("Decrease in accuracy vs correlation coefficient") +
  scale_x_continuous("Correlation") +
  scale_y_continuous("Decrease in accuracy") +
  scale_colour_manual(values = palette[c(1,4,5,6)],labels = c('X1','X2','X3','X4'), guide = 'legend',name='')

corr_plot_2 <- ggplot() +
  geom_line(
    aes(x = C, y = VI_2[1,],colour = palette[1]),
    size = 1,show.legend = TRUE) +
  geom_line(
    aes(x = C, y = VI_2[2,],colour = palette[4]),
    size = 1,show.legend = TRUE) +
  geom_line(
    aes(x = C, y = VI_2[3,],colour = palette[5]),
    size = 1,show.legend = TRUE) +
  geom_line(
    aes(x = C, y = VI_2[4,],colour = palette[7]),
    size = 1,show.legend = TRUE) +
  ggtitle("Decrease in Gini impurity vs correlation coefficient") +
  scale_x_continuous("Correlation") +
  scale_y_continuous("Decrease in Gini impurity") +
  scale_colour_manual(values = palette[c(1,4,5,6)],labels = c('X1','X2','X3','X4'), guide = 'legend',name='')

ggarrange(corr_plot_1, corr_plot_2, ncol=2, nrow=1)
