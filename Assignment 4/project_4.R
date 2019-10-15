#################################################################
# PROJECT 4: Visualisation via dimension reduction
# Updated: 2019-06-08
# Author: Anna Carlsson
#################################################################

##### SET WORKING DIRECTORY AND LOAD PACKAGES #####
setwd("/Users/anna/Dokument/GitHub/MVE440/Exam/Datasets")
library(dplyr)
library(ggplot2)
library(gplots)
library(ggpubr)
library(dimRed)
library(Rtsne)
library(ElemStatLearn)
library(NMF)

set.seed(5532) # For reproducible results

##### Datasets #####
# Easy dataset
#data <- read.csv(file="easy.csv", header=TRUE, sep=",")
#colnames(data)[1] <- "target"

# Medium dataset
#data <- read.csv(file="medium.csv", header=TRUE, sep=",")
#colnames(data)[1] <- "target"

# Hard dataset
data <- read.csv(file="hard.csv", header=TRUE, sep=",")
colnames(data)[1] <- "target"


# SPAM dataset
#data <- as.data.frame(ElemStatLearn::spam)
#data <- data[!duplicated(data),]
#colnames(data)[58] <- "target"

features <- subset(data, select=-target)
labels <- data$target
labels <- as.factor(labels)

##### tSNE #####
data_tSNE <- Rtsne(features, dims=2, perplexity=20,check_duplicates=FALSE)

plot_tsne <- ggplot() +
  geom_point(aes(x = data_tSNE$Y[,1], y = data_tSNE$Y[,2],colour = labels),size=0.1,show.legend = FALSE) +
  labs(title = "tSNE (perplexity = 20)",
       x = "tSNE1",
       y = "tSNE2",
       colour = "Clusters") + 
  theme_gray() +
  theme(
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 11),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 11))


##### PCA #####
# PCA equivalent to performing SVD on centered data
data_svd_raw <- svd(scale(as.matrix(features), scale = FALSE))
data_svd_raw <- scale(as.matrix(features), scale = FALSE) %*% data_svd_raw$v

PCA_1 <- data_svd_raw[,1]
PCA_2 <- data_svd_raw[,2]

plot_pca <- ggplot() +
  geom_point(aes(x = PCA_1, y = PCA_2, colour = labels), size=0.1, show.legend = FALSE) +
  labs(title = "PCA",
       x = "PCA1",
       y = "PCA2",
       colour = "Clusters") + 
  theme_gray() +
  theme(
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 11),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 11))


##### Kernel-PCA #####
kpc <- kpca(~., data = features, kernel="rbfdot", kpar=list(sigma=0.0001), features=2)
kpca_reduced = cbind(labels, rotated(kpc)[,1],rotated(kpc)[,2])
kpca_reduced = as.data.frame(kpca_reduced)
colnames(kpca_reduced) = c('labels','kPCA1','kPCA2')

plot_kpca <- ggplot() +
  geom_point(
    aes(x = kpca_reduced$kPCA1, y = kpca_reduced$kPCA2, colour=labels), size=0.1, show.legend = FALSE) +
  labs(title = "Kernel-PCA",
       x = "kPCA1",
       y = "kPCA2",
       colour = "Clusters") + 
  theme_gray() +
  theme(
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 11),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 11))

##### Non-negative matrix factorization #####
features_nmf <- NMF::nneg(as.matrix(features), method='min')
nmf <- nmf(features_nmf,2)
W <- nmf@fit@W
H <- nmf@fit@H
approxa <- as.matrix(features_nmf) %*% t(H)
approx <- as.data.frame(approxa)

plot_nmf <- ggplot() + 
  geom_point(
    aes(x=approx[,1], y=approx[,2], colour=labels), size = 0.1, show.legend = FALSE) + 
  labs(title = "Non-negative matrix factorization",
       x = "NMF1",
       y = "NMF2",
       colour = "Clusters") + 
  theme(
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 11))

##### Arrange plots #####
ggarrange(plot_pca, plot_kpca, plot_nmf, plot_tsne, ncol = 2, nrow = 2)

