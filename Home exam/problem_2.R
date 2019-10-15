#################################################################
# PROBLEM 2: Regression in big-n
# Updated: 2019-06-12
# Author: Anna Carlsson
#################################################################

##### SET WORKING DIRECTORY AND LOAD PACKAGES #####
setwd("/Users/anna/Dokument/GitHub/MVE440/Exam/Datasets")
options(java.parameters = "-Xmx20g") 
library(methods)
library(dplyr)
library(ggplot2)
library(gplots)
library(ggpubr)
library(Rtsne)
library(subspace)
library(rJava)
library(dbscan)
library(tibble)
library(diptest)
library(cluster)
library(factoextra)

set.seed(5532) # For reproducible results

##### Load data #####
load("exercise2.RData")
X <- as.data.frame(X)

##### Subset selection #####
# Selection of subset of features selected by PCA and Hartigans' dip test

# PCA
pca <- prcomp(X, scale. = T)
std <- pca$sdev
var <- std^2
prop_var <- var/sum(var)
cumsum <- cumsum(prop_var)

# Plot the cumulative variance 
# ggplot() +
#   geom_path(aes(x = seq(1,229,1), y = cumsum)) +
#   labs(title = "Cumulative explained variance",
#        x = "Number of PCA components",
#        y = "Explained variance",
#        colour = "Clusters") + 
#   theme_gray() +
#   theme(
#     plot.title = element_text(size = 10),
#     axis.title = element_text(size = 8),
#     axis.text = element_text(size = 8),
#     legend.title = element_text(size = 8),
#     legend.text = element_text(size = 8))

# Hartigan's Dip Test 
dip_values <- vector()
p_values <- vector()
significant <- vector()
i = 0
for (variable in 1:598){
  temp <- dip.test(X[,variable],simulate.p.value = TRUE, B=2000)
  dip_values[variable] <- temp$statistic
  p_values[variable] <- temp$p.value
  if (temp$p.value<0.05){
    significant[i] <- variable
    i = i+1
  }
}

temp <- dip.test(X[,1],simulate.p.value = TRUE, B=2000)

dip_values <- as.data.frame(t(dip_values))
p_values <- as.data.frame(t(p_values))
#write.table(significant, file="signifcant.txt", row.names=FALSE, col.names=FALSE)
#significant <- scan('signifcant.txt')

# Reduce dimension of datasets
dip_data <- X[,significant]

pca_comp <- as.matrix(pca$rotation)
pca_10_data <- as.matrix(X) %*% pca_comp[,1:10]
pca_100_data <- as.matrix(X) %*% pca_comp[,1:100]
pca_200_data <- as.matrix(X) %*% pca_comp[,1:200]


##### CLIQUE #####
# Dip data
clique <- CLIQUE(as.matrix(dip_data), xi = 20, tau = 0.27)
cl_data <- bind_cols(
  as_tibble(X),
  cluster = rep(NA, dim(X)[1]))
for(i in 1:length(clique)) {
  cl <- clique[[i]]
  cl_data[cl$objects, "cluster"] <- i
}
cluster_dip <- cl_data$cluster
sill_dip <- silhouette(x=cluster_dip,dist=dist(dip_data,method="minkowski")) 
fviz_silhouette(sill_dip,label = FALSE) +
  theme(legend.position="none") +
  labs(title="Silhouette DIP: CLIQUE",
       subtitle = "Average silhoutte width: -0.18",
       y = "Silhoutte width")

# PCA-2 data
clique <- CLIQUE(as.matrix(pca_10_data[,1:2]), xi = 5, tau = 0.06)
cl_data <- bind_cols(
  as_tibble(X),
  cluster = rep(NA, dim(X)[1]))
for(i in 1:length(clique)) {
  cl <- clique[[i]]
  cl_data[cl$objects, "cluster"] <- i
}
cluster_pca2 <- cl_data$cluster
sill_pca2 <- silhouette(x=cluster_pca2,dist=dist(pca_10_data,method="minkowski")) 
fviz_silhouette(sill_pca2,label = FALSE) +
  theme(legend.position="none") +
  labs(title="Silhouette PCA2: CLIQUE",
    subtitle = "Average silhoutte width: 0.18",
       y = "Silhoutte width")

# PCA-10 data
clique <- CLIQUE(as.matrix(pca_10_data), xi = 5, tau = 0.27)
cl_data <- bind_cols(
  as_tibble(X),
  cluster = rep(NA, dim(X)[1]))
for(i in 1:length(clique)) {
  cl <- clique[[i]]
  cl_data[cl$objects, "cluster"] <- i
}
cluster_pca10 <- cl_data$cluster
sill_pca10 <- silhouette(x=cluster_pca10,dist=dist(pca_10_data,method="minkowski")) 
fviz_silhouette(sill_pca10,label = FALSE) +
  theme(legend.position="none") +
  labs(title="Silhouette PCA10: CLIQUE",
       subtitle = "Average silhoutte width: 0.0075",
       y = "Silhoutte width")
mean(sill_pca10[,3])

# PCA-100 data
clique <- CLIQUE(as.matrix(pca_100_data), xi = 15, tau = 0.2)
cl_data <- bind_cols(
  as_tibble(X),
  cluster = rep(NA, dim(X)[1]))
for(i in 1:length(clique)) {
  cl <- clique[[i]]
  cl_data[cl$objects, "cluster"] <- i
}
cluster_pca100 <- cl_data$cluster
sill_pca100 <- silhouette(x=cluster_pca100,dist=dist(pca_100_data,method="minkowski")) 
fviz_silhouette(sill_pca100,label = FALSE) +
  theme(legend.position="none") +
  labs(title="Silhouette PCA100: CLIQUE",
       subtitle = "Average silhoutte width: -0.16",
       y = "Silhoutte width")
mean(sill_pca100[,3])

# PCA-200 data
clique <- CLIQUE(as.matrix(pca_200_data), xi = 15, tau = 0.24)
cl_data <- bind_cols(
  as_tibble(X),
  cluster = rep(NA, dim(X)[1]))
for(i in 1:length(clique)) {
  cl <- clique[[i]]
  cl_data[cl$objects, "cluster"] <- i
}
cluster_pca200 <- cl_data$cluster
sill_pca200 <- silhouette(x=cluster_pca200,dist=dist(pca_200_data,method="minkowski")) 
fviz_silhouette(sill_pca200,label = FALSE) +
  theme(legend.position="none") +
  labs(title="Silhouette PCA200: CLIQUE",
       subtitle = "Average silhoutte width: -0.052",
       y = "Silhoutte width")
mean(sill_pca200[,3])

##### ProClus #####
data <- pca_200_data
avg_sill <- vector()
for (k in 2:25){
  proclus <- ProClus(data = as.matrix(data), k = k, d = 5)
  cl_data <- bind_cols(
    as_tibble(X),
    cluster = rep(NA, dim(X)[1]))
  for(i in 1:length(proclus)) {
    cl <- proclus[[i]]
    cl_data[cl$objects, "cluster"] <- i
  }
  cluster <- cl_data$cluster
  cluster[is.na(cluster)] <- 0
  temp <- silhouette(x=cluster,dist=dist(data,method="minkowski")) 
  avg_sill[k] <- mean(temp[,3])
}

# Dipp data
proclus <- ProClus(as.matrix(dip_data), k = 3, d = 4)
cl_data <- bind_cols(
  as_tibble(X),
  cluster = rep(NA, dim(X)[1]))
for(i in 1:length(proclus)) {
  cl <- proclus[[i]]
  cl_data[cl$objects, "cluster"] <- i
}
cluster <- cl_data$cluster
NAs <- which(is.na(cluster))
sill_dipp <- silhouette(x=cluster[-NAs],dist=dist(dip_data[-NAs,],method="minkowski")) 
fviz_silhouette(sill_dipp,label = FALSE) +
  theme(legend.position="none") +
  labs(title="Silhouette DIP: ProClus",
       subtitle = "Average silhoutte width: -0.039",
       y = "Silhoutte width")
mean(sill_dipp[,3])

# PCA2
proclus <- ProClus(pca_10_data[,1:2], k = 4, d = 5)
cl_data <- bind_cols(
  as_tibble(X),
  cluster = rep(NA, dim(X)[1]))
for(i in 1:length(proclus)) {
  cl <- proclus[[i]]
  cl_data[cl$objects, "cluster"] <- i
}
cluster <- cl_data$cluster
NAs <- which(is.na(cluster))
sill_pca2 <- silhouette(x=cluster[-NAs],dist=dist(pca_10_data[-NAs,1:2],method="minkowski")) 
fviz_silhouette(sill_pca2,label = FALSE) +
  theme(legend.position="none") +
  labs(title="Silhouette PCA2: ProClus",
       subtitle = "Average silhoutte width: 0.49",
       y = "Silhoutte width")
mean(sill_pca2[,3])


# PCA10
proclus <- ProClus(pca_10_data, k = 3, d = 10)
cl_data <- bind_cols(
  as_tibble(X),
  cluster = rep(NA, dim(X)[1]))
for(i in 1:length(proclus)) {
  cl <- proclus[[i]]
  cl_data[cl$objects, "cluster"] <- i
}
cluster <- cl_data$cluster
NAs <- which(is.na(cluster))
sill_pca10<- silhouette(x=cluster[-NAs],dist=dist(pca_10_data[-NAs,],method="minkowski")) 
fviz_silhouette(sill_pca10,label = FALSE) +
  theme(legend.position="none") +
  labs(title="Silhouette PCA10: ProClus",
       subtitle = "Average silhoutte width: 0.11",
       y = "Silhoutte width")
mean(sill_pca10[,3])

# PCA100
proclus <- ProClus(pca_100_data, k = 3, d = 10)
cl_data <- bind_cols(
  as_tibble(X),
  cluster = rep(NA, dim(X)[1]))
for(i in 1:length(proclus)) {
  cl <- proclus[[i]]
  cl_data[cl$objects, "cluster"] <- i
}
cluster <- cl_data$cluster
NAs <- which(is.na(cluster))
sill_pca100<- silhouette(x=cluster[-NAs],dist=dist(pca_100_data[-NAs,],method="minkowski")) 
fviz_silhouette(sill_pca100,label = FALSE) +
  theme(legend.position="none") +
  labs(title="Silhouette PCA100: ProClus",
       subtitle = "Average silhoutte width: -0.051",
       y = "Silhoutte width")
mean(sill_pca100[,3])

# PCA200
proclus <- ProClus(pca_200_data, k = 3, d = 15)
cl_data <- bind_cols(
  as_tibble(X),
  cluster = rep(NA, dim(X)[1]))
for(i in 1:length(proclus)) {
  cl <- proclus[[i]]
  cl_data[cl$objects, "cluster"] <- i
}
cluster <- cl_data$cluster
NAs <- which(is.na(cluster))
sill_pca200<- silhouette(x=cluster[-NAs],dist=dist(pca_200_data[-NAs,],method="minkowski")) 
fviz_silhouette(sill_pca200,label = FALSE) +
  theme(legend.position="none") +
  labs(title="Silhouette PCA200: ProClus",
       subtitle = "Average silhoutte width: -0.023",
       y = "Silhoutte width")
mean(sill_pca200[,3])


##### SubClu #####
data <- pca_200_data
avg_sill <- vector()
index = 11
for (eps in seq(0.001,10,0.1)){
  subclu <- SubClu(data, epsilon = eps, minSupport = 30)
  cl_data <- bind_cols(
    as_tibble(X),
    cluster = rep(NA, dim(X)[1]))
  for(i in 1:length(subclu)) {
    cl <- subclu[[i]]
    cl_data[cl$objects, "cluster"] <- i
  }
  cluster <- cl_data$cluster
  cluster[is.na(cluster)] <- 0
  temp <- silhouette(x=cluster,dist=dist(data,method="minkowski")) 
  avg_sill[index] <- mean(temp[,3])
  index = index + 1
}
seq(0.001,10,0.1)[which.max(avg_sill)]

# DIP data
subclu <- SubClu(dip_data, epsilon = 4.801, minSupport = 30)
sc_data <- bind_cols(
  as_tibble(X),
  cluster = rep(NA, dim(X)[1]))
for(i in 1:length(subclu)) {
  sc <- subclu[[i]]
  sc_data[sc$objects, "cluster"] <- i
}
cluster <- cl_data$cluster
NAs <- which(is.na(cluster))
sill_dip<- silhouette(x=cluster[-NAs],dist=dist(dip_data[-NAs,],method="minkowski")) 
fviz_silhouette(sill_dip,label = FALSE) +
  theme(legend.position="none") +
  labs(title="Silhouette DIP: SubClu",
       subtitle = "Average silhoutte width: -0.041",
       y = "Silhoutte width")
mean(sill_dip[,3])

# PCA2
subclu <- SubClu(pca_10_data[,1:2], epsilon = 0.601, minSupport = 30)
sc_data <- bind_cols(
  as_tibble(X),
  cluster = rep(NA, dim(X)[1]))
for(i in 1:length(subclu)) {
  sc <- subclu[[i]]
  sc_data[sc$objects, "cluster"] <- i
}
cluster <- cl_data$cluster
NAs <- which(is.na(cluster))
sill_pca2 <- silhouette(x=cluster[-NAs],dist=dist(pca_10_data[-NAs,1:2],method="minkowski")) 
fviz_silhouette(sill_pca2,label = FALSE) +
  theme(legend.position="none") +
  labs(title="Silhouette PCA2: SubClu",
       subtitle = "Average silhoutte width: -0.051",
       y = "Silhoutte width")
mean(sill_pca2[,3])

# PCA10
subclu <- SubClu(pca_10_data, epsilon = 5.401, minSupport = 30)
sc_data <- bind_cols(
  as_tibble(X),
  cluster = rep(NA, dim(X)[1]))
for(i in 1:length(subclu)) {
  sc <- subclu[[i]]
  sc_data[sc$objects, "cluster"] <- i
}
cluster <- cl_data$cluster
NAs <- which(is.na(cluster))
sill_pca10<- silhouette(x=cluster[-NAs],dist=dist(pca_10_data[-NAs,],method="minkowski")) 
fviz_silhouette(sill_pca10,label = FALSE) +
  theme(legend.position="none") +
  labs(title="Silhouette PCA10: SubClu",
       subtitle = "Average silhoutte width: -0.024",
       y = "Silhoutte width")
mean(sill_pca10[,3])

# PCA100
subclu <- SubClu(pca_100_data, epsilon = 6.501, minSupport = 30)
sc_data <- bind_cols(
  as_tibble(X),
  cluster = rep(NA, dim(X)[1]))
for(i in 1:length(subclu)) {
  sc <- subclu[[i]]
  sc_data[sc$objects, "cluster"] <- i
}
cluster <- cl_data$cluster
NAs <- which(is.na(cluster))
sill_pca100<- silhouette(x=cluster[-NAs],dist=dist(pca_100_data[-NAs,],method="minkowski")) 
fviz_silhouette(sill_pca100,label = FALSE) +
  theme(legend.position="none") +
  labs(title="Silhouette PCA100: SubClu",
       subtitle = "Average silhoutte width: -0.023",
       y = "Silhoutte width")
mean(sill_pca100[,3])

# PCA200
subclu <- SubClu(pca_200_data, epsilon = 9.201, minSupport = 30)
sc_data <- bind_cols(
  as_tibble(X),
  cluster = rep(NA, dim(X)[1]))
for(i in 1:length(subclu)) {
  sc <- subclu[[i]]
  sc_data[sc$objects, "cluster"] <- i
}
cluster <- cl_data$cluster
NAs <- which(is.na(cluster))
sill_pca200<- silhouette(x=cluster[-NAs],dist=dist(pca_200_data[-NAs,],method="minkowski")) 
fviz_silhouette(sill_pca10,label = FALSE) +
  theme(legend.position="none") +
  labs(title="Silhouette PCA200: SubClu",
       subtitle = "Average silhoutte width: -0.023",
       y = "Silhoutte width")
mean(sill_pca200[,3])

##### Variable "importance" #####
# Find variables with largest loads of PC1
loads <- abs(pca$rotation[,1])
loads_sorted <- sort(loads)
loads_sorted

values_selected <- dip_values[significant]
values_selected
sort(values_selected)


##### Visualisation #####
# tSNE
data_tSNE <- Rtsne(X, dims=2, perplexity=20)
cluster <- as.factor(cluster)

ggplot() +
  geom_point(aes(x = data_tSNE$Y[,1], y = data_tSNE$Y[,2],colour=cluster),size=0.1) +
  #geom_point(aes(x = data_tSNE$Y[,1], y = data_tSNE$Y[,2]),size=0.2) +
  labs(title = "DIP: ProClus (tSNE, perplexity = 20)",
       x = "tSNE1",
       y = "tSNE2",
       colour = "Clusters") + 
  theme_gray() +
  theme(
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12))

# PCA
data_svd_raw <- svd(scale(as.matrix(X), scale = FALSE))
data_svd_raw <- scale(as.matrix(X), scale = FALSE) %*% data_svd_raw$v

PCA_1 <- data_svd_raw[,1]
PCA_2 <- data_svd_raw[,2]

ggplot() +
  #geom_point(aes(x = PCA_1, y = PCA_2, colour = cluster), size=0.1, show.legend = FALSE) +
  geom_point(aes(x = PCA_1, y = PCA_2), size=0.2, show.legend = FALSE) +
  labs(title = "Dimension reduction using PCA",
       x = "PCA1",
       y = "PCA2",
       colour = "Clusters") + 
  theme_gray() +
  theme(
    plot.title = element_text(size = 14),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12))


##### Visualising data with PCA and tSNE #####
# tSNE
data_tSNE <- Rtsne(X, dims=2, perplexity=20)

ggplot() +
  geom_point(aes(x = data_tSNE$Y[,1], y = data_tSNE$Y[,2]),size=0.2) +
  labs(title = "Dimension reduction using tSNE (perplexity = 20)",
       x = "tSNE1",
       y = "tSNE2",
       colour = "Clusters") + 
  theme_gray() +
  theme(
    plot.title = element_text(size = 14),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12))

# PCA
data_svd_raw <- svd(scale(as.matrix(X), scale = FALSE))
data_svd_raw <- scale(as.matrix(X), scale = FALSE) %*% data_svd_raw$v

PCA_1 <- data_svd_raw[,1]
PCA_2 <- data_svd_raw[,2]

ggplot() +
  geom_point(aes(x = PCA_1, y = PCA_2), size=0.2, show.legend = FALSE) +
  labs(title = "Dimension reduction using PCA",
       x = "PCA1",
       y = "PCA2",
       colour = "Clusters") + 
  theme_gray() +
  theme(
    plot.title = element_text(size = 14),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12))
