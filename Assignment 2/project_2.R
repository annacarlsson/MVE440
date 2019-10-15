#################################################################
# PROJECT 2: Flexible/non-convex shapes of clusters
# Updated: 2019-06-01
# Author: Anna Carlsson
#################################################################

##### Set working directory and load packages #####
setwd("/Users/anna/Dokument/GitHub/MVE440/Exam/Datasets/")
library(dplyr)
library(ggplot2)
library(gplots)
library(ggpubr)
library(mlbench)
library(dbscan)
library(reticulate)
library(farff)
library(mclust)
#library(mda)
#library(factoextra)

palette <- c(
  "#08589e","#2b8cbe","#4eb3d3","#7bccc4","#a8ddb5",  "#ccebc5", "#e0f3db", "#f7fcf0","#cc5260","#FCF4C7","#19605D","#D57A66","#070707","#D37B64","#F6BD60","#DB3069","#FF773D","#49111C")

set.seed(641) # For reproducible results

##### DATASETS #####
# Moon dataset
n <- 1000
noise <-rnorm(n, 3, .2)
theta1 = runif(n, 0, pi)
theta2 = runif(n, pi, 2*pi)

x1 = as.data.frame(noise * cos(theta1))
x2 = as.data.frame(3 + noise * cos(theta2))

y1 = as.data.frame(-1 + noise * sin(theta1))
y2 = as.data.frame(1 + noise * sin(theta2))
y <- as.data.frame(mapply(c, y1,y2))
x <- as.data.frame(mapply(c, x1,x2))

class1 <- as.data.frame(replicate(1000,0))
class2 <- as.data.frame(replicate(1000,1))
classes <- as.data.frame(mapply(c,class1,class2))

data_moons <- bind_cols(x,y,classes)
data_moons <- data_moons[,-4]
colnames(data_moons) <- c("x","y","CLASS")
data_moons$CLASS <- as.factor(data_moons$CLASS)

# Spiral dataset
data_spiral <- mlbench.spirals(2000, cycles=1, sd=0.03)
data_spiral <- bind_cols(as.data.frame(data_spiral$x[,1]),as.data.frame(data_spiral$x[,2]),as.data.frame(data_spiral$classes))
colnames(data_spiral) <- c("x","y","CLASS")

# Strange shapes dataset
data_strange <- readARFF('cluto-t4-8k.arff')

# Varying density dataset
np <- import("numpy")
data_density <- as.data.frame(np$load("clusterable_data.npy"))

##### K-MEANS #####
# Moons data
moons_kmeans <- kmeans(data_moons[,1:2], 2)$cluster
moons_kmeans <- as.factor(moons_kmeans)

# Spiral data
spiral_kmeans <- kmeans(data_spiral[,1:2], 2)$cluster
spiral_kmeans <- as.factor(spiral_kmeans)

# Strange data
strange_kmeans <- kmeans(data_strange[,1:2], 6)$cluster
strange_kmeans <- as.factor(strange_kmeans)

# Varied density data
density_kmeans <- kmeans(data_density, 6)$cluster
density_kmeans <- as.factor(density_kmeans)

#### HIERARCHICAL CLUSTERING #####
# Moons data
moons_hc <- hclust(dist(data_moons[,1:2]), method = "single")
moons_hc <- cutree(moons_hc, k = 2)
moons_hc <- as.factor(moons_hc)

# Spiral data
spiral_hc <- hclust(dist(data_spiral[,1:2]), method = "single")
spiral_hc <- cutree(spiral_hc, k = 2)
spiral_hc <- as.factor(spiral_hc)

# Strange data
strange_hc <- hclust(dist(data_strange[,1:2]), method = "average")
strange_hc <- cutree(strange_hc, k = 6)
strange_hc <- as.factor(strange_hc)

# Varied density data
density_hc <- hclust(dist(data_density), method = "average")
density_hc <- cutree(density_hc, k = 6)
density_hc <- as.factor(density_hc)

#### GAUSSIAN MIXTURE MODELS #####
# Moons data
moons_gmm <- Mclust(data_moons[,1:2],2)
moons_gmm <- as.factor(moons_gmm$classification)

# Spiral data
spiral_gmm <- Mclust(data_spiral[,1:2],2)
spiral_gmm <- as.factor(spiral_gmm$classification)

# Strange data
strange_gmm <- Mclust(data_strange[,1:2],6)
strange_gmm <- as.factor(strange_gmm$classification)

# Varied density data
density_gmm <- Mclust(data_density,6)
density_gmm <- as.factor(density_gmm$classification)

##### DBSCAN #####
# Moons data
moons_dbscan <- dbscan::dbscan(data_moons[,1:2], eps = 0.3, minPts = 5)
moons_dbscan <- as.factor(moons_dbscan$cluster)

# Spiral data
spiral_dbscan <- dbscan::dbscan(data_spiral[,1:2], eps = 0.1, minPts = 10)
spiral_dbscan <- as.factor(spiral_dbscan$cluster)

# Strange data
strange_dbscan <- dbscan(data_strange[,1:2], eps = 7, minPts = 10)
strange_dbscan <- as.factor(strange_dbscan$cluster)

# Varied density data 
density_dbscan <- dbscan(data_density, eps = 0.025, minPts = 15)
density_dbscan <- as.factor(density_dbscan$cluster)

##### HDBSCAN #####
# Moons data
moons_hdbscan <- hdbscan(data_moons[,1:2], minPts = 5)
moons_hdbscan <- as.factor(moons_hdbscan$cluster)

# Spiral data
spiral_hdbscan <- hdbscan(data_spiral[,1:2], minPts = 5)
spiral_hdbscan <- as.factor(spiral_hdbscan$cluster)

# Strange data
strange_hdbscan <- hdbscan(data_strange[,1:2], minPts = 20)
strange_hdbscan <- as.factor(strange_hdbscan$cluster)

# Varied density data 
density_hdbscan <- hdbscan(data_density[,1:2], minPts = 15)
density_hdbscan <- as.factor(density_hdbscan$cluster)

##### PLOT GENERATION PART #####
# Moons data
plot_moons_correct <- ggplot(data_moons) + 
  geom_point(aes(x=x, y=y,color=data_moons$CLASS),size=1,show.legend = FALSE) + 
  labs(title = "Labelled data",
       x = "x",
       y = "y",
       colour = "Clusters") + 
  theme(
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12)) +
  scale_colour_manual(values = palette[c(1,4,9)])

plot_moons_kmeans <- ggplot(data_moons) + 
  geom_point(aes(x=x, y=y,color=moons_kmeans),size=1,show.legend = FALSE) + 
  labs(title = "K-means",
       x = "x",
       y = "y",
       colour = "Clusters") + 
  theme(
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12)) +
  scale_colour_manual(values = palette[c(1,4,9)])

plot_moons_hc <- ggplot(data_moons) + 
  geom_point(aes(x=x, y=y,color=moons_hc),size=1,show.legend = FALSE) + 
  labs(title = "Hierarchical clustering (single linkage)",
       x = "x",
       y = "y",
       colour = "Clusters") + 
  theme(
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12)) +
  scale_colour_manual(values = palette[c(1,4,9)])

plot_moons_gmm <- ggplot(data_moons) + 
  geom_point(aes(x=x, y=y,color=moons_gmm),size=1,show.legend = FALSE) + 
  labs(title = "Gaussian mixture model",
       x = "x",
       y = "y",
       colour = "Clusters") + 
  theme(
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12)) +
  scale_colour_manual(values = palette[c(1,4,9)])

plot_moons_dbscan <- ggplot(data_moons) + 
  geom_point(aes(x=x, y=y,color=moons_dbscan),size=1,show.legend = FALSE) + 
  labs(title = "DBSCAN",
       x = "x",
       y = "y",
       colour = "Clusters") + 
  theme(
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12)) +
  scale_colour_manual(values = palette[c(1,4,9)])

plot_moons_hdbscan <- ggplot(data_moons) + 
  geom_point(aes(x=x, y=y,color=moons_hdbscan),size=1,show.legend = FALSE) + 
  labs(title = "HDBSCAN",
       x = "x",
       y = "y",
       colour = "Clusters") + 
  theme(
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12)) +
  scale_colour_manual(values = palette[c(1,4,9)])

ggarrange(plot_moons_correct, plot_moons_kmeans, plot_moons_hc, plot_moons_gmm, plot_moons_dbscan, plot_moons_hdbscan, ncol = 3, nrow = 2)

# Spiral data
plot_spiral_labelled <- ggplot(data_spiral) + 
  geom_point(aes(x=x, y=y,color=data_spiral$CLASS),size=1,show.legend = FALSE) + 
  labs(title = "Labelled data",
       x = "x",
       y = "y",
       colour = "Clusters") + 
  theme(
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12)) +
  scale_colour_manual(values = palette[c(1,5)])

plot_spiral_kmeans <- ggplot(data_spiral) + 
  geom_point(aes(x=x, y=y,color=spiral_kmeans),size=1,show.legend = FALSE) + 
  labs(title = "K-means",
       x = "x",
       y = "y",
       colour = "Clusters") + 
  theme(
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12)) +
  scale_colour_manual(values = palette[c(1,5)])

plot_spiral_hc <- ggplot(data_spiral) + 
  geom_point(aes(x=x, y=y,color=spiral_hc),size=1,show.legend = FALSE) + 
  labs(title = "Hierarchical clustering (single linkage)",
       x = "x",
       y = "y",
       colour = "Clusters") + 
  theme(
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12)) +
  scale_colour_manual(values = palette[c(1,5)])

plot_spiral_gmm <- ggplot(data_spiral) + 
  geom_point(aes(x=x, y=y,color=spiral_gmm),size=1,show.legend = FALSE) + 
  labs(title = "Gaussian mixture model",
       x = "x",
       y = "y",
       colour = "Clusters") + 
  theme(
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12)) +
  scale_colour_manual(values = palette[c(1,5)])

plot_spiral_dbscan <- ggplot(data_spiral) + 
  geom_point(aes(x=x, y=y,color=spiral_dbscan),size=1,show.legend = FALSE) + 
  labs(title = "DBSCAN",
       x = "x",
       y = "y",
       colour = "Clusters") + 
  theme(
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12)) +
  scale_colour_manual(values = palette[c(1,5)])

plot_spiral_hdbscan <- ggplot(data_spiral) + 
  geom_point(aes(x=x, y=y,color=spiral_hdbscan),size=1,show.legend = FALSE) + 
  labs(title = "HDBSCAN",
       x = "x",
       y = "y",
       colour = "Clusters") + 
  theme(
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12)) +
  scale_colour_manual(values = palette[c(1,5)])

ggarrange(plot_spiral_labelled, plot_spiral_kmeans, plot_spiral_hc, plot_spiral_gmm, plot_spiral_dbscan, plot_spiral_hdbscan, ncol = 3, nrow = 2)

# Strange data
plot_strange_labelled <- ggplot(data_strange) + 
  geom_point(aes(x=x, y=y,color=data_strange$CLASS),size=1,show.legend = FALSE) + 
  labs(title = "Labelled data",
       x = "x",
       y = "y",
       colour = "Clusters") + 
  theme(
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12)) +
  scale_colour_manual(values = palette[c(1,2,3,5,6,9,11)])

plot_strange_kmeans <- ggplot(data_strange) + 
  geom_point(aes(x=x, y=y,color=strange_kmeans),size=1,show.legend = FALSE) + 
  labs(title = "K-means",
       x = "x",
       y = "y",
       colour = "Clusters") + 
  theme(
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12)) +
  scale_colour_manual(values = palette[c(1,2,3,5,6,9,11)])

plot_strange_hc <- ggplot(data_strange) + 
  geom_point(aes(x=x, y=y,color=strange_hc),size=1,show.legend = FALSE) + 
  labs(title = "Hierarchical clustering (average linkage)",
       x = "x",
       y = "y",
       colour = "Clusters") + 
  theme(
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12)) +
  scale_colour_manual(values = palette[c(1,2,3,5,6,9,11)])

plot_strange_gmm <- ggplot(data_strange) + 
  geom_point(aes(x=x, y=y,color=strange_gmm),size=1,show.legend = FALSE) + 
  labs(title = "Gaussian mixture model",
       x = "x",
       y = "y",
       colour = "Clusters") + 
  theme(
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12)) +
  scale_colour_manual(values = palette[c(1,2,3,5,6,9,11)])

plot_strange_dbscan <- ggplot(data_strange) + 
  geom_point(aes(x=x, y=y,color=strange_dbscan),size=1,show.legend = FALSE) + 
  labs(title = "DBSCAN",
       x = "x",
       y = "y",
       colour = "Clusters") + 
  theme(
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12)) +
  scale_colour_manual(values = palette)

plot_strange_hdbscan <- ggplot(data_strange) + 
  geom_point(aes(x=x, y=y,color=strange_hdbscan),size=1,show.legend = FALSE) + 
  labs(title = "HDBSCAN",
       x = "x",
       y = "y",
       colour = "Clusters") + 
  theme(
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12)) +
  scale_colour_manual(values = palette[c(1,2,3,5,6,9,11)])

ggarrange(plot_strange_labelled, plot_strange_kmeans, plot_strange_hc, plot_strange_gmm, plot_strange_dbscan, plot_strange_hdbscan, ncol = 3, nrow = 2)

# Density data
plot_density_labelled <- ggplot(data_density) + 
  geom_point(aes(x=V1, y=V2,color=palette[3]), size=1, show.legend = FALSE) + 
  labs(title = "Unlabelled data",
       x = "x",
       y = "y",
       colour = "Clusters") + 
  theme(
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12)) + 
  scale_colour_manual(values = palette[3])

plot_density_kmeans <- ggplot(data_density) + 
  geom_point(aes(x=V1, y=V2,color=density_kmeans),size=1, show.legend = FALSE) + 
  labs(title = "K-means",
       x = "x",
       y = "y",
       colour = "Clusters") + 
  theme(
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12)) +
  scale_colour_manual(values = palette[c(1,3,5,9,16,12,15,17)])

plot_density_hc <- ggplot(data_density) + 
  geom_point(aes(x=V1, y=V2,color=density_hc),size=1, show.legend = FALSE) + 
  labs(title = "Hierarchical clustering (average linkage)",
       x = "x",
       y = "y",
       colour = "Clusters") + 
  theme(
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12)) +
  scale_colour_manual(values = palette[c(1,3,5,9,16,12,15,17)])

plot_density_gmm <- ggplot(data_density) + 
  geom_point(aes(x=V1, y=V2,color=density_gmm),size=1, show.legend = FALSE) + 
  labs(title = "Gaussian mixture model",
       x = "x",
       y = "y",
       colour = "Clusters") + 
  theme(
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12)) +
  scale_colour_manual(values = palette[c(1,3,5,9,16,12,15,17)])

plot_density_dbscan <- ggplot(data_density) + 
  geom_point(aes(x=V1, y=V2,color=density_dbscan),size=1,show.legend = FALSE) + 
  labs(title = "DBSCAN",
       x = "x",
       y = "y",
       colour = "Clusters") + 
  theme(
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12)) +
  scale_colour_manual(values = palette[c(1,3,5,9,16,12,15,17)])

plot_density_hdbscan <- ggplot(data_density) + 
  geom_point(aes(x=V1, y=V2,color=density_hdbscan),size=1,show.legend = FALSE) + 
  labs(title = "HDBSCAN",
       x = "x",
       y = "y",
       colour = "Clusters") + 
  theme(
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12)) +
  scale_colour_manual(values = palette[c(1,3,5,9,16,12,15,17)])

ggarrange(plot_density_labelled, plot_density_kmeans, plot_density_hc, plot_density_gmm, plot_density_dbscan, plot_density_hdbscan, ncol = 3, nrow = 2)

