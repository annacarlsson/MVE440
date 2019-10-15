#################################################################
# PROJECT 3/4: Data generation
# Updated: 2019-06-05
# Author: Anna Carlsson
#################################################################

##### SET WORKING DIRECTORY AND LOAD PACKAGES #####
setwd("/Users/anna/Dokument/GitHub/MVE440/Exam/Datasets")
library(clusterGeneration);

##### CLUSTER SETTINGS #####
non_noisy <- 15
noisy <- 15
n_var <- non_noisy + noisy
num_clust <- 4
sep_val <- 0.001 #0.001, 0.1, 0.3
outliers <- 0.2

name <- "test"

##### GENERATE CLUSTER #####
data <- genRandomClust(numClust=num_clust,
                       sepVal=sep_val,
                       numNonNoisy=non_noisy,
                       numNoisy=noisy,
                       numOutlier=outliers,
                       numReplicate=1,
                       fileName=name,
                       clustszind=3,
                       clustSizes=c(1000,1000,1000,1000),
                       covMethod=c("eigen", "onion", "c-vine", "unifcorrmat"),
                       rangeVar=c(1, 10),
                       lambdaLow=5,
                       ratioLambda=50,
                       alphad=1,
                       eta=1,
                       rotateind=TRUE,
                       iniProjDirMethod=c("SL", "naive"),
                       projDirMethod=c("newton", "fixedpoint"),
                       alpha=0.05,
                       ITMAX=20,
                       eps=1.0e-10,
                       quiet=TRUE,
                       outputDatFlag=FALSE,
                       outputLogFlag=FALSE,
                       outputEmpirical=TRUE,
                       outputInfo=FALSE)


##### SAVE TO .CSV #####
artificial_data <- data.frame(data$memList$test_1,data$datList$test_1)
names(artificial_data)[1] <- "label"
write.csv(artificial_data, file = "hard.csv",row.names=FALSE)

