rm(list=ls())

source('~/Dropbox/Dworkin_lab/WillPitchers/Will_FUNCTIONS.R', chdir = TRUE)
source('~/Maths/R/AmandaSource.R', chdir = TRUE)

setwd("~/Documents/RadishData/MarkerData/STRUCTURE/Knumber/500000-1M")

ClusterData1 <- read.csv("InferCluster_16_run1.txt", header=TRUE)

# ClusterData1 <- read.csv("InferCluster_16_run1_I.txt", header=TRUE)
# row.names(ClusterData1) <- ClusterData1[,1]
# ClusterData1 <- ClusterData1[,2:474]

# ClusterData2 <- read.csv("InferCluster_13_run2_I.txt", header=TRUE, colClasses=c(rep("factor", 2), rep("numeric", 13)))
# ClusterData3 <- read.csv("InferCluster_15_run3_I.txt", header=TRUE, colClasses=c(rep("factor", 2), rep("numeric", 15)))
# ClusterData4 <- read.csv("InferCluster_18_run4_I.txt", header=TRUE, colClasses=c(rep("factor", 2), rep("numeric", 18)))
# ClusterData5 <- read.csv("InferCluster_18_run5_I.txt", header=TRUE, colClasses=c(rep("factor", 2), rep("numeric", 18)))

# Labels1 <- read.csv("InferCluster_16_run1_I_pops.txt")
# cbind(ClusterData1$Label, Labels1$Pop)
ClusterData1$Pop <- factor(ClusterData1$Pop)
ClusterData1 <- ClusterData1[ order( ClusterData1$Pop ), ]
str(ClusterData1)
barplot( t( ClusterData1[,3:18] ), beside=F, col= c(1:16), xlab="", xaxt="n")



barplot(as.matrix(ClusterData1[2:17,2:473]), col= c(1:16), xlab="", xaxt="n")
barplot(as.matrix(ClusterData1[2:17,2:473]), col= c(1:16)[row.names(ClusterData1)[2:17]], xlab="", xaxt="n")
running.total <- rep(NA, 48)
for(i in 1:48){ running.total[i] <- sum( table(t(ClusterData1[1,]))[i], table(t(ClusterData1[1,]))[1:i] ) }
axis( side=1, at=running.total ,labels=1:48 )




