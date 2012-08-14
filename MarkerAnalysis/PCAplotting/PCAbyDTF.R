source("~/Dropbox/Dworkin_lab/WillPitchers/Will_FUNCTIONS.R")

setwd("~/Documents/RadishData/MarkerData/SSRsnp/")

smartpca.pca <- read.table("Marker.pca", skip=11)

str(smartpca.pca)

labels.dat <- read.table("MarkerPopEdit.txt", col.names= c("Individual", "Type", "Pop", "Species", "Color", "Vernalization", "DTF", "Bins"))

pca.lab <- data.frame(smartpca.pca, labels.dat)

pca.labD <- pca.lab[order(pca.lab$DTF, decreasing=TRUE),]

pca.labD
pca.labD$DTF <- factor(pca.labD$DTF)

str(pca.labD)
mycol <- c("snow3", rev(heat.colors(21)), "grey0")[pca.labD$DTF]
par(mfrow=c(1,1))
par(mar=c(5,5,4,2))
plot( pca.labD$V1, pca.labD$V2, pch=16, col=mycol, main="Days To Flowering", xlab="PCA1", ylab="PCA2", cex=1.3)

cbind(pca.labD$DTF, mycol)

legend(-0.11, -0.05, levels(pca.labD$DTF), col= c("snow3", rev(heat.colors(21)), "grey0"), pch=16, ncol=2, bty="n", cex=1.3 )
text(-.03,-.1, "'0' denotes un-tested DTF", cex=1.3)
text(-.03, -.12, "'999' denotes plants that never flowered", cex=1.3)


#WithoutRAs As Jeff asked

known.subset <- pca.lab[c(1:148,344:473),]
known.subset <- droplevels(known.subset)
known.subset <- known.subset[order(known.subset$Pop),]
known.subset$Bins <- factor(known.subset$Bins)

Crop <- "gray0"
Native <- "dodgerblue4"
Weedy <- "firebrick"
under45 <- 15
fortyfive85 <- 16
neverflower <- 17
par(mfrow=c(1,1))
par(mar=c(5,5,4,2))
plot(known.subset$V1, known.subset$V2, col=c(Crop, Native, Weedy)[known.subset$Type], pch=c(under45, fortyfive85, neverflower)[known.subset$Bins], xlab="PCA1", ylab="PCA2", cex=1.3)

legend(-0.105, -0.08, levels(known.subset$Type), col= c(Crop, Native, Weedy), pch=16, bty="n", cex=1.1)

legend(-0.08, -0.08, legend=c("< 45 Days", "45 to 85 Days", "> 85 Days"), col= 1, pch=c(under45, fortyfive85, neverflower), bty="n", cex=1.1)

text(-.03,-.1, "'0' denotes un-tested DTF", cex=1.3)
text(-.03, -.12, "'999' denotes plants that never flowered", cex=1.3)


#No RAs but makes more sense


known.subset <- pca.lab[c(1:148,344:473),]
known.subset <- droplevels(known.subset)
known.subset <- known.subset[order(known.subset$Pop),]
known.subset$Bins <- factor(known.subset$Bins)

Crop <- 15
Native <- 16
Weedy <- 17
under45 <- "firebrick"
fortyfive85 <- "dodgerblue4"
neverflower <- "gray0"
par(mfrow=c(1,1))
par(mar=c(5,5,4,2))
plot(known.subset$V1, known.subset$V2, col=c(under45, fortyfive85, neverflower)[known.subset$Bins], pch=c(Crop, Native, Weedy)[known.subset$Type], xlab="PCA1", ylab="PCA2", cex=1.3)

legend(-0.105, -0.08, levels(known.subset$Type), col=1, pch=c(Crop, Native, Weedy), bty="n", cex=1.1)

legend(-0.08, -0.08, legend=c("< 45 Days", "45 to 85 Days", "> 85 Days"), pch= 16, col=c(under45, fortyfive85, neverflower), bty="n", cex=1.1)






