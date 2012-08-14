source("~/Dropbox/Dworkin_lab/WillPitchers/Will_FUNCTIONS.R")

setwd("~/Documents/RadishData/MarkerData/SSRsnp/")

PCA.dat1 <- read.table("Marker.pca", skip=11)

str(PCA.dat1)

labels.dat <- read.table("~/Documents/RadishData/MarkerData/SSRsnp/MarkerPopEdit.txt", col.names=c("Individual", "Type", "Pop", "Species", "Veins", "Vernalization"))

pca.lab <- data.frame(PCA.dat1, labels.dat)

Symbols <- c(13,16,6,1)
Colors <- c("black","deeppink")
str(pca.lab)
par(mfrow=c(1,1))
par(mar=c(5,5,4,2))
plot(pca.lab$V1, pca.lab$V2,
  pch=Symbols[pca.lab$Vernalization], col=Colors[pca.lab$Veins], 
  xlab="PC1", ylab="PC2", cex.lab=1.8, cex=1.2)
cbind(Symbols[pca.lab$Vernalization],pca.lab$Pop)
cbind(Colors[pca.lab$Veins],pca.lab$Pop)
  
legend(-0.12, -0.06, levels(pca.lab$Vernalization), 
  pch=Symbols, bty="n", cex=1.3, 
  title="Vernalization Requirement", y.intersp=1.5)
legend(-0.06, -0.082, levels(pca.lab$Veins), pch=16, 
  col=Colors, bty="n", cex=1.3, 
  title="Vein Color", y.intersp=1.5)
  c("o","+","?","~")
 