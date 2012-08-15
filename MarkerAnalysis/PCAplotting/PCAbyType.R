#Plots eigenvalues from Marker.pca output file from SmartPCA by Eigensoft. Overlays data with colors denoting the subspecies and presumed life history. 
#Some of these plots remove RA plant data without re-calculating the eigenvalues!!

source("~/Dropbox/Dworkin_lab/WillPitchers/Will_FUNCTIONS.R")

setwd("~/Documents/RadishData/2005MarkerData/SmartPCA_SSRsnp/")

PCA.dat1 <- read.table("Marker.pca", skip=11)

str(PCA.dat1)

# labels.dat1 <- read.table("Marker.ind", col.names= c("Individual", "Type", "Pop"))

labels.dat2 <- read.table("~/Documents/RadishData/2005MarkerData/MarkerPopEdit.txt", col.names=c("Individual", "Type", "Pop", "Species", "Color", "Vernalization", "DTF", "Bins"))

pca.lab <- data.frame(PCA.dat1, labels.dat2)

str(pca.lab)
Crop <- "gray0"
Native <- "dodgerblue4"
Weedy <- "firebrick"
UnknownType <- "gray"

confusus <- 1
Daikon <- 15
European <- 7
landra <- 3
maritimus <- 17
Oilseed <- 2
raphanistrum <- 16
Rattail <- 0
rostratus <- 4
UnknownSp <- 5

pca.lab <- pca.lab[(pca.lab$Pop!="NZIL"),]
pca.lab <- pca.lab[(row.names(pca.lab)!="345"),]
pca.lab <- droplevels(pca.lab)

plot( pca.lab$V1, pca.lab$V2, 
  pch=c(Daikon, European, landra, maritimus, Oilseed, raphanistrum, Rattail, rostratus, UnknownSp)[pca.lab$Species], col=c(Crop, Native, UnknownType, Weedy)[pca.lab$Type], 
  xlim=range( pca.lab$V1), ylim=range( pca.lab$V2),
  xlab= "PC1", ylab= "PC2" )
  

legend(-0.07, -0.08, levels(pca.lab$Type), pch=16, col=c(Crop, Native, UnknownType, Weedy), bty="n")

legend(-0.12, -0.06, legend=c(
	expression(italic("landra")), 
	expression(italic("maritimus")),
	expression(italic("raphanistrum")), 
	expression(italic("rostratus"))),
	pch=c(landra, maritimus, raphanistrum, rostratus), bty ="n")

legend(-0.12, -0.105, legend=c(	
	"Daikon", 
	"European",
	"Oilseed", 
	"Rattail",
	"Unknown"),
	pch=c(Daikon, European, Oilseed, Rattail, UnknownSp), bty="n")
  
  
#subset of data without RA plants, confusus

known.subset <- pca.lab[(pca.lab$Type!="UnknownType"),] 
known.subset <- droplevels(known.subset)
plot( known.subset$V1, known.subset$V2, pch=c(Daikon, European, landra, maritimus, Oilseed, raphanistrum, Rattail, rostratus)[known.subset$Species], 
  col=c(Crop, Native, Weedy)[known.subset$Type], 
  xlim=range(pca.lab$V1), ylim=range(pca.lab$V2),
  xlab= "PC1", ylab= "PC2", cex=1.2 )

legend(-0.075, -0.08, levels(known.subset$Type), pch=16, col=c(Crop, Native, Weedy), bty="n", cex=1.2)


legend(-0.13, -0.028, legend=c(
	#expression(italic("confusus")),
	expression(italic("landra")), 
	expression(italic("maritimus")),
	expression(italic("raphanistrum")),
	expression(italic("rostratus"))),
	pch=c(landra, maritimus, raphanistrum, rostratus), bty="n", cex=1.2)
	
legend(-0.13, -0.089, legend=c(	
	"Daikon",
	"European",
	"Oilseed", 
	"Rattail"),
	pch=c(Daikon, European, Oilseed, Rattail), bty="n", cex=1.2)

#plot of just the crops and natives

known.subset <- pca.lab[(pca.lab$Type!="Weedy"),] 
known.subset <- known.subset[(known.subset$Type!="UnknownType"),]
known.subset <- droplevels(known.subset)

plot( known.subset$V1, known.subset$V2, 
  pch=c(Daikon, European, landra, maritimus, Oilseed, raphanistrum, Rattail, rostratus)[known.subset$Species], 
  col=c(Crop, Native)[known.subset$Type], 
  xlim=range(pca.lab$V1), ylim=range(pca.lab$V2),
  xlab= "PC1", ylab= "PC2" )

legend(-0.13, -0.06, legend=c(
	#expression(italic("confusus")),
	expression(italic("landra")),
	expression(italic("maritimus")),
	expression(italic("raphanistrum")),
	expression(italic("rostratus"))),
	pch=c(landra, maritimus, raphanistrum, rostratus), bty="n")

legend(-0.13, -0.105, legend=c(
	"Daikon",
	"European",
	"Oilseed",
	"Rattail"),
	pch=c(Daikon, European, Oilseed, Rattail), bty="n")

legend(-0.075, -0.08, levels(known.subset$Type), pch=16, col=c(Crop, Native), bty="n")
