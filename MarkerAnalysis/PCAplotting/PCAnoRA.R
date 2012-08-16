#Plots eigenvalues from Marker.pca output file from SmartPCA by Eigensoft. Overlays data with colors denoting the subspecies, type, DTF or other life history traits. These data are a subset of the 2005 marker data that omits all RA### genotyping. 

source("~/Dropbox/Dworkin_lab/WillPitchers/Will_FUNCTIONS.R")
source('~/Documents/RadishData/RadishScripts/Misc_scripts/AmandaSource.R', chdir = TRUE)


setwd("~/Documents/RadishData/2005MarkerData/")


PCA.dat <- read.table("SmartPCAnoRA/Marker.pca", skip=11)

str(PCA.dat)

labels.dat <- read.table("MarkerPopEdit.txt", col.names=c("Individual", "Type", "Pop", "Species", "Color", "Vernalization", "DTF", "Bins"))

pca.lab <- data.frame(PCA.dat, labels.dat[grep("RA\\d\\d\\d", labels.dat$Pop, invert=TRUE),]) 

pca.lab <- pca.lab[(row.names(pca.lab)!="345"),] #This plant has all "0" values from SmartPCA

pca.lab <- droplevels(pca.lab)

Crop <- "gray0"
Native <- "dodgerblue4"
Weedy <- "firebrick"

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

#plotbyType

plot( pca.lab$V1, pca.lab$V2, 
  pch=c(confusus, Daikon, European, landra, maritimus, Oilseed, raphanistrum, Rattail, rostratus, UnknownSp)[pca.lab$Species], col=c(Crop, Native, Weedy)[pca.lab$Type], 
  xlim=range( pca.lab$V1), ylim=range( pca.lab$V2),
  xlab= "PC1", ylab= "PC2" )   

legend(0.05, -0.13, levels(pca.lab$Type), pch=16, col=c(Crop, Native, Weedy), bty="n")

legend(-0.035, -0.085, legend=c(
	expression(italic("confusus")), 
	expression(italic("landra")), 
	expression(italic("maritimus")),
	expression(italic("raphanistrum")), 
	expression(italic("rostratus"))),
	pch=c(confusus, landra, maritimus, raphanistrum, rostratus), bty ="n")

legend(-0.035, -0.15, legend=c(	
	"Daikon", 
	"European",
	"Oilseed", 
	"Rattail"),
	pch=c(Daikon, European, Oilseed, Rattail), bty="n")
	
	
	
#PlotbyPopulation

Weedsym <- c(1:8)
Nativesym <- c(1:6)
Cropsym <- c(1:14)

par(new=FALSE, mfrow=c(1,1), mar=c(5,6,6,4))


plot(pca.lab$V1, pca.lab$V2, type="n", xlab="PCA1", ylab="PCA2", cex.lab=1.5 )

par(new=TRUE)
plot(pca.lab$V1[pca.lab$Type=="Weedy"], pca.lab$V2[pca.lab$Type=="Weedy"], 
	pch=Weedsym[droplevels(pca.lab$Pop[pca.lab$Type=="Weedy"])], col=Weedy, xlim=range(pca.lab$V1), ylim=range(pca.lab$V2), 
	axes=FALSE, xlab="", ylab="", cex=1.5)

par(new=TRUE)	
plot(pca.lab$V1[pca.lab$Type=="Native"], pca.lab$V2[pca.lab$Type=="Native"], 
	pch=Nativesym[droplevels(pca.lab$Pop[pca.lab$Type=="Native"])], col=Native, xlim=range(pca.lab$V1), ylim=range(pca.lab$V2), 
	axes=FALSE, xlab="", ylab="", cex=1.5)

par(new=TRUE)
plot(pca.lab$V1[pca.lab$Type=="Crop"], pca.lab$V2[pca.lab$Type=="Crop"], 
	pch=Cropsym[droplevels(pca.lab$Pop[pca.lab$Type=="Crop"])], col=Crop, xlim=range(pca.lab$V1), ylim=range(pca.lab$V2), 
	axes=FALSE, xlab="", ylab="", cex=1.5)

legend(-0.07, -0.105, legend=levels(droplevels(pca.lab$Pop[pca.lab$Type=="Weedy"])), pch=Weedsym, col=Weedy, bty="n", title="Weedy", cex=1.5)
legend(-0.03, -0.105, legend=levels(droplevels(pca.lab$Pop[pca.lab$Type=="Crop"])), pch=Cropsym, col=Crop, bty="n", ncol=2, title="Crop", cex=1.5)  
legend(0.05, -0.105, legend=levels(droplevels(pca.lab$Pop[pca.lab$Type=="Native"])), pch=Nativesym, col=Native, bty="n", title="Native", cex=1.5)
