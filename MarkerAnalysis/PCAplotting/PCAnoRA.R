#Plots eigenvalues from Marker.pca output file from SmartPCA by Eigensoft. Overlays data with colors denoting the subspecies, type, DTF or other life history traits. These data are a subset of the 2005 marker data that omits all RA### genotyping. 

source("~/Dropbox/Dworkin_lab/WillPitchers/Will_FUNCTIONS.R")
source('~/Documents/RadishData/RadishScripts/Misc_scripts/AmandaSource.R', chdir = TRUE)


setwd("~/Documents/RadishData/2005MarkerData/")


PCA.dat <- read.table("SmartPCAnoRA/Marker.pca", skip=11)

str(PCA.dat)

labels.dat <- read.table("MarkerPopEdit.txt", col.names=c("Individual", "Type", "Pop", "Species", "Color", "Vernalization", "DTF", "Bins"))

pca.lab <- data.frame(PCA.dat, labels.dat[grep("RA\\d\\d\\d", labels.dat$Pop, invert=TRUE),]) 

pca.lab <- pca.lab[(row.names(pca.lab)!="345"),] #This plant has all "0" values from SmartPCA
pca.lab$new.name <- factor( paste(pca.lab$Species, pca.lab$Pop, sep="," ) )
pca.lab <- droplevels(pca.lab)

Crop.col <- "gray0"
Native.col <- "dodgerblue4"
Weedy.col <- "firebrick"

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
  pch=c(confusus, Daikon, European, landra, maritimus, Oilseed, raphanistrum, Rattail, rostratus, UnknownSp)[pca.lab$Species], col=c(Crop.col, Native.col, Weedy.col)[pca.lab$Type], 
  xlim=range( pca.lab$V1), ylim=range( pca.lab$V2),
  xlab= "PC1", ylab= "PC2" )   

legend(0.05, -0.13, levels(pca.lab$Type), pch=16, col=c(Crop.col, Native.col, Weedy.col), bty="n")

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

par(new=FALSE, mfrow=c(1,1), mar=c(5,6,6,4))

species.order <- pca.lab[order(pca.lab$Species),]

Weed.data <- species.order[species.order$Type=="Weedy",]
Native.data <- species.order[species.order$Type=="Native",]
Crop.data <- species.order[species.order$Type=="Crop",]

Weed.sym <- c(1:length(levels(droplevels(Weed.data$Pop))))
Native.sym <- c(1:length(levels(droplevels(Native.data$Pop))))
Crop.sym <- c(1:length(levels(droplevels(Crop.data$Pop))))

plot(species.order$V1, species.order$V2, type="n", 
	xlab="PCA1", ylab="PCA2", cex.lab=1.5, ylim=c(-0.32, 0.15) )

par(new=TRUE)
plot(Weed.data$V1, Weed.data$V2, 
	pch=Weed.sym[droplevels(Weed.data$new.name)], col=Weedy.col, 
	xlim=range(species.order$V1), ylim=c(-0.32, 0.15), 
	axes=FALSE, xlab="", ylab="", cex=1.5)

par(new=TRUE)	
plot(Native.data$V1, Native.data$V2, 
	pch=Native.sym[droplevels(Native.data$new.name)], col=Native.col,
	xlim=range(species.order$V1), ylim=c(-0.32, 0.15), 
	axes=FALSE, xlab="", ylab="", cex=1.5)

par(new=TRUE)
plot(Crop.data$V1, Crop.data$V2, 
	pch=Crop.sym[droplevels(Crop.data$new.name)], col=Crop.col, 
	xlim=range(species.order$V1), ylim=c(-0.32, 0.15), 
	axes=FALSE, xlab="", ylab="", cex=1.5)


legend(-0.03, -0.2, legend=levels(droplevels(Weed.data$new.name)), 
	pch=Weed.sym, col=Weedy.col, title="Weedy", cex=1.2)

legend(-0.09, -0.12, legend=levels(droplevels(Crop.data$new.name)), 
	pch=Crop.sym, col=Crop.col, title="Crop", cex=1.2)  

legend(0.035, -0.225, legend=levels(droplevels(Native.data$new.name)), 
	pch=Native.sym, col=Native.col, title="Native", cex=1.2)



