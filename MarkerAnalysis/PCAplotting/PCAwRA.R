#Plots eigenvalues from Marker.pca output file from SmartPCA by Eigensoft. Overlays data with colors denoting the subspecies, type, DTF or other life history traits. These data are a subset of the 2005 marker data that omits all RA### genotyping. 

source("~/Dropbox/Dworkin_lab/WillPitchers/Will_FUNCTIONS.R")
source('~/Documents/RadishData/RadishScripts/Misc_scripts/AmandaSource.R', chdir = TRUE)

setwd("~/Documents/RadishData/2005MarkerData/")

PCA.dat <- read.table("SmartPCA_SSRsnp/Marker.pca", skip=11)

str(PCA.dat)

labels.dat <- read.table("MarkerPopEdit.txt", col.names=c("Individual", "Type", "Pop", "Species", "Color", "Vernalization", "DTF", "Bins"))

pca.lab <- data.frame(PCA.dat, labels.dat)
#pca.lab <- data.frame(PCA.dat, labels.dat[grep("RA\\d\\d\\d", labels.dat$Pop, invert=TRUE),]) 

pca.lab <- pca.lab[(row.names(pca.lab)!="345"),] #This plant has all "0" values from SmartPCA
pca.lab <- pca.lab[pca.lab$Pop!="NZIL",] #This pop has all "0" values from SmartPCA with the RAs
pca.lab$new.name <- factor( paste(pca.lab$Species, pca.lab$Pop, sep="," ) )
pca.lab <- droplevels(pca.lab)

Crop.col <- "gray0"
Native.col <- "dodgerblue4"
Weedy.col <- "firebrick"
Unknown.col <- "gray"
	
#PlotbyPopulation

par(new=FALSE, mfrow=c(1,1), mar=c(5,6,6,4))

species.order <- pca.lab[order(pca.lab$Species),]

Weed.data <- species.order[species.order$Type=="Weedy",]
Native.data <- species.order[species.order$Type=="Native",]
Crop.data <- species.order[species.order$Type=="Crop",]
Unknown.data <- species.order[species.order$Type=="UnknownType",]

Weed.sym <- c(1:length(levels(droplevels(Weed.data$Pop))))
Native.sym <- c(1:length(levels(droplevels(Native.data$Pop))))
Crop.sym <- c(1:length(levels(droplevels(Crop.data$Pop))))
Unknown.sym <- c(1:length(levels(droplevels(Unknown.data$Pop))))


plot(species.order$V1, species.order$V2, type="n", 
	xlab="PCA1", ylab="PCA2", cex.lab=1.5 )

par(new=TRUE)
plot(Weed.data$V1, Weed.data$V2, 
	pch=Weed.sym[droplevels(Weed.data$new.name)], col=Weedy.col, 
	xlim=range(species.order$V1), ylim=range(species.order$V2), 
	axes=FALSE, xlab="", ylab="", cex=1.5)

par(new=TRUE)	
plot(Native.data$V1, Native.data$V2, 
	pch=Native.sym[droplevels(Native.data$new.name)], col=Native.col,
	xlim=range(species.order$V1), ylim=range(species.order$V2), 
	axes=FALSE, xlab="", ylab="", cex=1.5)

par(new=TRUE)
plot(Crop.data$V1, Crop.data$V2, 
	pch=Crop.sym[droplevels(Crop.data$new.name)], col=Crop.col, 
	xlim=range(species.order$V1), ylim=range(species.order$V2), 
	axes=FALSE, xlab="", ylab="", cex=1.5)

par(new=TRUE)
plot(Unknown.data$V1, Unknown.data$V2, 
	pch=Unknown.sym[droplevels(Unknown.data$new.name)], col=Unknown.col, 
	xlim=range(species.order$V1), ylim=range(species.order$V2), 
	axes=FALSE, xlab="", ylab="", cex=1.5)

legend(-0.0735, -0.10, legend=levels(droplevels(Weed.data$new.name)), 
	pch=Weed.sym, col=Weedy.col, title="Weedy", cex=1.2)

legend(-0.126, -0.061, legend=levels(droplevels(Crop.data$new.name)), 
	pch=Crop.sym, col=Crop.col, title="Crop", cex=1.2)  

legend(-0.044, -0.12, legend=levels(droplevels(Native.data$new.name)), 
	pch=Native.sym, col=Native.col, title="Native", cex=1.2)

legend(-0.1, -0.087, legend=levels(droplevels(Unknown.data$Pop)), 
	pch=Unknown.sym, col=Unknown.col, ncol=2, title="Unknown", cex=1.2)


	
#legend(-0.03, -0.2, legend=levels(droplevels(Weed.data$new.name)), 
#	pch=Weed.sym, col=Weedy.col, title="Weedy", cex=1.2)

#legend(-0.09, -0.12, legend=levels(droplevels(Crop.data$new.name)), 
#	pch=Crop.sym, col=Crop.col, title="Crop", cex=1.2)  

#legend(0.035, -0.225, legend=levels(droplevels(Native.data$new.name)), 
#	pch=Native.sym, col=Native.col, title="Native", cex=1.2)

#legend(0.035, -0.225, legend=levels(droplevels(Unknown.data$new.name)), 
#	pch=Unknown.sym, col=Unknown.col, title="Unknown", cex=1.2)


