#Plots eigenvalues from Marker.pca output file from SmartPCA by Eigensoft. Overlays data with colors denoting the subspecies, type, DTF or other life history traits. These data are a subset of the 2005 marker data that omits all RA### genotyping. 

source("~/Dropbox/Dworkin_lab/WillPitchers/WRP_FUNCTIONS.R")

source('/Volumes/Storage/RadishData/RadishScripts/Misc_scripts/AmandaSource.R', chdir = TRUE)

setwd("/Volumes/Storage/RadishData/2005MarkerData/")


PCA.dat <- read.table("SmartPCA/SmartPCAnoRA/Marker.pca", skip=11)

PCA.all <- read.table("SmartPCA/SmartPCA_SSRsnp/Marker.pca", skip=11)

PCA.crop <- read.table("SmartPCA/SmartPCA_justCrops/justCrops.pca", skip=11)

str(PCA.dat)
str(PCA.all)
str(PCA.crop)

labels.dat <- read.csv("/Volumes/Storage/RadishData/2005MarkerData/MarkerPopEditOrder.csv", header=F, col.names=c("Individual", "Type", "Pop", "Order", "Name", "Species", "Color", "Vernalization", "DTF", "Bins"))

pca.lab <- data.frame(PCA.dat, labels.dat[grep("RA\\d\\d\\d", labels.dat$Pop, invert=TRUE),]) 

pca.lab <- pca.lab[(row.names(pca.lab)!="345"),] #This plant has all "0" values from SmartPCA
pca.lab$new.name <- factor( paste(pca.lab$Species, pca.lab$Pop, sep="," ) )
pca.lab <- droplevels(pca.lab)

pca.all <- data.frame(PCA.all, labels.dat)
pca.all <- pca.all[pca.all$V1 !=0,]

pca.all <- droplevels(pca.all)

pca.crop <- data.frame(PCA.crop, labels.dat[labels.dat$Type=="Crop",])
pca.crop <- droplevels(pca.crop)


Crop.col <- "gray0"
Native.col <- "dodgerblue4"
Weedy.col <- "firebrick"
Unknown.col <- "tan"

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

par(new=FALSE, mfrow=c(1,1), mar=c(5,6,6,4))

species.order <- pca.lab[order(pca.lab$Species),]

Weed.data <- species.order[species.order$Type=="Weedy",]
Native.data <- species.order[species.order$Type=="Native",]
Crop.data <- species.order[species.order$Type=="Crop",]

Weed.sym <- c(1:length(levels(droplevels(Weed.data$Pop))))
Native.sym <- c(1:length(levels(droplevels(Native.data$Pop))))
Crop.sym <- c(1:length(levels(droplevels(Crop.data$Pop))))



#plotbyType

plot( pca.lab$V1, pca.lab$V2, 
  pch=c(confusus, Daikon, European, landra, maritimus, Oilseed, raphanistrum, Rattail, rostratus, UnknownSp)[pca.lab$Species], col=c(Crop.col, Native.col, Weedy.col)[pca.lab$Type], 
  xlim=range( pca.lab$V1), ylim=range( pca.lab$V1),
  xlab= "PC1", ylab= "PC2" )   

legend(0.055, 0.185, levels(pca.lab$Type), pch=16, col=c(Crop.col, Native.col, Weedy.col), bty="n")

legend(0.11, 0.185, legend=c(
	expression(italic("confusus")), 
	expression(italic("landra")), 
	expression(italic("maritimus")),
	expression(italic("raphanistrum")), 
	expression(italic("rostratus"))),
	pch=c(confusus, landra, maritimus, raphanistrum, rostratus), bty ="n")

legend(0.11, 0.132, legend=c(	
	"Daikon", 
	"European",
	"Oilseed", 
	"Rattail"),
	pch=c(Daikon, European, Oilseed, Rattail), bty="n")
	

#PlotbyPopulation

par(mar=c(4,4,3,1))
plot(species.order$V1, species.order$V2, type="n", 
	xlab="PCA1", ylab="PCA2", cex.lab=1.1, xlim=c(-.3,.2), ylim=c(-.3,.2) )

par(new=TRUE)
plot(Weed.data$V1, Weed.data$V2, 
	pch=Weed.sym[droplevels(Weed.data$new.name)], col=Weedy.col, 
	xlim=range(species.order$V1), ylim=c(-0.32, 0.15), 
	axes=FALSE, xlab="", ylab="", cex=1.2)

par(new=TRUE)	
plot(Native.data$V1, Native.data$V2, 
	pch=Native.sym[droplevels(Native.data$new.name)], col=Native.col,
	xlim=range(species.order$V1), ylim=c(-0.32, 0.15), 
	axes=FALSE, xlab="", ylab="", cex=1.2)

par(new=TRUE)
plot(Crop.data$V1, Crop.data$V2, 
	pch=Crop.sym[droplevels(Crop.data$new.name)], col=Crop.col, 
	xlim=range(species.order$V1), ylim=c(-0.32, 0.15), 
	axes=FALSE, xlab="", ylab="", cex=1.2)


legend(-0.023, -0.175, legend=levels(droplevels(Weed.data$new.name)), 
	pch=Weed.sym, col=Weedy.col, title="Weedy", cex=1)

legend(-0.09, -0.08, legend=levels(droplevels(Crop.data$new.name)), 
	pch=Crop.sym, col=Crop.col, title="Crop", cex=1)  

legend(0.053, -0.207, legend=levels(droplevels(Native.data$new.name)), 
	pch=Native.sym, col=Native.col, title="Native", cex=1)

#Plot by DTF


plot( pca.lab$V1, pca.lab$V2, 
  pch=c(confusus, Daikon, European, landra, maritimus, Oilseed, raphanistrum, Rattail, rostratus)[pca.lab$Species], col=c("palegreen", "chartreuse4", "grey4")[pca.lab$Bins], 
  xlim=range( pca.lab$V1), ylim=range( pca.lab$V2),
  xlab= "PC1", ylab= "PC2", cex=1.4)   

legend(-0.004, -0.164, c("< 45 Days", "46 - 85 Days", "> 85 Days"),
	pch=16, col=c("palegreen", "chartreuse4", "grey4"),
	cex=1.4, title="Flowering Time", bty="n")

legend(-0.09, -0.14, legend=c(
	expression(italic("confusus")), 
	expression(italic("landra")), 
	expression(italic("maritimus")),
	expression(italic("raphanistrum")),
	#expression(paste(italic("raphanistrum"), 
	#plain(" (black)"), sep=" ")), 
	expression(italic("rostratus"))),
	pch=c(confusus, landra, maritimus, raphanistrum, rostratus), 
	cex=1.4, title="Natives", bty="n")
	

legend(-0.043, -0.152, legend=c(	
	"Daikon", 
	"European",
	"Oilseed", 
	"Rattail"),
	pch=c(Daikon, European, Oilseed, Rattail),
	cex=1.4, title="Crops", bty="n")

legend(-0.09, -0.1, legend=expression(italic("raphanistrum")), pch=16, cex=1.4, title="Weeds", bty="n")








pdf(file="squareNoRApca.pdf", width=8.5, height=8.5)
plot(species.order$V1, species.order$V2, type="n", 
	xlab="Eigenvector 1", ylab="Eigenvector 2", cex.lab=1.1,xlim=c(-0.5, 0.15), ylim=c(-0.5, 0.15) )

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

legend(-.023, -0.181, legend=levels(droplevels(Weed.data$new.name)), 
	pch=Weed.sym, col=Weedy.col, title="Weedy", cex=1)

legend(-.09, -0.09, legend=levels(droplevels(Crop.data$new.name)), 
	pch=Crop.sym, col=Crop.col, title="Crop", cex=1)  

legend(.052, -0.212, legend=levels(droplevels(Native.data$new.name)), 
	pch=Native.sym, col=Native.col, title="Native", cex=1)
dev.off()
#####################################################

#3D scatterplot of first 3 PCA

#####################################################
cc <- colors()
cc <- cc[2:657]

randcols <- sample(cc, 28)
rand2cols <- sample(cc, 47)

require(rgl)

plot3d(pca.lab$V1, pca.lab$V2, pca.lab$V3, 
	col=c(Crop.col, Native.col, Weedy.col)[pca.lab$Type], 
#	col=randcols[pca.lab$Pop], 
	size=8, 
	xlab="PCA1", ylab="PCA2", zlab="PCA3")

plot3d(pca.all$V1, pca.all$V2, pca.all$V3, 
	col=c(Crop.col, Native.col, Unknown.col, Weedy.col)[pca.all$Type], 
#	col=rand2cols[pca.all$Pop], 
	size=8, 
	xlab="PCA1", ylab="PCA2", zlab="PCA3")



#####################################################

# Just crops plot



#####################################################

crop.order <- pca.crop[order(pca.crop$Species),]

plot(crop.order$V1, crop.order$V2, 
	xlab="PCA1", ylab="PCA2", 
	pch=16, 
	col=c("aquamarine", "blueviolet", "chocolate4", "blue", "deeppink", 
	"chartreuse4", "grey69", "red3", "plum2", "orchid1", 
	"goldenrod", "darkolivegreen3", "black", "lightblue3")[pca.crop$Pop], 
	cex.lab=1.5, ylim=c(-0.32, 0.15) )
	

legend(0.05, -0.18, legend=levels(droplevels(pca.crop$Pop)), 
	pch=16, col=c("aquamarine", "blueviolet", "chocolate4", "blue", "deeppink", 
	"chartreuse4", "grey69", "red3", "plum2", "orchid1", 
	"goldenrod", "darkolivegreen3", "black", "lightblue3"), title="Weedy", cex=1.2, ncol=3)







cubedraw <- function(res3d, min = 0, max = 255, cex = 2, text. = FALSE)
  {
    ## Purpose: Draw nice cube with corners
    cube01 <- rbind(c(0,0,1), 0, c(1,0,0), c(1,1,0), 1, c(0,1,1), # < 6 outer
                    c(1,0,1), c(0,1,0)) # <- "inner": fore- & back-ground
    cub <- min + (max-min)* cube01
    ## visibile corners + lines:
    res3d$points3d(cub[c(1:6,1,7,3,7,5) ,], cex = cex, type = 'b', lty = 1)
    ## hidden corner + lines
    res3d$points3d(cub[c(2,8,4,8,6),     ], cex = cex, type = 'b', lty = 3)
    if(text.)## debug
        text(res3d$xyz.convert(cub), labels=1:nrow(cub), col='tomato', cex=2)
  }
  
  
  
  
  
## 6 a) The named colors in R, i.e. colors()
  cc <- colors()
  crgb <- t(col2rgb(cc))
  par(xpd = TRUE)
  rr <- scatterplot3d(crgb, color = cc, box = FALSE, angle = 24,
      xlim = c(-50, 300), ylim = c(-50, 300), zlim = c(-50, 300))
  cubedraw(rr)