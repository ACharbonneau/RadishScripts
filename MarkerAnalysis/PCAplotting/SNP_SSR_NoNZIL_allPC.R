rm( list=ls())
#setwd("/Volumes/Storage/RadishData/2005MarkerData/SmartPCA/SNP_SSR_NoNZIL_allPC/")
#setwd("/Volumes/Storage/RadishData/2005MarkerData/SmartPCA/2014SmartPCA/")
setwd("/Volumes/Storage/RadishData/2005MarkerData/SmartPCA/2014SmartPCAnoNZIL/")


#PCA.dat <- read.table("Marker.pca", skip=109)
PCA.dat <- read.table("Marker.pca", skip=11)

#labels.dat <- read.csv("/Volumes/Storage/RadishData/2005MarkerData/MarkerPopEditOrder.csv", header=F, col.names=c("Individual", "Type", "Pop", "Order", "Name", "Species", "Color", "Vernalization", "DTF", "Bins"))

labels.dat <- read.csv("/Volumes/Storage/RadishData/2005MarkerData/SmartPCA/2014SmartPCA/MarkerPopEditOrder2014.csv", header=F, col.names=c("Individual", "Type", "Pop", "Order", "Name", "Species", "Color", "Vernalization", "DTF", "Bins"))


#Use only for no NZIL runs:
labels.dat <- labels.dat[grep("NZIL", labels.dat$Pop, invert=TRUE),]



#Use for all runs:
labels.dat <- labels.dat[grep("UnknownType", labels.dat$Type, invert=TRUE),]


pca.lab <- data.frame(PCA.dat, labels.dat[grep("RA\\d\\d\\d", labels.dat$Pop, invert=TRUE),]) 

pca.lab <- pca.lab[(row.names(pca.lab)!="345"),] #This plant has all "0" values from SmartPCA

pca.lab$new.name <- factor( paste(pca.lab$Species, pca.lab$Pop, sep="," ) )
pca.lab <- droplevels(pca.lab)
Crop.col <- "gray0"
Native.col <- "dodgerblue4"
Weedy.col <- "firebrick"
Unknown.col <- "tan"
israel.col <- "green"
newSpain.col <- "purple"


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

species.order <- pca.lab[order(pca.lab$Species),]

Weed.data <- species.order[species.order$Type=="Weedy",]
Native.data <- species.order[species.order$Type=="Native",]
Crop.data <- species.order[species.order$Type=="Crop",]
israel.data <-  species.order[species.order$Name=="Israel",]
newSpain.data <- species.order[species.order$Name=="Spain",]

Weed.sym <- c(1:length(levels(droplevels(Weed.data$Pop))))
Native.sym <- c(1:length(levels(droplevels(Native.data$Pop))))
Crop.sym <- c(1:length(levels(droplevels(Crop.data$Pop))))
israel.sym <- c(1:length(levels(droplevels(israel.data$Pop))))
newSpain.sym <- c(1:length(levels(droplevels(newSpain.data$Pop))))

pdf(file="squareNoRA_NoNZIL_pca.pdf", width=8.5, height=8.5)
#pdf(file="squareNoRA_pca.pdf", width=8.5, height=8.5)

plot((species.order$V1 * -1), 
	(species.order$V2 * -1),
	type="n", 
	xlab="Eigenvector 1", ylab="Eigenvector 2", cex.lab=1.1, 
	#xlim=c(-0.1, 0.2), ylim=c(-0.1, 0.2) #For paper
	#xlim=c(-0.13, 0.1), ylim=c(-0.3, 0.13) #newpops, w/NZIL
	xlim=c(-0.1, 0.1), ylim=c(-0.1, 0.2) #newpops, w/o NZIL
)


par(new=TRUE)
plot((Weed.data$V1 * -1), 
	(Weed.data$V2 * -1), 
	pch=Weed.sym[droplevels(Weed.data$new.name)], col=Weedy.col, 
	#xlim=c(-0.1, 0.2), ylim=c(-0.1, 0.2),
	#xlim=c(-0.13, 0.1), ylim=c(-0.3, 0.13), 
	xlim=c(-0.1, 0.1), ylim=c(-0.1, 0.2),
	axes=FALSE, xlab="", ylab="", cex=1.5)

par(new=TRUE)	
plot((Native.data$V1 * -1), 
	(Native.data$V2 * -1), 
	pch=Native.sym[droplevels(Native.data$new.name)], col=Native.col,
	#xlim=c(-0.1, 0.2), ylim=c(-0.1, 0.2),
	#xlim=c(-0.13, 0.1), ylim=c(-0.3, 0.13), 
	xlim=c(-0.1, 0.1), ylim=c(-0.1, 0.2),
	axes=FALSE, xlab="", ylab="", cex=1.5)

par(new=TRUE)
plot((Crop.data$V1 * -1), 
	(Crop.data$V2 * -1), 
	pch=Crop.sym[droplevels(Crop.data$new.name)], col=Crop.col, 
	#xlim=c(-0.1, 0.2), ylim=c(-0.1, 0.2),
	#xlim=c(-0.13, 0.1), ylim=c(-0.3, 0.13), 
	xlim=c(-0.1, 0.1), ylim=c(-0.1, 0.2),
	axes=FALSE, xlab="", ylab="", cex=1.5)
	
par(new=TRUE)
plot((israel.data$V1 * -1), 
	(israel.data$V2 * -1), 
	pch=israel.sym[droplevels(israel.data$new.name)], col=israel.col, 
	#xlim=c(-0.1, 0.2), ylim=c(-0.1, 0.2),
	#xlim=c(-0.13, 0.1), ylim=c(-0.3, 0.13), 
	xlim=c(-0.1, 0.1), ylim=c(-0.1, 0.2),
	axes=FALSE, xlab="", ylab="", cex=1.5)	
	
par(new=TRUE)
plot((newSpain.data$V1 * -1), 
	(newSpain.data$V2 * -1), 
	pch=newSpain.sym[droplevels(newSpain.data$new.name)], col=newSpain.col, 
	#xlim=c(-0.1, 0.2), ylim=c(-0.1, 0.2),
	#xlim=c(-0.13, 0.1), ylim=c(-0.3, 0.13), 
	xlim=c(-0.1, 0.1), ylim=c(-0.1, 0.2),
	axes=FALSE, xlab="", ylab="", cex=1.5)		

########### Plots for paper ####################

#legend(.0515, 0.21, legend=levels(droplevels(Weed.data$new.name)), 
#	pch=Weed.sym, col=Weedy.col, title="Weedy", cex=1)

#legend(.138, 0.21, legend=levels(droplevels(Crop.data$new.name)), 
#	pch=Crop.sym, col=Crop.col, title="Crop", cex=1)  

#legend(-.11, 0.21, legend=levels(droplevels(Native.data$new.name)), 
#	pch=Native.sym, col=Native.col, title="Native", cex=1)
	
############## Adding New populations with NZIL ################
#legend(-0.081,-0.174, legend=levels(droplevels(Weed.data$new.name)), 
#	pch=Weed.sym, col=Weedy.col, title="Weedy", cex=1)

#legend(-0.138, -0.09, legend=levels(droplevels(Crop.data$new.name)), 
#	pch=Crop.sym, col=Crop.col, title="Crop", cex=1)  

#legend(-0.138, 0.145, legend=levels(droplevels(Native.data$new.name)), 
#	pch=Native.sym, col=Native.col, title="Native", cex=1)
	
#legend(-.04, 0.145, legend=levels(droplevels(israel.data$Pop)), 
#	pch=israel.sym, col=israel.col, title="Israel", cex=1)

#legend(-.072, 0.145, legend=levels(droplevels(newSpain.data$Pop)), 
#	pch=newSpain.sym, col=newSpain.col, title="New Spain", cex=1)

############## Adding New populations without NZIL ################
legend(0.001, 0.21, legend=levels(droplevels(Weed.data$new.name)), 
	pch=Weed.sym, col=Weedy.col, title="Weedy", cex=1)

legend(0.059, 0.21, legend=levels(droplevels(Crop.data$new.name)), 
	pch=Crop.sym, col=Crop.col, title="Crop", cex=1)  

legend(-0.107, 0.21, legend=levels(droplevels(Native.data$new.name)), 
	pch=Native.sym, col=Native.col, title="Native", cex=1)
	
legend(-.079, 0.14, legend=levels(droplevels(israel.data$Pop)), 
	pch=israel.sym, col=israel.col, title="Israel", cex=1)

legend(-.107, 0.14, legend=levels(droplevels(newSpain.data$Pop)), 
	pch=newSpain.sym, col=newSpain.col, title="New Spain", cex=1)


dev.off()
