rm( list=ls())
#setwd("/Volumes/Storage/RadishData/2005MarkerData/SmartPCA/SNP_SSR_NoNZIL_allPC/")
setwd("/Volumes/Storage/RadishData/2005MarkerData/SmartPCA/2014SmartPCA/")
#setwd("/Volumes/Storage/RadishData/2005MarkerData/SmartPCA/2014SmartPCAnoNZIL/")


#PCA.dat <- read.table("Marker.pca", skip=109)
PCA.dat <- read.table("Marker.pca", skip=11)

#labels.dat <- read.csv("/Volumes/Storage/RadishData/2005MarkerData/MarkerPopEditOrder.csv", header=F, col.names=c("Individual", "Type", "Pop", "Order", "Name", "Species", "Color", "Vernalization", "DTF", "Bins"))

labels.dat <- read.csv("/Volumes/Storage/RadishData/2005MarkerData/SmartPCA/2014SmartPCA/MarkerPopEditOrder2014.csv", header=F, col.names=c("Individual", "Type", "Pop", "Order", "Name", "Species", "Color", "Vernalization", "DTF", "Bins", "locals"))


#Use only for no NZIL runs:
#labels.dat <- labels.dat[grep("NZIL", labels.dat$Pop, invert=TRUE),]



#Use for all runs:
labels.dat <- labels.dat[grep("UnknownType", labels.dat$Type, invert=TRUE),]


pca.lab <- data.frame(PCA.dat, labels.dat[grep("RA\\d\\d\\d", labels.dat$Pop, invert=TRUE),]) 

#pca.lab <- pca.lab[(row.names(pca.lab)!="345"),] #This plant has all "0" values from SmartPCA

pca.lab$new.name <- factor( paste(pca.lab$Species, pca.lab$Pop, sep="," ) )
pca.lab <- droplevels(pca.lab)
Crop.col <- "gray0"
lanmar.col <- "dodgerblue4"
raphNN.col <- "firebrick"
raphNat.col <- "tan"
rost.col <- "palegreen3"
conf.col <- "orchid"


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

raphNN.data <- species.order[species.order$locals=="raphNN",]
lanmar.data <- species.order[species.order$locals=="lanmar",]
Crop.data <- species.order[species.order$Type=="Crop",]
rost.data <-  species.order[species.order$locals=="rostratus",]
raphNat.data <- species.order[species.order$locals=="raphNat",]
conf.data <- species.order[species.order$locals=="confusus",]

raphNN.sym <- c(1:length(levels(droplevels(raphNN.data$Pop))))
lanmar.sym <- c(1:length(levels(droplevels(lanmar.data$Pop))))
Crop.sym <- c(1:length(levels(droplevels(Crop.data$Pop))))
rost.sym <- c(1:length(levels(droplevels(rost.data$Pop))))
raphNat.sym <- c(1:length(levels(droplevels(raphNat.data$Pop))))
conf.sym <- c(1:length(levels(droplevels(conf.data$Pop))))

pdf(file="squareNoRA_2014paper_pca.pdf", width=8.5, height=8.5)
#pdf(file="squareNoRA_pca.pdf", width=8.5, height=8.5)

plot((species.order$V1 * -1), 
	(species.order$V2 * -1),
	type="n", 
	xlab="Eigenvector 1", ylab="Eigenvector 2", cex.lab=1.1, 
	#xlim=c(-0.1, 0.2), ylim=c(-0.1, 0.2) #For paper
	xlim=c(-0.1, 0.11), ylim=c(-0.3, 0.13) #newpops, w/NZIL
	#xlim=c(-0.1, 0.1), ylim=c(-0.1, 0.2) #newpops, w/o NZIL
)


par(new=TRUE)
plot((raphNN.data$V1 * -1), 
	(raphNN.data$V2 * -1), 
	pch=raphNN.sym[droplevels(raphNN.data$new.name)], col=raphNN.col, 
	#xlim=c(-0.1, 0.2), ylim=c(-0.1, 0.2),
	xlim=c(-0.1, 0.11), ylim=c(-0.3, 0.13), #newpops, w/NZIL
	#xlim=c(-0.1, 0.1), ylim=c(-0.1, 0.2),
	axes=FALSE, xlab="", ylab="", cex=1.5)

par(new=TRUE)	
plot((lanmar.data$V1 * -1), 
	(lanmar.data$V2 * -1), 
	pch=lanmar.sym[droplevels(lanmar.data$new.name)], col=lanmar.col,
	#xlim=c(-0.1, 0.2), ylim=c(-0.1, 0.2),
	xlim=c(-0.1, 0.11), ylim=c(-0.3, 0.13), #newpops, w/NZIL
	#xlim=c(-0.1, 0.1), ylim=c(-0.1, 0.2),
	axes=FALSE, xlab="", ylab="", cex=1.5)

par(new=TRUE)
plot((Crop.data$V1 * -1), 
	(Crop.data$V2 * -1), 
	pch=Crop.sym[droplevels(Crop.data$new.name)], col=Crop.col, 
	#xlim=c(-0.1, 0.2), ylim=c(-0.1, 0.2),
	xlim=c(-0.1, 0.11), ylim=c(-0.3, 0.13), #newpops, w/NZIL
	#xlim=c(-0.1, 0.1), ylim=c(-0.1, 0.2),
	axes=FALSE, xlab="", ylab="", cex=1.5)
	
par(new=TRUE)
plot((raphNat.data$V1 * -1), 
	(raphNat.data$V2 * -1), 
	pch=raphNat.sym[droplevels(raphNat.data$new.name)], col=raphNat.col, 
	#xlim=c(-0.1, 0.2), ylim=c(-0.1, 0.2),
	xlim=c(-0.1, 0.11), ylim=c(-0.3, 0.13), #newpops, w/NZIL
	#xlim=c(-0.1, 0.1), ylim=c(-0.1, 0.2),
	axes=FALSE, xlab="", ylab="", cex=1.5)	
	
par(new=TRUE)
plot((rost.data$V1 * -1), 
	(rost.data$V2 * -1), 
	pch=rost.sym[droplevels(rost.data$new.name)], col=rost.col, 
	#xlim=c(-0.1, 0.2), ylim=c(-0.1, 0.2),
	xlim=c(-0.1, 0.11), ylim=c(-0.3, 0.13), #newpops, w/NZIL
	#xlim=c(-0.1, 0.1), ylim=c(-0.1, 0.2),
	axes=FALSE, xlab="", ylab="", cex=1.5)		
	
par(new=TRUE)
plot((conf.data$V1 * -1), 
	(conf.data$V2 * -1), 
	pch=conf.sym[droplevels(conf.data$new.name)], col=conf.col, 
	#xlim=c(-0.1, 0.2), ylim=c(-0.1, 0.2),
	xlim=c(-0.1, 0.11), ylim=c(-0.3, 0.13), #newpops, w/NZIL
	#xlim=c(-0.1, 0.1), ylim=c(-0.1, 0.2),
	axes=FALSE, xlab="", ylab="", cex=1.5)	

########### Plots for paper ####################

#legend(.0515, 0.21, legend=levels(droplevels(Weed.data$new.name)), 
#	pch=Weed.sym, col=Weedy.col, title="Weedy", cex=1)

#legend(.138, 0.21, legend=levels(droplevels(Crop.data$new.name)), 
#	pch=Crop.sym, col=Crop.col, title="Crop", cex=1)  

#legend(-.11, 0.21, legend=levels(droplevels(Native.data$new.name)), 
#	pch=Native.sym, col=Native.col, title="Native", cex=1)
	
############## Adding New populations with NZIL ################
legend(-0.106,-0.215, legend=levels(droplevels(raphNN.data$new.name)), 
	pch=raphNN.sym, col=raphNN.col, title="Non-native RRR", cex=1)

legend(0.066, -0.09, legend=levels(droplevels(Crop.data$new.name)), 
	pch=Crop.sym, col=Crop.col, title="Crop", cex=1)  

legend(-0.106, -0.14, legend=levels(droplevels(lanmar.data$new.name)), 
	pch=lanmar.sym, col=lanmar.col, title="landra & maritimus", cex=1)
	
legend(0.035, -0.132, legend=levels(droplevels(raphNat.data$Pop)), 
	pch=raphNat.sym, col=raphNat.col, title="native RRR", cex=1)

legend(0.035, -0.09, legend=levels(droplevels(rost.data$Pop)), 
	pch=rost.sym, col=rost.col, title="rostratus", cex=1, bty="n")
rect(0.035, -0.128, 0.064, -0.09)

############## Adding New populations without NZIL ################
#legend(-0.004, 0.21, legend=levels(droplevels(raphNN.data$new.name)), 
#	pch=raphNN.sym, col=raphNN.col, title="Non-native RRR", cex=1)

#legend(-0.107, 0.21, legend=levels(droplevels(Crop.data$new.name)), 
#	pch=Crop.sym, col=Crop.col, title="Crop", cex=1)  

#legend(0.055, 0.21, legend=levels(droplevels(lanmar.data$new.name)), 
#	pch=lanmar.sym, col=lanmar.col, title="landra & maritimus", cex=1, bty="n")

#rect(0.054, 0.162, 0.1065, 0.21)
	
#legend(0.079, 0.16, legend=levels(droplevels(raphNat.data$Pop)), 
#	pch=raphNat.sym, col=raphNat.col, title="native RRR", cex=1)

#legend(0.054, 0.16, legend=levels(droplevels(rost.data$Pop)), 
#	pch=rost.sym, col=rost.col, title="rostratus", cex=1)


dev.off()
