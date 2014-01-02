
source('/Volumes/Storage/RadishData/RadishScripts/Misc_scripts/AmandaSource.R', chdir = TRUE)

setwd("/Volumes/Storage/RadishData/2005MarkerData/")

PhyDistWeed <- read.csv("/Volumes/Storage/RadishData/2005MarkerData/PhysicalDistancesBetWeeds.csv", header=T)

species.order <- pca.lab[order(pca.lab$Species),]

Weed.data <- droplevels(species.order[species.order$Type=="Weedy",])
Native.data <- species.order[species.order$Type=="Native",]
Crop.data <- species.order[species.order$Type=="Crop",]

Weed.sym <- c(1:length(levels(droplevels(Weed.data$Pop))))
Native.sym <- c(1:length(levels(droplevels(Native.data$Pop))))
Crop.sym <- c(1:length(levels(droplevels(Crop.data$Pop))))

PCA.dat <- read.table("SmartPCA/SmartPCAnoRA/Marker.pca", skip=11)
labels.dat <- read.csv("/Volumes/Storage/RadishData/2005MarkerData/MarkerPopEditOrder.csv", header=F, col.names=c("Individual", "Type", "Pop", "Order", "Name", "Species", "Color", "Vernalization", "DTF", "Bins"))

pca.lab <- data.frame(PCA.dat, labels.dat[grep("RA\\d\\d\\d", labels.dat$Pop, invert=TRUE),]) 

pca.lab <- pca.lab[(row.names(pca.lab)!="345"),] #This plant has all "0" values from SmartPCA
pca.lab$new.name <- factor( paste(pca.lab$Species, pca.lab$Pop, sep="," ) )
pca.lab <- droplevels(pca.lab)

WeedPops <- levels(droplevels(Weed.data$Pop))


distBydiv <- as.data.frame(matrix(NA, (length(Weed.data$V1)*(length(Weed.data$V1)-1)), 8))
colnames(distBydiv) <- c("Pop1", "Pop2", "Ind1", "Ind2", "PC1", "PC2", "PC1minPC2", "PhyDis")

a=1	
for(lineOne in 1:length(Weed.data[,1])){
	indiv1 <- Weed.data[lineOne,]
	#print(indiv1)
	for(lineTwo in 1:length(Weed.data[,1])){
		indiv2 <- Weed.data[lineTwo,]
	#	print(pop2)
		distBydiv[a,1] <- paste(indiv1$Pop)
		temp1 <- indiv1$Pop
		distBydiv[a,2] <- paste(indiv2$Pop)
		temp2 <- indiv2$Pop
		distBydiv[a,3] <- indiv1[11]
		distBydiv[a,4] <- indiv2[11]
		distBydiv[a,5] <- indiv1[2]
		distBydiv[a,6] <- indiv2[2]
		distBydiv[a,7] <- abs(distBydiv[a,5] - distBydiv[a,6])
		distBydiv[a,8] <- PhyDistWeed[PhyDistWeed$X == indiv1$Pop, colnames(PhyDistWeed) == indiv2$Pop]
		
	a=a+1
	}
}


plot(distBydiv$PhyDis, distBydiv$PC1minPC2, xlab="Distance in Kilometers (by air)", ylab="Absolute PC2 distance", main="Correlation of Genetic and Physical distance")









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


