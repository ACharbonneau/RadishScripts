source("~/Dropbox/Dworkin_lab/WillPitchers/Will_FUNCTIONS.R")

setwd("~/Documents/RadishData/MarkerData/SSRsnp/")

smartpca.pca <- read.table("Marker.pca", skip=11)

str(smartpca.pca)

labels.dat <- read.table("MarkerPopEdit.txt", col.names= c("Individual", "Type", "Pop", "Species", "Color", "Vernalization", "DTF"))

pca.lab <- data.frame(smartpca.pca, labels.dat)

str(pca.lab)

pca.lab.O <- pca.lab[order(pca.lab$Pop),]

mycols <- c( rep( c(1,2,3,4,5,6,"goldenrod","darkorchid2"), 6))
mysym <- c(1:24, 24:1)
cbind( mysym[pca.lab.O$Pop], pca.lab.O$Pop )

par(mfrow=c(1,1))
par(mar=c(5,5,4,2))
plot( pca.lab.O$V1, pca.lab.O$V2, 
  col=mycols[pca.lab.O$Pop], pch=mysym[pca.lab.O$Pop], 
  xlim=c(-0.13, 0.12), ylim=c(-0.16, 0.12), cex= 1.5, 
  cex.lab=2, xlab="PC1", ylab="PC2" )
legend( 0.08, 0.13, levels(pca.lab.O$Pop), col=mycols, 
  pch=mysym, ncol=2, cex=2 )
  
#Make a plot without the RA data

known.subset <- pca.lab[c(1:148,344:473),]
known.subset <- droplevels(known.subset)
known.subset <- known.subset[order(known.subset$Pop),]
subcol <- c("gray0","dodgerblue4","firebrick")
Crop <- "gray0"
Native <- "dodgerblue4"
Weedy <- "firebrick"
Xtrasym <- c("#","=","@","%")
subsym <- c(24:1)
par(mfrow=c(1,1))
par(mar=c(5,5,4,2))
plot(x=c(-0.13, 0.10), y=c(-0.16, 0.10), type="n", xlab="PC1", ylab="PC2", main="PCA by Population")
par(new=TRUE)	

plot(xlim=c(-0.13, 0.10), ylim=c(-0.16, 0.10), known.subset$V1[1:238], known.subset$V2[1:238], col=subcol[droplevels(known.subset$Type[1:238])], pch=subsym[droplevels(known.subset$Pop[1:238])], cex= 1.5, xlab="", ylab="")

par(new=TRUE)

plot(xlim=c(-0.13, 0.10), ylim=c(-0.16, 0.10), known.subset$V1[239:278], known.subset$V2[239:278], col=subcol[droplevels(known.subset$Type[239:278])], pch=Xtrasym[droplevels(known.subset$Pop[239:278])], cex= 1, xlab="", ylab="")

one.to.28 <- levels(known.subset$Pop)
	legend(x=0.06, y=0.097, 
	legend= one.to.28[1:24], 
	col=c(Crop, Weedy, Crop, Weedy, Weedy, Crop, Native, Weedy, Crop,
	Crop, Crop, Native, Crop, Native, Crop, Weedy, Crop, Native, Native,
	Crop, Crop, Weedy, Native, Crop),
	pch=c(subsym), cex=1.25, bty="n" )
	
	legend(x=0.0605, y=-0.11, legend= one.to.28[25:28], col=c(Crop, Weedy, Weedy, Crop), pch=c(Xtrasym), cex=1.15, bty="n")

	legend(x=0.08, y=0.0, legend= c("Crop", "Weed", "Native"), pch=16, col=c(Crop,Weedy, Native), cex=1.25, bty="n")
	
#polygon(x=c(.128,.128,.11,.11), y=c(.088,.078,.078,.088),  border="red")
#polygon(x=c(.128,.128,.11,.11), y=c(.076,.066,.066,.076),  border="red")
#polygon(x=c(.128,.128,.11,.11), y=c(.018,.008,.008,.018),  border="red")
#polygon(x=c(.128,.128,.11,.11), y=c(-.040,-.030,-.030,-.040),  border="red")
#polygon(x=c(.128,.128,.11,.11), y=c(-.099,-.089,-.089,-.099),  border="red")
#polygon(x=c(.086,.086,.104,.104), y=c(.053,.043,.043,.053),  border="red")
#polygon(x=c(.086,.086,.104,.104), y=c(-.016,-.006,-.006,-.016),  border="red")  
#polygon(x=c(.086,.086,.104,.104), y=c(-.040,-.030,-.030,-.040),  border="red")  
#polygon(x=c(.086,.086,.104,.104), y=c(-.0865,-.0765,-.0765,-.0865),  border="red")
#polygon(x=c(.086,.086,.104,.104), y=c(-.0985,-.0885,-.0885,-.0985),  border="red")
#polygon(x=c(.086,.086,.104,.104), y=c(-.145,-.135,-.135,-.145),  border="red") 



