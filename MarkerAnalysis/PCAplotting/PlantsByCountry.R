
source('/Volumes/Storage/RadishData/RadishScripts/Misc_scripts/AmandaSource.R', chdir = TRUE)

setwd("/Volumes/Storage/RadishData/2005MarkerData/")
require(RColorBrewer)

PCA.all <- read.table("SmartPCA/SNP_SSR_no_outliers/Marker.pca", skip=11)

str(PCA.all)

labels.dat <- read.csv("/Volumes/Storage/RadishData/2005MarkerData/MarkerPopEdit_w_RA.txt", header=F, sep="\t", col.names=c("Individual", "Type", "Pop", "Order", "Name", "Species", "Color", "Vernalization", "DTF", "Bins", "Country"))


pca.all <- data.frame(PCA.all, labels.dat)
#pca.all <- pca.all[pca.all$V1 !=0,]

pca.all <- droplevels(pca.all)
pca.all$new.name <- factor( paste(pca.all$Species, pca.all$Pop, sep="," ) )

colors_18 <- sort(c(brewer.pal(12, "Set3"), brewer.pal(10, "Paired"), brewer.pal(8, "Accent")))

c16 <- c(       "green4","deeppink1", "skyblue2", "blue1",
                "#6A3D9A", # purple
                "#FF7F00", # orange
                "black","gold1",
               "maroon","#FB9A99", # lt pink
                "#FDBF6F", # lt orange
               "yellow4",
               "steelblue4","darkturquoise",
                "green1","brown1")
pie(rep(1,19), col=c16) 

plot(pca.all$V1, pca.all$V2, pch=c(15,17,18,16)[pca.all$Type], col=c16[pca.all$Species], xlab="Eigenvector 1", ylab="Eigenvector 2", xlim=c(-.16, .1), ylim=c(-.16, .1))


legend(-0.05,-0.115, legend=levels(droplevels(pca.all$Type)), pch=c(15,17,18,16))
legend(-0.15,-0.11, legend=levels(droplevels(pca.all$Species)), pch=16, col=c16, ncol=2)



Crop.col <- "gray0"
Native.col <- "dodgerblue4"
Weedy.col <- "firebrick"
RA.col <- "tan"

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

species.order <- pca.all[order(pca.all$Species),]

Weed.data <- species.order[species.order$Type=="Weedy",]
Native.data <- species.order[species.order$Type=="Native",]
Crop.data <- species.order[species.order$Type=="Crop",]
RA.data <- species.order[species.order$Type=="UnknownType",]

Weed.sym <- c(1:length(levels(droplevels(Weed.data$Pop))))
Native.sym <- c(1:length(levels(droplevels(Native.data$Pop))))
Crop.sym <- c(1:length(levels(droplevels(Crop.data$Pop))))
RA.sym <- c(1:length(levels(droplevels(RA.data$Pop))))





#pdf(file="squareNoRApca.pdf", width=8.5, height=8.5)
plot(species.order$V1, species.order$V2, type="n", 
	xlab="Eigenvector 1", ylab="Eigenvector 2", cex.lab=1.1,xlim=c(-0.5, 0.15), ylim=c(-0.5, 0.15) )

par(new=TRUE)
plot(Weed.data$V1, Weed.data$V2, 
	pch=Weed.sym[droplevels(Weed.data$Name)], col=Weedy.col, 
	xlim=range(species.order$V1), ylim=c(-0.32, 0.15), 
	axes=FALSE, xlab="", ylab="", cex=1.5)

par(new=TRUE)	
plot(Native.data$V1, Native.data$V2, 
	pch=Native.sym[droplevels(Native.data$Name)], col=Native.col,
	xlim=range(species.order$V1), ylim=c(-0.32, 0.15), 
	axes=FALSE, xlab="", ylab="", cex=1.5)

par(new=TRUE)
plot(Crop.data$V1, Crop.data$V2, 
	pch=Crop.sym[droplevels(Crop.data$Name)], col=Crop.col, 
	xlim=range(species.order$V1), ylim=c(-0.32, 0.15), 
	axes=FALSE, xlab="", ylab="", cex=1.5)
	
par(new=TRUE)
plot(RA.data$V1, RA.data$V2, 
	pch=RA.sym[droplevels(RA.data$Name)], col=RA.col, 
	xlim=range(species.order$V1), ylim=c(-0.32, 0.15), 
	axes=FALSE, xlab="", ylab="", cex=1.5)


legend(-.023, -0.181, legend=levels(droplevels(Weed.data$new.name)), 
	pch=Weed.sym, col=Weedy.col, title="Weedy", cex=1)

legend(-.09, -0.09, legend=levels(droplevels(Crop.data$new.name)), 
	pch=Crop.sym, col=Crop.col, title="Crop", cex=1)  

legend(.052, -0.212, legend=levels(droplevels(Native.data$new.name)), 
	pch=Native.sym, col=Native.col, title="Native", cex=1)
#dev.off()