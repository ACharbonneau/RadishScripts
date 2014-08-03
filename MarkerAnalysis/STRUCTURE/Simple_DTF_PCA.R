rm( list=ls())
#setwd("/Volumes/Storage/RadishData/2005MarkerData/SmartPCA/SNP_SSR_NoNZIL_allPC/")
#setwd("/Volumes/Storage/RadishData/2005MarkerData/SmartPCA/2014SmartPCA/")
setwd("/Volumes/Storage/RadishData/2005MarkerData/SmartPCA/2014SmartPCAnoNZIL/")


#PCA.dat <- read.table("Marker.pca", skip=109)
PCA.dat <- read.table("Marker.pca", skip=11)

#labels.dat <- read.csv("/Volumes/Storage/RadishData/2005MarkerData/MarkerPopEditOrder.csv", header=F, col.names=c("Individual", "Type", "Pop", "Order", "Name", "Species", "Color", "Vernalization", "DTF", "Bins"))

labels.dat <- read.csv("/Volumes/Storage/RadishData/2005MarkerData/SmartPCA/2014SmartPCA/MarkerPopEditOrder2014.csv", header=F, col.names=c("Individual", "Type", "Pop", "Order", "Name", "Species", "Color", "Vernalization", "DTF", "Bins", "locals"))


#Use only for no NZIL runs:
labels.dat <- labels.dat[grep("NZIL", labels.dat$Pop, invert=TRUE),]



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


c_never <- "dodgerblue4"
b_Forty6To100 <- "tan"
a_less_45 <- "firebrick"

dtf_col <- c(a_less_45, b_Forty6To100, c_never)

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

#species.order <- species.order[species.order$locals != "rostratus",]

pdf(file="squareNoRA_2014DTF_pca_Evo_FT_rost.pdf", width=8.5, height=8.5)


plot((species.order$V1 * -1), 
	(species.order$V2 * -1),
	xlab="Eigenvector 1", ylab="Eigenvector 2", cex.lab=1.1, 
	cex=2, col=dtf_col[species.order$Bins], pch=16,
	#xlim=c(-0.1, 0.2), ylim=c(-0.1, 0.2) #For paper
	#xlim=c(-0.1, 0.11), ylim=c(-0.3, 0.13) #newpops, w/NZIL
	xlim=c(-0.1, 0.1), ylim=c(-0.1, 0.2) #newpops, w/o NZIL
)
legend(0.0, 0.21, legend=c("Flowers in < 45 days", "Flowers in 46-100 days", "Flowers in > 100 days"), pch=16, cex=1.7, col=c(a_less_45, b_Forty6To100, c_never))

dev.off()