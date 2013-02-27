source('/Volumes/Storage/RadishData/RadishScripts/Misc_scripts/AmandaSource.R', chdir = TRUE)
setwd("/Volumes/Storage/RadishData/2005MarkerData/STRUCTURE/RedoneStructure/EstimateK/Corrfreq/NoRACoNoJackknife/Jackknife/Bn26a")
require(ggplot2)
require(vcd)
require(reshape)

#dataset <- "Bn26a.txt-5_f.parsed"

dataset <- 

str.data <- read.csv(dataset, header=F)
str.data <- str.data[,c(3,5:ncol(str.data-3))]
colnames(str.data) <- c("%missing",1:(ncol(str.data)-1))

str.labels <- read.table("/Volumes/Storage/RadishData/2005MarkerData/MarkerPopEdit.txt", col.names=c("Individual", "Type", "Pop", "Species", "Color", "Vernalization", "DTF", "Bins"))
str.labels <- str.labels[str.labels$Type!="UnknownType",]

all.data <- cbind(str.labels[,2:8],str.data)
row.names(all.data) <- str.labels$Individual

crop.data <- all.data[all.data$Type=="Crop",]
weed.data <- all.data[all.data$Type=="Weedy",]
native.data <- all.data[all.data$Type=="Native",]
daikon.data <- all.data[all.data$Species=="Daikon",]
european.data <- all.data[all.data$Species=="European",]
oilrat.data <- all.data[all.data$Species=="Rattail" | all.data$Species=="Oilseed",]


crop.data.m <- melt(cbind(crop.data, ind=rownames(crop.data)), is.vars=c('ind'), id.vars=c("ind", "Type", "Pop", "Species", "Color", "Vernalization", "DTF", "Bins", "%missing"))
weed.data.m <- melt(cbind(weed.data, ind=rownames(weed.data)), is.vars=c('ind'), id.vars=c("ind", "Type", "Pop", "Species", "Color", "Vernalization", "DTF", "Bins", "%missing"))
native.data.m <- melt(cbind(native.data, ind=rownames(native.data)), is.vars=c('ind'), id.vars=c("ind", "Type", "Pop", "Species", "Color", "Vernalization", "DTF", "Bins", "%missing"))
daikon.data.m <- melt(cbind(daikon.data, ind=rownames(daikon.data)), is.vars=c('ind'), id.vars=c("ind", "Type", "Pop", "Species", "Color", "Vernalization", "DTF", "Bins", "%missing"))
european.data.m <- melt(cbind(european.data, ind=rownames(european.data)), is.vars=c('ind'), id.vars=c("ind", "Type", "Pop", "Species", "Color", "Vernalization", "DTF", "Bins", "%missing"))
oilrat.data.m <- melt(cbind(oilrat.data, ind=rownames(oilrat.data)), is.vars=c('ind'), id.vars=c("ind", "Type", "Pop", "Species", "Color", "Vernalization", "DTF", "Bins", "%missing"))

pops.l <- levels(droplevels(all.data$Pop))

library(scales)

##  Labels
thing = 1
nacho <- matrix(NA,6,3)
for (i in c("GMIL", "CBES", "NZIL", "SAES", "MAES", "PBFR" )){
	taco <- native.data.m[native.data.m[3]==i,][1]
	nacho[thing,1] <- taco[,1][3]
    nacho[thing,2] <- i
	thing=thing + 1
}


NP_label <- c("GMIL", "CBES", "NZIL", "SAES", "MAES", "PBFR" )
NP_x <- c
NP_y <- 
colnames(NP_lab)[1] <- "x"
NP_lab$lab <- as.character(100 * dfTab$Freq / sum(dfTab$Freq))




NP <- ggplot(native.data.m, aes(x=ind, y=value, fill=variable)) + geom_bar(position= "fill", stat="identity") + scale_y_continuous(labels=percent_format())

WP <- ggplot(weed.data.m, aes(x=ind, y=value, fill=variable)) + geom_bar(position= "fill", stat="identity") + scale_y_continuous(labels=percent_format())

CP <- ggplot(crop.data.m, aes(x=ind, y=value, fill=variable)) + geom_bar(position= "fill", stat="identity") + scale_y_continuous(labels=percent_format())

DP <- ggplot(daikon.data.m, aes(x=ind, y=value, fill=variable)) + geom_bar(position= "fill", stat="identity") + scale_y_continuous(labels=percent_format())

EP <- ggplot(european.data.m, aes(x=ind, y=value, fill=variable)) + geom_bar(position= "fill", stat="identity") + scale_y_continuous(labels=percent_format())

RP <- ggplot(oilrat.data.m, aes(x=ind, y=value, fill=variable)) + geom_bar(position= "fill", stat="identity") + scale_y_continuous(labels=percent_format())

multiplot(
	NP + theme(axis.text.x = element_text(angle=90, size=10), legend.position="none") + ggtitle("Native") + ylab("% ID to K") + xlab("Individual"), 
	WP + theme(axis.text.x = element_text(angle=90, size=10), legend.position="none") + ggtitle("Weedy") + ylab("% ID to K") + xlab("Individual"), 
#	CP + theme(axis.text.x = element_text(angle=90, size=7), legend.position="none") + ggtitle("Crop Populations"),
#	DP + theme(axis.text.x = element_text(angle=90, size=10)) + ggtitle("Daikon") + ylab("% ID to K") + xlab("Individual") + guides(fill=guide_legend(title="Groups")), 
	DP + theme(axis.text.x = element_text(angle=90, size=10), legend.position="none") + ggtitle("Daikon") + ylab("%ID to K") + xlab("Individual"),
	EP + theme(axis.text.x = element_text(angle=90, size=10), legend.position="none") + ggtitle("Crop European") + ylab("% ID to K") + xlab("Individual"), 
	RP + theme(axis.text.x = element_text(angle=90, size=10), legend.position="none") + ggtitle("Rattail & Oilseed") + ylab("% ID to K") + xlab("Individual"), 
	cols=1)




geom_text(aes(x2,y2,label=texthere),
	data.frame(x2=c(0,0), y2=c(0,0), texthere=c("GMIL", "AFFR")))


#HP1 <- ggplot(all.data.m, aes(x = ind, y = value, fill = variable)) + 
	#geom_bar(position = "fill") + 
	#scale_y_continuous(labels = percent_format())
