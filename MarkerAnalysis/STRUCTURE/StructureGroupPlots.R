source('~/Documents/RadishData/RadishScripts/Misc_scripts/AmandaSource.R', chdir = TRUE)
setwd("~/Documents/RadishData/2005MarkerData/STRUCTURE/RedoneStructure/EstimateK/NoRANoHelpGroups/IndivFiles")
require(ggplot2)
require(vcd)
require(reshape)

str.data <- read.csv("NoRaCoNo-10_f.txt", header=F)
str.data <- str.data[,c(3,5:ncol(str.data-3))]
colnames(str.data) <- c("%missing",1:(ncol(str.data)-1))

str.labels <- read.table("~/Documents/RadishData/2005MarkerData/MarkerPopEdit.txt", col.names=c("Individual", "Type", "Pop", "Species", "Color", "Vernalization", "DTF", "Bins"))
str.labels <- str.labels[str.labels$Type!="UnknownType",]

all.data <- cbind(str.labels[,2:8],str.data)
row.names(all.data) <- str.labels$Individual

crop.data <- all.data[all.data$Type=="Crop",]
weed.data <- all.data[all.data$Type=="Weedy",]
native.data <- all.data[all.data$Type=="Native",]
daikon.data <- all.data[all.data$Species=="Daikon",]
european.data <- all.data[all.data$Species=="European",]
oilrat.data <- all.data[all.data$Species=="Rattail" | all.data$Species=="Oilseed",]


crop.data.m <- melt(cbind(crop.data, ind=rownames(crop.data)), is.vars=c('ind'), measure.vars=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
weed.data.m <- melt(cbind(weed.data, ind=rownames(weed.data)), is.vars=c('ind'), measure.vars=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
native.data.m <- melt(cbind(native.data, ind=rownames(native.data)), is.vars=c('ind'), measure.vars=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
daikon.data.m <- melt(cbind(daikon.data, ind=rownames(daikon.data)), is.vars=c('ind'), measure.vars=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
european.data.m <- melt(cbind(european.data, ind=rownames(european.data)), is.vars=c('ind'), measure.vars=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
oilrat.data.m <- melt(cbind(oilrat.data, ind=rownames(oilrat.data)), is.vars=c('ind'), measure.vars=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))

pops.l <- levels(droplevels(all.data$Pop))

library(scales)

NP <- ggplot(native.data.m, aes(x=ind, y=value, fill=variable)) + geom_bar(position= "fill") + scale_y_continuous(labels=percent_format())

WP <- ggplot(weed.data.m, aes(x=ind, y=value, fill=variable)) + geom_bar(position= "fill") + scale_y_continuous(labels=percent_format())

CP <- ggplot(crop.data.m, aes(x=ind, y=value, fill=variable)) + geom_bar(position= "fill") + scale_y_continuous(labels=percent_format())

DP <- ggplot(daikon.data.m, aes(x=ind, y=value, fill=variable)) + geom_bar(position= "fill") + scale_y_continuous(labels=percent_format())

EP <- ggplot(european.data.m, aes(x=ind, y=value, fill=variable)) + geom_bar(position= "fill") + scale_y_continuous(labels=percent_format())

RP <- ggplot(oilrat.data.m, aes(x=ind, y=value, fill=variable)) + geom_bar(position= "fill") + scale_y_continuous(labels=percent_format())



multiplot(
	NP + theme(axis.text.x = element_text(angle=90, size=10), legend.position="none") + ggtitle("Native") + ylab("% ID to K") + xlab("Individual"), 
	WP + theme(axis.text.x = element_text(angle=90, size=10), legend.position="none") + ggtitle("Weedy") + ylab("% ID to K") + xlab("Individual"), 
#	CP + theme(axis.text.x = element_text(angle=90, size=7), legend.position="none") + ggtitle("Crop Populations"),
	DP + theme(axis.text.x = element_text(angle=90, size=10)) + ggtitle("Daikon") + ylab("% ID to K") + xlab("Individual") + guides(fill=guide_legend(title="Groups")), 
	EP + theme(axis.text.x = element_text(angle=90, size=10), legend.position="none") + ggtitle("European") + ylab("% ID to K") + xlab("Individual"), 
	RP + theme(axis.text.x = element_text(angle=90, size=10), legend.position="none") + ggtitle("Rattail & Oilseed") + ylab("% ID to K") + xlab("Individual"), 
	cols=1)


HP1 <- ggplot(all.data.m, aes(x = ind, y = value, fill = variable)) + 
	geom_bar(position = "fill") + 
	scale_y_continuous(labels = percent_format())
