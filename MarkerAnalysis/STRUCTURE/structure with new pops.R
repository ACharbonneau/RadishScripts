rm( list=ls())

source('/Volumes/Storage/RadishData/RadishScripts/Misc_scripts/AmandaSource.R', chdir = TRUE)
require(RColorBrewer)

setwd("/Volumes/Storage/RadishData/2005MarkerData/STRUCTURE/RedoneStructure/EstimateK/Corrfreq/NewPops2013/parsed_data")

pdf(file="/Volumes/Storage/RadishData/2005MarkerData/STRUCTURE/RedoneStructure/EstimateK/Corrfreq/NewPops2013/RanalphaNewPopsEvo.pdf", height=9.3, width=15.3)

str.data  <- read.csv("/Volumes/Storage/RadishData/2005MarkerData/STRUCTURE/RedoneStructure/EstimateK/Corrfreq/NewPops2013/parsed_data/7_2014_all_data-8_f.parsed", header=F)

		str.to.sort <- order(str.data$V2)
		str.sorted <- str.data[str.to.sort,] 
		str.sorted <- str.sorted[,c(3,5:ncol(str.data-3))]
		K = length(str.sorted)-1
		colnames(str.sorted) <- c("%missing",1:(ncol(str.sorted)-1))


labels <- read.csv("/Volumes/Storage/RadishData/2005MarkerData/SmartPCA/2014SmartPCA/MarkerPopEditOrder2014.csv", header=F, col.names=c("Individual", "Type", "Pop", "Order", "Name", "Species", "Color", "Vernalization", "DTF", "Bins", "locals"))

labels <- labels[labels$Type!="UnknownType",]
		labels <- labels[labels$Pop!="NZIL",]

		labels.to.sort <- order(labels$Individual)
		labels.sorted <- labels[labels.to.sort,]

	all.data <- cbind(labels.sorted[,2:11],str.sorted)
	row.names(all.data) <- labels.sorted$Individual


crop.data <- all.data[all.data$Type=="Crop",]
weed.data <- all.data[all.data$locals=="raphNN",]
native.data <- all.data[all.data$locals=="lanmar" | all.data$locals=="rostratus",]
raphNat.data <- all.data[all.data$locals=="raphNat",]
daikon.data <- all.data[all.data$Species=="Daikon",]
european.data <- all.data[all.data$Species=="European",]
oilrat.data <- all.data[all.data$Species=="Rattail" | all.data$Species=="Oilseed",]

daikon.table <- t(daikon.data[12:length(daikon.data[1,])][order(daikon.data$Order),])
weed.table <- t(weed.data[12:length(weed.data[1,])][order(weed.data$Order),])
native.table <- t(native.data[12:length(native.data[1,])][order(native.data$Order),])
raphNat.table <- t(raphNat.data[12:length(raphNat.data[1,])][order(raphNat.data$Order),])
european.table <- t(european.data[12:length(european.data[1,])][order(european.data$Order),])
oilrat.table <- t(oilrat.data[12:length(oilrat.data[1,])][order(oilrat.data$Order),])


colnames(native.table) <- native.data$Pop[order(native.data$Order)]
colnames(weed.table) <- weed.data$Pop[order(weed.data$Order)]
colnames(raphNat.table) <- raphNat.data$Pop[order(raphNat.data$Order)]
colnames(daikon.table) <- daikon.data$Pop[order(daikon.data$Order)]
colnames(european.table) <- european.data$Pop[order(european.data$Order)]
colnames(oilrat.table) <- oilrat.data$Pop[order(oilrat.data$Order)]


col_pal1 = brewer.pal(12, "Set3")
col_pal2 = brewer.pal(8, "Dark2")
col_pal3 = brewer.pal(12, "Paired")
col_pal = c(col_pal1, col_pal2, col_pal3)

K_text <- paste("STRUCTURE Plot K=", K, sep="")

par(mfrow=c(1,1), mar=c(0,0,0,0))
par(fig=c(0,1,.8,.9)) #new=TRUE)
barplot(native.table, col=col_pal[1:K], cex.names=1.2, xaxt="n", yaxt="n", space=c(rep(0,10),1, rep(0,9), 1, rep(0,9), 1,rep(0,9)))
axis(side=3, at=22, labels=c(K_text), cex=5, tick=F, line=.8)
#axis(side=3, at=21.5, labels="Natives", cex=2, tick=F, line=-1)
#axis(side=1, at=c(5,16,27,38), labels=c(rep(expression(italic("R.r.maritimus")), 2), expression(italic("R.r.landra")), expression(italic("R.rostratus"))), tick=F, line=-1)

par(fig=c(0,1,.63,.73), new=TRUE)
barplot(weed.table, col=col_pal[1:K], cex.names=1.2, xaxt="n", yaxt="n", space=c(rep(0,10), 1, rep(0,9), 1, rep(0,9), 1, rep(0,9), 1, rep(0,9)))
#axis(side=3, at=27, labels="R.r. outside native range", cex=1.2, tick=F, line=-1)
#axis(side=1, at=c(5,16,27,38,49), tick=F, labels=c("Germany", "Finland", "New York", rep("Australia", 2)), line=-1)

par(fig=c(0,1,.46,.56), new=TRUE)
barplot(raphNat.table, col=col_pal[1:K], cex.names=1.2, xaxt="n", yaxt="n", space=c(rep(0,10), 1, rep(0,9), 1, rep(0,9), 1, rep(0,9), 1, rep(0,9), 1,rep(0,9), 1,rep(0,9), 1, rep(0,9), 1, rep(0,9), 1, rep(0,9), 1, rep(0,9)))
#axis(side=3, at=60, labels="R.r. inside native range", cex=1.2, tick=F, line=-1)
#axis(side=1, at=c(5,16,27,38,49,60,71,82,93,103,115), tick=F, labels=c(rep("Israel", 2), "France", "Spain", rep("Israel", 3), rep("Spain", 4)), line=-1)

par(fig=c(0,.5,.29,.39), new=TRUE)
barplot(daikon.table, col=col_pal[1:K], cex.names=1.2, xaxt="n", yaxt="n", space=c(rep(0,10), 1, rep(0,9), 1, rep(0,9), 1,rep(0,9)) )
#axis(side=3, at=22, labels="Daikon Crops", cex=1.2, tick=F, line=-1)
#axis(side=1, at=c(5,16,27,38), tick=F, labels=c("Miyashige", "New Crown", "Tokinashi", "Watermelon"), line=-1)


par(fig=c(.5,1,.29,.39), new=TRUE)
barplot(european.table, col=col_pal[1:K], cex.names=1.2, xaxt="n", yaxt="n", space=c(rep(0,10),1, rep(0,9), 1, rep(0,8), 1,rep(0,9)) )
#axis(side=3, at=22, labels="European Crops", cex=1.2, tick=F, line=-1)
#axis(side=1, at=c(5,16,27,37), tick=F, labels=c("Cherry Belle", "D'avignon", "Early S.G.", "Sparkler"), line=-1)

par(fig=c(0,1,.12,.22), new=TRUE)
barplot(oilrat.table, col=col_pal[1:K], cex.names=1.2, xaxt="n", yaxt="n", space=c(rep(0,10),1, rep(0,9), 1, rep(0,9), 3,rep(0,9), 1, rep(0,9), 1, rep(0,8)) )
#axis(side=3, at=c(16,51), labels=c("Oilseed Crops", "Rattail Crops"), cex=1.2, tick=F, line=-1)
#axis(side=1, at=c(5,16,27,40,51,62), tick=F, labels=c("Arena", "Colonel", "Adagio", "Madras podding", "Rattail", "Rattail"), line=-1)


dev.off()