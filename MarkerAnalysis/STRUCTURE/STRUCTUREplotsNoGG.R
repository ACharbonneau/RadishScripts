source('/Volumes/Storage/RadishData/RadishScripts/Misc_scripts/AmandaSource.R', chdir = TRUE)
require(RColorBrewer)
#dataset <- "NoRACoNo-5_f.txt"

########################################################

#setwd("/Volumes/Storage/RadishData/2005MarkerData/STRUCTURE/RedoneStructure/EstimateK/Corrfreq/NoRACoNoJackknife/Jackknife/Parsed/Na10H06Parsed/")

setwd("/Volumes/Storage/RadishData/2005MarkerData/STRUCTURE/RedoneStructure/CLUMPP/NoRACoNo/")

#setwd("/Volumes/Storage/RadishData/2005MarkerData/STRUCTURE/RedoneStructure/NoAdmixture/Parsed")


pdf(file="/Volumes/Storage/RadishData/2005MarkerData/STRUCTURE/RedoneStructure/EstimateK/Plots/CLUMMP6grant.pdf", height=9.3, width=15.3)

########################################################

#### You have to add a +1 to File_number and change the second for loop to 2:length(ALLTHEFILES) for 
#### datasets without a K=1


ALLTHEFILES <- dir()

File_Num <- length(ALLTHEFILES)
File_list <- matrix(1:File_Num, nrow=File_Num, ncol=2)
for(n in 1:File_Num){
	x <- ALLTHEFILES[n]
	m <- regexec("-([0-9]+)", x)
	K_new <- regmatches(x, m)
	K <- K_new[[1]][2]
	#print(K)
	File_list[as.numeric(K),1] <- x
}


for(i in c(1:length(ALLTHEFILES))){
	
dataset <- File_list[i,1]
str.data <- 0
str.data <- read.csv(dataset, header=F)
str.data <- str.data[,c(3,5:ncol(str.data-3))]
K = length(str.data)-1
colnames(str.data) <- c("%missing",1:(ncol(str.data)-1))

str.labels <- read.csv("/Volumes/Storage/RadishData/2005MarkerData/MarkerPopEditOrder.csv", header=F, col.names=c("Individual", "Type", "Pop", "Order", "Name", "Species", "Color", "Vernalization", "DTF", "Bins"))
str.labels <- str.labels[str.labels$Type!="UnknownType",]

all.data <- cbind(str.labels[,2:10],str.data)
row.names(all.data) <- str.labels$Individual

crop.data <- all.data[all.data$Type=="Crop",]
weed.data <- all.data[all.data$Type=="Weedy",]
native.data <- all.data[all.data$Type=="Native",]
daikon.data <- all.data[all.data$Species=="Daikon",]
european.data <- all.data[all.data$Species=="European",]
oilrat.data <- all.data[all.data$Species=="Rattail" | all.data$Species=="Oilseed",]





daikon.table <- t(daikon.data[11:length(daikon.data[1,])][order(daikon.data$Order),])
weed.table <- t(weed.data[11:length(weed.data[1,])][order(weed.data$Order),])
native.table <- t(native.data[11:length(native.data[1,])][order(native.data$Order),])
european.table <- t(european.data[11:length(european.data[1,])][order(european.data$Order),])
oilrat.table <- t(oilrat.data[11:length(oilrat.data[1,])][order(oilrat.data$Order),])

colnames(native.table) <- native.data$Pop[order(native.data$Order)]
colnames(weed.table) <- weed.data$Pop[order(weed.data$Order)]
colnames(daikon.table) <- daikon.data$Pop[order(daikon.data$Order)]
colnames(european.table) <- european.data$Pop[order(european.data$Order)]
colnames(oilrat.table) <- oilrat.data$Pop[order(oilrat.data$Order)]

############### Plotting ###############
#par(mfrow=c(5,1), mar=c(3.5,3,3,2), oma=c(1,1,1,1), las=2)
bob<- layout(matrix(c(1,1,2,2,3,4,5,5), 4,2, byrow=T), widths=c(1, 1, 4, 1, 1))


col_pal1 = brewer.pal(12, "Set3")
col_pal2 = brewer.pal(8, "Dark2")
col_pal3 = brewer.pal(12, "Paired")
col_pal = c(col_pal1, col_pal2, col_pal3)

K_text <- "K="

par(mfrow=c(1,1), mar=c(0,0,0,0))

par(fig=c(0,1,.8,.9), new=TRUE)
barplot(native.table, col=col_pal[1:K], cex.names=1.2, xaxt="n", yaxt="n", space=c(rep(0,10),1, rep(0,9), 1, rep(0,9), 1,rep(0,9), 1, rep(0,9), 1,rep(0,9)))
axis(side=3, at=33, labels="STRUCTURE Plot, K=6", cex=1.2, tick=F, line=.8)
axis(side=3, at=33, labels="Natives", cex=1, tick=F, line=-1)
axis(side=1, at=c(5,16,27,38,49,60), labels=c(rep(expression(italic("R.r.maritimus")), 2), expression(italic("R.r.landra")), expression(italic("R.r.raphanistrum")), expression(italic("R.rostratus")), expression(italic("R.confusus"))), tick=F, line=-1)
#legend(x=70.5, y=.7, legend=paste(K_text,K), bty="n", cex=1.7 )

par(fig=c(0,1,.6,.7), new=TRUE)
barplot(weed.table, col=col_pal[1:K], cex.names=1.2, xaxt="n", yaxt="n", space=c(rep(0,10), 1, rep(0,9), 1, rep(0,9), 1, rep(0,9), 1, rep(0,9), 1,rep(0,9), 1,rep(0,9), 1, rep(0,9)))
axis(side=3, at=44, labels="Weeds", cex=1.2, tick=F, line=-1)
axis(side=1, at=c(5,16,27,38,49,60,71,82), tick=F, labels=c(rep("Israel", 2), "France", "Germany", "Finland", "New York", rep("Australia", 2)), line=-1)

par(fig=c(0,.4,.4,.5), new=TRUE)
barplot(daikon.table, col=col_pal[1:K], cex.names=1.2, xaxt="n", yaxt="n", space=c(rep(0,10), 1, rep(0,9), 1, rep(0,9)) )
axis(side=3, at=16, labels="Daikons", cex=1.2, tick=F, line=-1)
axis(side=1, at=c(5,16,27), tick=F, labels=c("Miyashige", "New Crown", "Tokinashi"), line=-1)


par(fig=c(.4,1,.4,.5), new=TRUE)
barplot(european.table, col=col_pal[1:K], cex.names=1.2, xaxt="n", yaxt="n", space=c(rep(0,10),1, rep(0,9), 1, rep(0,8), 1,rep(0,9), 1, rep(0,9)) )
axis(side=3, at=26, labels="European Crops", cex=1.2, tick=F, line=-1)
axis(side=1, at=c(5,16,27,37,48), tick=F, labels=c("Cherry Belle", "D'avignon", "Early S.G.", "Watermelon", "Sparkler"), line=-1)

par(fig=c(0,1,.2,.3), new=TRUE)
barplot(oilrat.table, col=col_pal[1:K], cex.names=1.2, xaxt="n", yaxt="n", space=c(rep(0,10),1, rep(0,9), 1, rep(0,9), 3,rep(0,9), 1, rep(0,9), 1, rep(0,8)) )
axis(side=3, at=c(16,51), labels=c("Oilseed Crops", "Rattail Crops"), cex=1.2, tick=F, line=-1)
axis(side=1, at=c(5,16,27,40,51,62), tick=F, labels=c("Arena", "Colonel", "Adagio", "Madras podding", "Rattail", "Rattail"), line=-1)


}    


, space=c(rep(0, 10), rep(1, 1), rep(0, 9)), xaxt='n')
	axis( side=1, at=c(2,4,6,8,10,12,14,16), labels=c(rep("name", 8)))



dev.off()
