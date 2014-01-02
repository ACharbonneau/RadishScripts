source('/Volumes/Storage/RadishData/RadishScripts/Misc_scripts/AmandaSource.R', chdir = TRUE)
require(RColorBrewer)
#dataset <- "NoRACoNo-5_f.txt"

########################################################


setwd("/Volumes/Storage/RadishData/2005MarkerData/STRUCTURE/RedoneStructure/EstimateK/Corrfreq/NoRANoNZIL/NoRANoNZIL_20/parsed_data")

pdf(file="/Volumes/Storage/RadishData/2005MarkerData/STRUCTURE/RedoneStructure/EstimateK/Corrfreq/NoRANoNZIL/Random20.pdf", height=9.3, width=15.3)

########################################################

#### You have to add a +1 to File_Num and change the second 'for loop' to 2:length(ALLTHEFILES) for datasets without a K=1
#### 

##Get all the files from this directory to put into a single PDF
#Sort the filelist into numerical order by K instead of order from filesystem

ALLTHEFILES <- dir()

File_Num <- length(ALLTHEFILES) #+1
File_list <- matrix(1:File_Num, nrow=File_Num, ncol=2)
for(n in 1:File_Num){
	x <- ALLTHEFILES[n]
	m <- regexec("-([0-9]+)", x)
	K_new <- regmatches(x, m)
	K <- K_new[[1]][2]
	#print(K)
	File_list[as.numeric(K),1] <- x
}

#Sort each STRUCTURE file and metadata file by the plant ID number then combine them. This gets rid
#of the random order of indivduals needed to run STRUCTURE

for(i in c(1:length(ALLTHEFILES))){  # <- 1:length for K=1, 2:length for no
	
dataset <- File_list[i,1]
str.data <- 0
str.data <- read.csv(dataset, header=F)
str.to.sort <- order(str.data$V2)
str.sorted <- str.data[str.to.sort,] 
str.sorted <- str.sorted[,c(3,5:ncol(str.data-3))]
K = length(str.sorted)-1
colnames(str.sorted) <- c("%missing",1:(ncol(str.sorted)-1))

#Get the metadata about each individual from a seperate file. Remove all the "RA" and "NZIL" individuals

labels <- read.csv("/Volumes/Storage/RadishData/2005MarkerData/MarkerPopEditOrder.csv", header=F, col.names=c("Individual", "Type", "Pop", "Order", "Name", "Species", "Color", "Vernalization", "DTF", "Bins"))

labels <- labels[labels$Type!="UnknownType",]
labels <- labels[labels$Pop!="NZIL",]

labels.to.sort <- order(labels$Individual)
labels.sorted <- labels[labels.to.sort,]

all.data <- cbind(labels.sorted[,2:10],str.sorted)
row.names(all.data) <- labels.sorted$Individual

#For prettier plotting, lump all of the different species together. Later you'll plot each
#species seperately in a divided plotting screen

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
#bob<- layout(matrix(c(1,1,2,2,3,4,5,5), 4,2, byrow=T), widths=c(1, 1, 4, 1, 1))


col_pal1 = brewer.pal(12, "Set3")
col_pal2 = brewer.pal(8, "Dark2")
col_pal3 = brewer.pal(12, "Paired")
col_pal = c(col_pal1, col_pal2, col_pal3)

K_text <- paste("STRUCTURE Plot K=", K, sep="")

par(mfrow=c(1,1), mar=c(0,0,0,0))

# WITH Confusus!

#par(fig=c(0,1,.8,.9)) #new=TRUE)
#barplot(native.table, col=col_pal[1:K], cex.names=1.2, xaxt="n", yaxt="n", space=c(rep(0,10),1, rep(0,9), 1, rep(0,9), 1,rep(0,9), 1, rep(0,9), 1,rep(0,9)))
#axis(side=3, at=33, labels=c(K_text), cex=1.2, tick=F, line=.8)
#axis(side=3, at=33, labels="Natives", cex=1, tick=F, line=-1)
#axis(side=1, at=c(5,16,27,38,49,60), labels=c(rep(expression(italic("R.r.maritimus")), 2), expression(italic("R.r.landra")), expression(italic("R.r.raphanistrum")), expression(italic("R.rostratus")), expression(italic("R.confusus"))), tick=F, line=-1)

# WITHOUT Confusus!
par(fig=c(0,1,.8,.9)) #new=TRUE)
barplot(native.table, col=col_pal[1:K], cex.names=1.2, xaxt="n", yaxt="n", space=c(rep(0,10),1, rep(0,9), 1, rep(0,9), 1,rep(0,9), 1, rep(0,9)))
axis(side=3, at=27, labels=c(K_text), cex=1.2, tick=F, line=.8)
axis(side=3, at=27, labels="Natives", cex=1, tick=F, line=-1)
axis(side=1, at=c(5,16,27,38,49), labels=c(rep(expression(italic("R.r.maritimus")), 2), expression(italic("R.r.landra")), expression(italic("R.r.raphanistrum")), expression(italic("R.rostratus"))), tick=F, line=-1)


par(fig=c(0,1,.6,.7), new=TRUE)
barplot(weed.table, col=col_pal[1:K], cex.names=1.2, xaxt="n", yaxt="n", space=c(rep(0,10), 1, rep(0,9), 1, rep(0,9), 1, rep(0,9), 1, rep(0,9), 1,rep(0,9), 1,rep(0,9), 1, rep(0,9)))
axis(side=3, at=44, labels="Weeds", cex=1.2, tick=F, line=-1)
axis(side=1, at=c(5,16,27,38,49,60,71,82), tick=F, labels=c(rep("Israel", 2), "France", "Germany", "Finland", "New York", rep("Australia", 2)), line=-1)

par(fig=c(0,.5,.4,.5), new=TRUE)
barplot(daikon.table, col=col_pal[1:K], cex.names=1.2, xaxt="n", yaxt="n", space=c(rep(0,10), 1, rep(0,9), 1, rep(0,9), 1,rep(0,9)) )
axis(side=3, at=22, labels="Daikon Crops", cex=1.2, tick=F, line=-1)
axis(side=1, at=c(5,16,27,38), tick=F, labels=c("Miyashige", "New Crown", "Tokinashi", "Watermelon"), line=-1)


par(fig=c(.5,1,.4,.5), new=TRUE)
barplot(european.table, col=col_pal[1:K], cex.names=1.2, xaxt="n", yaxt="n", space=c(rep(0,10),1, rep(0,9), 1, rep(0,8), 1,rep(0,9)) )
axis(side=3, at=22, labels="European Crops", cex=1.2, tick=F, line=-1)
axis(side=1, at=c(5,16,27,37), tick=F, labels=c("Cherry Belle", "D'avignon", "Early S.G.", "Sparkler"), line=-1)

par(fig=c(0,1,.2,.3), new=TRUE)
barplot(oilrat.table, col=col_pal[1:K], cex.names=1.2, xaxt="n", yaxt="n", space=c(rep(0,10),1, rep(0,9), 1, rep(0,9), 3,rep(0,9), 1, rep(0,9), 1, rep(0,8)) )
axis(side=3, at=c(16,51), labels=c("Oilseed Crops", "Rattail Crops"), cex=1.2, tick=F, line=-1)
axis(side=1, at=c(5,16,27,40,51,62), tick=F, labels=c("Arena", "Colonel", "Adagio", "Madras podding", "Rattail", "Rattail"), line=-1)


}    


#, space=c(rep(0, 10), rep(1, 1), rep(0, 9)), xaxt='n')
#	axis( side=1, at=c(2,4,6,8,10,12,14,16), labels=c(rep("name", 8)))



dev.off()
