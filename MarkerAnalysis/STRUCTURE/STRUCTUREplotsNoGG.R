source('/Volumes/Storage/RadishData/RadishScripts/Misc_scripts/AmandaSource.R', chdir = TRUE)
require(RColorBrewer)
#dataset <- "NoRACoNo-5_f.txt"

########################################################

#setwd("/Volumes/Storage/RadishData/2005MarkerData/STRUCTURE/RedoneStructure/EstimateK/Corrfreq/NoRACoNoJackknife/Jackknife/Parsed/Na10H06Parsed/")

setwd("/Volumes/Storage/RadishData/2005MarkerData/STRUCTURE/RedoneStructure/EstimateK/NoRANoHelpGroups/IndivFiles/UnCorr/Run1")

pdf(file="/Volumes/Storage/RadishData/2005MarkerData/STRUCTURE/RedoneStructure/EstimateK/Plots/UnCorr1.pdf", height=9.3, width=15.3)

########################################################

#### You have to add a +1 to File_number and chage the second for loop to 2:length(ALLTHEFILES) for 
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


for(i in c(1:length(ALLTHEFILES)){
	
dataset <- File_list[i,1]
str.data <- 0
str.data <- read.csv(dataset, header=F)
str.data <- str.data[,c(3,5:ncol(str.data-3))]
K = length(str.data)-1
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


 
daikon.table <- t(daikon.data[9:length(daikon.data[1,])])
weed.table <- t(weed.data[9:length(weed.data[1,])])
native.table <- t(native.data[9:length(native.data[1,])])
european.table <- t(european.data[9:length(european.data[1,])])
oilrat.table <- t(oilrat.data[9:length(oilrat.data[1,])])

colnames(native.table) <- native.data$Pop
colnames(weed.table) <- weed.data$Pop
colnames(daikon.table) <- daikon.data$Pop
colnames(european.table) <- european.data$Pop
colnames(oilrat.table) <- oilrat.data$Pop

############### Plotting ###############
par(mfrow=c(5,1), mar=c(3.5,3,3,2), oma=c(1,1,1,1), las=2)

col_pal1 = brewer.pal(12, "Set3")
col_pal2 = brewer.pal(8, "Dark2")
col_pal3 = brewer.pal(12, "Paired")
col_pal = c(col_pal1, col_pal2, col_pal3)

K_text <- "K="

barplot(native.table, main = "Native", cex.main=2, col=col_pal[1:K], cex.names=1.2)
legend(x=70.5, y=.7, legend=paste(K_text,K), bty="n", cex=1.7 )

barplot(weed.table, main = "Weed", cex.main=2, col=col_pal[1:K], cex.names=1.2)

barplot(daikon.table, main = "Daikon", cex.main=2, col=col_pal[1:K], cex.names=1.2)

barplot(european.table, main = "European", cex.main=2, col=col_pal[1:K], cex.names=1.2)

barplot(oilrat.table, main = "Oilseed and Rattail", cex.main=2, col=col_pal[1:K], cex.names=1.2)

}

dev.off()
