source('/Volumes/Storage/RadishData/RadishScripts/Misc_scripts/AmandaSource.R', chdir = TRUE)
require(RColorBrewer)
#dataset <- "NoRACoNo-5_f.txt"

########################################################

#setwd("/Volumes/Storage/RadishData/2005MarkerData/STRUCTURE/RedoneStructure/EstimateK/Corrfreq/NoRACoNoJackknife/Jackknife/Parsed/Na10H06Parsed/")

#setwd("/Volumes/Storage/RadishData/2005MarkerData/STRUCTURE/RedoneStructure/CLUMPP/NoRACoNo/")

setwd("/Volumes/Storage/RadishData/2005MarkerData/STRUCTURE/RedoneStructure/EstimateK/Corrfreq/NoRACoNoPopJackknife/JustWeeds/jwClumppout/")

setwd("/Volumes/Storage/RadishData/2005MarkerData/STRUCTURE/RedoneStructure/EstimateK/Corrfreq/NoRACoNoPopJackknife/RaphinistrumNatives/rnClumppout/")

#setwd("/Volumes/Storage/RadishData/2005MarkerData/STRUCTURE/RedoneStructure/NoAdmixture/Parsed")


#pdf(file="/Volumes/Storage/RadishData/2005MarkerData/STRUCTURE/RedoneStructure/EstimateK/Plots/jwClumpp", height=9.3, width=15.3)

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

bob = c(9)
par(mfrow=c(2,1), las=2)

for (i in bob){	
dataset <- File_list[i,1]
str.data <- 0
str.data <- read.csv(dataset, header=F)
str.data <- str.data[,c(3,5:ncol(str.data-3))]
K = length(str.data)-1
colnames(str.data) <- c("%missing",1:(ncol(str.data)-1))

str.labels <- read.table("/Volumes/Storage/RadishData/2005MarkerData/MarkerPopEdit.txt", col.names=c("Individual", "Type", "Pop", "Order", "Name", "Species", "Color", "Vernalization", "DTF", "Bins"), sep="\t")
#str.labels <- str.labels[str.labels$Type!="UnknownType",]
#str.labels <- str.labels[str.labels$Type=="Weedy",]
str.labels <- str.labels[str.labels$Type!="UnknownType" & str.labels$Type!="Crop" & str.labels$Species!="rostratus" & str.labels$Species!="confusus",]


all.data <- cbind(str.labels[,2:8],str.data)
row.names(all.data) <- str.labels$Individual

weed.data <- all.data[all.data$Type=="Weedy",]
weed.table <- t(weed.data[9:length(weed.data[1,])])
colnames(weed.table) <- weed.data$Pop

native.data <- all.data[all.data$Type=="Native",]
native.table <- t(native.data[9:length(native.data[1,])])
colnames(native.table) <- native.data$Pop

col_pal1 = brewer.pal(12, "Set3")
col_pal2 = brewer.pal(8, "Dark2")
col_pal3 = brewer.pal(12, "Paired")
col_pal = c(col_pal1, col_pal2, col_pal3)

K_text <- "K ="

nacho = paste(K_text,i)



barplot(native.table, main = "Native", cex.main=2, col=col_pal[1:K], cex.names=1 )
barplot(weed.table, main = "Weed", cex.main=2, col=col_pal[1:K], cex.names=1 )

}
