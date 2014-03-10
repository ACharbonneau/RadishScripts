source('/Volumes/Storage/RadishData/RadishScripts/Misc_scripts/Functionarium.R', chdir = TRUE)
genotyping <- read.csv("/Volumes/Storage/RadishData/2005MarkerData/STRUCTURE/SNPssrFormatted.csv", na.strings="-9")

collapselist <- c("DWRD_124","DWRD_112","DWRD_61","DWRD_177","DWRD_107","DWRD_123",
"DWRD_121","DWRD_158","DWRD_48","DWRD_180","DWRD_97","DWRD_205",
"DWRD_27","Bn26a","BRMS005","Na10H06","Ra1H08","Ra2E11","Na14E08",
"Bn35d","Na12E05")

for(marker in collapselist){
	genotyping[,marker] <- as.numeric(genotyping[,paste(marker,"_1", sep="")] == genotyping[,paste(marker,"_2", sep="")])
}

head(genotyping)

populationslist <- list(genotyping$PopData)

numberhomo <- aggregate(genotyping[50:70], by=populationslist, FUN=sum2)

numbertotal <- aggregate(genotyping[50:70], by=populationslist, FUN=length2)

numberhet <- cbind(numbertotal[1], 
	numbertotal[2:length(colnames(numbertotal))] - 	
	numberhomo[2:length(colnames(numberhomo))])

percentHet <- t(signif( 
	numberhet[2:length(colnames(numberhet))] /  	
	numbertotal[2:length(colnames(numbertotal))], 3))
	
rownames(percentHet) <- colnames(numberhet[2:length(colnames(numberhet))])
colnames(percentHet) <- numbertotal$Group.1

meanHet <- colMeans(percentHet, na.rm=T)

#for(marker in collapselist){
#	genotyping[,paste(marker,"_1", sep="")] <- as.factor(genotyping[,paste(marker,"_1", sep="")])
#	
#	genotyping[,paste(marker,"_2", sep="")] <- as.factor(genotyping[,paste(marker,"_2", sep="")])
#}


#exphet <- rep(NA, length(levels(genotyping$PopData)))
exphet <- NA
for(pop in levels(genotyping$PopData)){
	allmarkers <- genotyping[genotyping$PopData == pop,]
	#print(pop)
	n <- 1
	tempHets <- rep(NA, length(collapselist))	
		for(marker in collapselist){
			#print(marker)
			marker1 <- paste(marker, "_1", sep="")
			marker2 <- paste(marker, "_2", sep="")
			bothalleles <- as.factor(c(allmarkers[,marker1], allmarkers[,marker2]))
			#print(bothalleles)
			tempHets[n] <- sum((table(bothalleles)/length(bothalleles))^2)
			#print(tempHets[n])
			n <- n+1
		}	
	exphet[pop] <- 1 - ((1 / length(collapselist)) * sum(tempHets))	

}

meanVSexp <- cbind(meanHet, exphet[2:length(exphet)])
colnames(meanVSexp) <- c("meanHeterozygosity", "Expected")