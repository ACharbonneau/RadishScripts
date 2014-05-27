alleles <- read.csv("/Volumes/Storage/RadishData/2005MarkerData/Phylip/AllelesSSRSNP.csv")

for( i in 1:nrow(alleles)){ 
	vari <- alleles[i,"X"]
	vari[2] <- list(colnames(alleles[i,] !=0 & alleles[i,] !=NA))
	print(vari)
	}

if (alleles[i,] != 0){ print "taco"}

crap<-for(i in 1:nrow(alleles)){ print(alleles[i,]==0) }