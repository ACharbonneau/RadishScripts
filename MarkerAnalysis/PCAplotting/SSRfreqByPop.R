setwd("~/Documents/RadishData/MarkerData/Phylip/")
AllData <- read.csv("FormatMarkers.csv", header=TRUE)

#read in data
SNPdata <- data.frame(AllData[,1:14])
SNPdata$X <- factor(SNPdata$X)
SSRdata <- data.frame(AllData[,c(1,15:30)])
str(SSRdata)

#Determine number of alleles represented in all populations of two column SSR data
Poss_alleles <- function(x){
	matrix(as.numeric(levels(sort(factor(na.omit(x))))))
}

#Gives frequency of all SSR alleles based on 2 columns of repeat genotype data, excluding missing values
SSR_freq <- function(x,y){
	x <- x[!is.na(x)]
	n <- length(x)
	fac <- factor(x)
	alleles <- levels(fac)
	counts <- matrix(table(fac))
	counts/n
}
#for each marker, combine the two columns of data. Use this info to make a matrix that has populations in colunms, and allele classes as rows
for(s in seq(from=2, to=16, by=2)){
	pops <- nlevels(SSRdata$X)
	alleles <- nlevels(factor(append(SSRdata[,s], SSRdata[,s+1])))
	MATRIX <- matrix(NA, ncol=pops, nrow=alleles)
	row.names(MATRIX) <- Poss_alleles(append(SSRdata[,s], SSRdata[,s+1]))
	colnames(MATRIX) <- levels(SSRdata$X)
	BothAlleles <- data.frame(cbind(rep(SSRdata$X,2),
	append(SSRdata[,s],SSRdata[,s+1])))
	colnames(BothAlleles) <- c("pop","rpeat")
#for each population, determine the alleles present and feed them into the SSR_freq function. Take the output and assign each frequency to the pre-made matrix in the position that coorosponds with its allele class. Leave all other spaces as NA.
	for(p in 1:48){
		MATRIX[c(levels(factor(BothAlleles$rpeat[BothAlleles$pop==p]))),p] <-
		SSR_freq(BothAlleles$rpeat[BothAlleles$pop==p])
		}
#print(MATRIX)
#Print out each successive MATRIX into a txt document, make all the NA's 0 frequencies
write.table(MATRIX, file="SSR_allele_freq.csv", append=TRUE, na="0", sep=",")
	}
	
#loop finds SSR frequency data that doesn't sum to 1	
#errors <- (ErrorList)
#for (marker in seq(from=2, to=16, by=2)) {
#	for (pop in 1:48) {
#		test <- sum( unlist(SSR_allele_freq[[marker]][pop] ))
#		if ( test != 1){
#			errors <- append(errors,(c(SSR_allele_freq[[marker]][pop], test,
#			names(SSR_allele_freq[marker]))))
#} } } 	
