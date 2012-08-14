setwd("~/Documents/RadishData/MarkerData/Phylip/")
AllData <- read.csv("FormatMarkers.csv", header=TRUE)

#read in data
SNPdata <- data.frame(AllData[,1:14])
SNPdata$X <- factor(SNPdata$X)
SSRdata <- data.frame(AllData[,c(1,15:30)])
str(SSRdata)

#Gives frequency of one SNP allele based on single column of (0,1,2) genotype data excluding missing values
SNP_freq <- function(x){
	x <- x[!is.na(x)]
	n <- length(x)
	round(sum(x)/(2*n), 3)
}
#Build a matrix to hold the output
SNP_allele_freq <- matrix(NA, nrow=length(levels(SNPdata$X)), ncol=(ncol(SNPdata)) )

#Use the SNP_freq function to find the allele frequency for each marker indexed by population
for(N in 1:ncol(SNPdata)){
	if(N == 1) SNP_allele_freq[,N] <- levels(SNPdata$X)
	else
	SNP_allele_freq[,N] <- with(SNPdata, 
	tapply(X=SNPdata[,N], INDEX=SNPdata$X, FUN= SNP_freq))
	colnames(SNP_allele_freq) <- colnames(SNPdata)
}
write.csv(SNP_allele_freq, file="SNP_allele_freq.csv")





