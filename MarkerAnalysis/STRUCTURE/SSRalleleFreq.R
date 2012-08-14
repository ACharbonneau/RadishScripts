setwd("~/Documents/RadishData/MarkerData/STRUCTURE/")
SSRs <- read.csv("SSRgenos.txt", header=FALSE)
SSRs
row.names(SSRs) <- c("0repeats", "1repeat", "2repeats", "3repeats", "4repeats", "5repeats", "6repeats", "7repeats", "8repeats", "9repeats", "10repeats", "11repeats", "12repeats", "13repeats", "14repeats", "15repeats", "16repeats", "17repeats", "18repeats", "19repeats", "20repeats", "21repeats", "22repeats", "23repeats", "24repeats", "25repeats") 
colnames(SSRs) <-c("Bn26a","BRMS005","Na10H06","Ra1H08","Ra2E11","Na14E08","Bn35d","Na12E05")
#numbers are the sum of alleles from both chromosomes of all individuals in all populations
N2 <- c(sum(SSRs$V1), sum(SSRs$V2), sum(SSRs$V3), sum(SSRs$V4), sum(SSRs$V5), sum(SSRs$V6), sum(SSRs$V7), sum(SSRs$V8))
N2

freq <- matrix( NA, nrow=nrow(SSRs), ncol=ncol(SSRs) )
for (a in 1:nrow(SSRs)){
	freq[a,] <- unlist(SSRs[a,]/N2)
}

H1 <- matrix( NA, nrow=1, ncol=ncol(freq))
for (h in 1:ncol(freq)){
	H1[,h] <- 1- sum(freq[,h]**2)
}

H1

