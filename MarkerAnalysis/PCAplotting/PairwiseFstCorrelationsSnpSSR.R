# Pairwise Fst comparison between SSRs and SNPs
#Written by Ian Dworkin
### AMANDA &/or JEFF . you might have to change the working directory to something like the following (depending on the name of your home directory on your machine)
# change ian to amanda or jeff.
setwd("/Users/ian/Dropbox/RadishWeedEvolution/PopulationStructure/Eigensoft")

SNP <- read.table("SNPpairwiseFST.txt", skip=2, col.names=c("POP1", "POP2","Fst","SE"))
SSR <- read.table("SSRpairwiseFST.txt", skip=2, col.names=c("POP1", "POP2","Fst","SE"))


# I have set these to read from dropbox, but they currently do not work!
# SNP <- read.table("https://www.dropbox.com/s/usxtc6qvnleyrrd/SNPpairwiseFST.txt", skip=2, col.names=c("POP1", "POP2","Fst","SE"))
# SSR <- read.table("https://www.dropbox.com/s/lvtnuq5sk9vhnrr/SSRpairwiseFST.txt", skip=2, col.names=c("POP1", "POP2","Fst","SE"))

plot(SNP[,3], SSR[,3], 
  col=densCols(SNP[,3], SSR[,3]), pch=16,
  ylab="SSR", xlab="SNP", main="Correlation between pairwise Fst (SNP vs. SSR)")
text(x=0.15, y=0.7, labels="Pearson r = 0.34")
cor(SNP[,3], SSR[,3])

# Confirming there was no differences in ordering.
SSR_SNP <- merge(SNP, SSR, by = intersect(names(SNP[,1:2]), names(SSR[,1:2])))
cor(SSR_SNP[,3], SSR_SNP[,5],use="everything", method= "spearman")

# What happens if we remove the cases where Fst = 0

SSR_SNP_subset <- SSR_SNP[SSR_SNP$Fst.y >0,] 
cor(SSR_SNP_subset[,3], SSR_SNP_subset[,5], method= "pearson")

plot(SSR_SNP_subset[,3], SSR_SNP_subset[,5], 
  col=densCols(SSR_SNP_subset[,3], SSR_SNP_subset[,5]), pch=16,
  ylab="SSR", xlab="SNP", main="Correlation between pairwise Fst subset (SNP vs. SSR)")
text(x=0.15, y=0.7, labels="Pearson r = 0.55")
#Correlation goes up to 0.55


SSR_SNP_subsetFst0 <- SSR_SNP[SSR_SNP$Fst.y ==0,] 
# The 0 Fst values are due to pop NZIL.
SSR_SNP_subsetFst0