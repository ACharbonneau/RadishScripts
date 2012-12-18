
source('~/Documents/RadishData/RadishScripts/Misc_scripts/AmandaSource.R', chdir = TRUE)

evanno <- read.csv("~/Documents/RadishData/2005MarkerData/STRUCTURE/RedoneStructure/EstimateK/HarvestCorrNoRA/evanno.txt", skip=15, header=T, sep="	")

colnames(evanno) <- c("K", "Reps", "MeanLnP", "StDev", "Ln_K", "Ln__K", "DeltaK")

plot(evanno$K, evanno$MeanLnP)
plot(evanno$K, evanno$DeltaK)
plot(evanno$K, evanno$Ln_K)
plot(evanno$K, evanno$Ln__K)