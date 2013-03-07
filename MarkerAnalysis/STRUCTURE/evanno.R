#This takes output from the StructureHarvester program and plots the DeltaK (evanno method) and mean LnP (normal method) of determining the best K. The LnP should give roughly the same answer as the final plot of PlotKprobabilities. 

source('/Volumes/Storage/RadishData/RadishScripts/Misc_scripts/AmandaSource.R', chdir = TRUE)

evanno <- read.csv("/Volumes/Storage/RadishData/2005MarkerData/STRUCTURE/RedoneStructure/EstimateK/HarvestCorrNoRA/evanno.txt", skip=15, header=T, sep="	")

colnames(evanno) <- c("K", "Reps", "MeanLnP", "StDev", "Ln_K", "Ln__K", "DeltaK")

pdf(file="/Volumes/Storage/RadishData/2005MarkerData/STRUCTURE/RedoneStructure/EstimateK/Plots/EvannoProbOfK.pdf")
plot(evanno$K, evanno$DeltaK, main = "Best K using Evanno Method", ylab="DeltaK", xlab="Proposed K")
text(x= evanno$K, y= evanno$DeltaK, pos=1, cex=.7)

plot(evanno$K, evanno$MeanLnP, main = "Best K using mean Ln P", ylab = "Ln probability", xlab="Proposed K")
text(x= evanno$K, y= evanno$MeanLnP, pos=1, cex=.7)

dev.off()






plot(evanno$K, evanno$Ln_K)
plot(evanno$K, evanno$Ln__K)
plot(evanno$K, evanno$MeanLnP)
