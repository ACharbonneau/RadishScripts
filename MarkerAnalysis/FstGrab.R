all <- read.csv("/Volumes/Storage/RadishData/21MarkersData/SpainIsrael2013Pops/ArlResults2014.csv", header=T, row.names=1)
SNP <- read.csv("/Volumes/Storage/RadishData/21MarkersData/SpainIsrael2013Pops/ArlResultsSNP2014.csv", header=T, row.names=1)
Micro <- read.csv("/Volumes/Storage/RadishData/21MarkersData/SpainIsrael2013Pops/ArlResultsMicro2014.csv", header=T, row.names=1)
ReJeff <- read.csv("/Volumes/Storage/RadishData/21MarkersData/SpainIsrael2013Pops/ArlResultsReJeff2014.csv", header=T, row.names=1)
OrgJeff <- read.csv("/Volumes/Storage/RadishData/21MarkersData/SpainIsrael2013Pops/ArlResultsOrgJeff2014.csv", header=T, row.names=1)
ReJeffFix <- read.csv("/Volumes/Storage/RadishData/21MarkersData/SpainIsrael2013Pops/ArlResultsReJeffFix2014.csv", header=T, row.names=1)
RecreateAll <- read.csv("/Volumes/Storage/RadishData/21MarkersData/SpainIsrael2013Pops/ArlResultsRecreateAll2014.csv", header=T, row.names=1)
RecreateAllMicro <- read.csv("/Volumes/Storage/RadishData/21MarkersData/SpainIsrael2013Pops/RecreateAllMicro.csv", header=T, row.names=1)
RecreateAllSNP <- read.csv("/Volumes/Storage/RadishData/21MarkersData/SpainIsrael2013Pops/RecreateAllSNP.csv", header=T, row.names=1)
Cutoff5 <- read.csv("/Volumes/Storage/RadishData/21MarkersData/SpainIsrael2013Pops/cutoff5.csv", header=T, row.names=1)
SNPcutoff5 <- read.csv("/Volumes/Storage/RadishData/21MarkersData/SpainIsrael2013Pops/SNPcutoff5.csv", header=T, row.names=1)
Microcutoff5 <- read.csv("/Volumes/Storage/RadishData/21MarkersData/SpainIsrael2013Pops/Microcutoff5.csv", header=T, row.names=1) #microsat mode
MicroStancutoff5 <- read.csv("/Volumes/Storage/RadishData/21MarkersData/SpainIsrael2013Pops/MicroStandardcutoff5.csv", header=T, row.names=1)


csvlist <- list("all", "SNP", "Micro", "ReJeff", "OrgJeff", "ReJeffFix", "RecreateAll", "RecreateAllMicro", "RecreateAllSNP", "Cutoff5", "SNPcutoff5", "Microcutoff5", "MicroStancutoff5")
csvdata <- list(all, SNP, Micro, ReJeff, OrgJeff, ReJeffFix, RecreateAll, RecreateAllMicro, RecreateAllSNP, Cutoff5, SNPcutoff5, Microcutoff5, MicroStancutoff5)


NativePop <- list("MAES", "PBFR", "CBES", "SAES", "DEES" )
WeedPop <- list("COAU", "WEAU", "AFFR", "AUFI", "NCDE", "BINY")

DataList <- list()

for (i in 1:length(csvlist)){
  #nameofthing <- paste(csvlist[i], "Useful", sep="")
  DataList[[i]] <- data.frame(matrix(NA, ncol=length(NativePop), nrow=length(WeedPop)))
  names( DataList )[i] <- csvlist[i]
  names( DataList[[i]] ) <- NativePop
  row.names( DataList[[i]]) <- WeedPop
}

for (l in 1:length(DataList)){
  for (i in 1:length(NativePop)){
    for (w in 1:length(WeedPop)){
      DataList[[l]][WeedPop[[w]], NativePop[[i]]] <- data.frame(csvdata[l])[NativePop[[i]], WeedPop[[w]]]
}}}

DataList
