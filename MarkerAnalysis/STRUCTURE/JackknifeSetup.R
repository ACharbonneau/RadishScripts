
setwd("/Volumes/Storage/RadishData/2005MarkerData/STRUCTURE/RedoneStructure/EstimateK/Corrfreq/NoRACoNoJackknife/Jackknife")
NoRApheno <- read.table("/Volumes/Storage/RadishData/2005MarkerData/STRUCTURE/NoRApheno.txt", skip=1, colnames= c(1:49))

markerA <- seq(7, 45, 2)
markerB <- seq(10, 48, 2)

names <- c("DWRD_124.txt", "DWRD_112.txt", "DWRD_61.txt", "DWRD_177.txt", "DWRD_107.txt", "DWRD_123.txt", "DWRD_121.txt", "DWRD_158.txt", "DWRD_48.txt", "DWRD_180.txt", "DWRD_97.txt", "DWRD_205.txt", "DWRD_27.txt", "Bn26a.txt", "BRMS005.txt", "Na10H06.txt", "Ra1H08.txt", "Ra2E11.txt", "Na14E08.txt", "Bn35d.txt", "Na12E05.txt")



for(i in 1:20){
    newset <- NoRApheno[,c(1:markerA[i], markerB[i]:49)]
    
    write.table(newset, names[i], sep=" ", row.names=FALSE, col.names=FALSE) 
    }

lastset <- NoRApheno[,c(1:47)]
write.table(lastset, names[21], sep=" ", row.names=FALSE, col.names=FALSE) 

