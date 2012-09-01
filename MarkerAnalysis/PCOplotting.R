library(ade4)
library(MASS)

markers <- read.csv("~/Documents/RadishData/2005MarkerData/SSRSNPallBiallelic.csv", na.strings="9")

str(markers)

marker.dist <- dist(na.omit(markers[2:122]), method="euclidean")

marker.pco <- ade4::dudi.pco(marker.dist)

plot(marker.pco$l1$RS1, -1*(marker.pco$l1$RS2)) 

marker.cmd <- cmdscale(marker.dist, k=2, eig=TRUE)

plot(marker.cmd$points)

