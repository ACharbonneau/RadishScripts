taco <- read.csv("/Volumes/Storage/RadishData/2005MarkerData/FST_Distance.csv", header=T)

nacho <- read.csv("/Volumes/Storage/RadishData/2005MarkerData/FST_DistanceNative.csv", header=T)


plot(taco$FST, taco$PhyDist, pch=15, col=c("red", "blue", "goldenrod", "green4", "brown", "black", "purple")[taco$Pop1], cex=1.7, )
par(new=T)
plot(taco$FST, taco$PhyDist, pch=17, col=c("red", "blue", "goldenrod", "green4", "brown", "black", "purple")[taco$Pop2], cex=1.3)

legend(0.055, 13000, legend=levels(taco$Pop1), pch=c(15,25,17,18,19,20,21))

legend(0.025, 13000, legend=levels(taco$Pop2), pch=16, col=c("red", "blue", "goldenrod", "green4", "brown", "black", "purple"))


legend(0.025, 13000, legend=levels(taco$Pop1), pch=16, col=c("red", "blue", "goldenrod", "green4", "brown", "black", "purple"))



plot(taco$FST, taco$PhyDist, pch=15, xlab="Pairwise Fst", ylab="Pairwise Physical Distance, kilometers", main="Isolation by Distance of weeds" )

plot(nacho$FST, nacho$PhyDist, pch=15, xlab="Pairwise Fst", ylab="Pairwise Physical Distance, kilometers", main="Isolation by Distance of natives" )