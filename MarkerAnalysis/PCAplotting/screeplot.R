

screeplot <- read.csv("/Volumes/Storage/RadishData/2005MarkerData/SmartPCA/allPCSnoRA/Marker.eval", header=F)
screeplot$V2 <- screeplot$V1/sum(screeplot$V1)

par(mfrow=c(2,1))
barplot(screeplot$V1, ylim=c(0,20), main="Eigenvalues of NoRA pca")

text(130, 5, labels="109")
arrows(130, 4, 130, 1, length=.1, angle=10)

barplot(screeplot$V2, ylim=c(0,.1), main="%Variance Explained")

text(130,.02, labels="109")
arrows(130, .015, 130, .002, length=.1, angle=10)