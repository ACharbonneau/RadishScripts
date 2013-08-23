require(RColorBrewer)

plants2012 <- read.csv("/Volumes/Storage/RadishData/Summer2012Planting/2012FieldData.csv", sep=",", na.strings="")

populations <- levels(plants2012$Code)

w <- "Weed"
c <- "Crop"
n <- "Native"
h <- "Hybrid"
ra <- "RA"

proportions <- cbind(populations, rep(NA, length(populations)), rep(NA, length(populations)), c(w,h,w,w,h,n,c,c, n,w,n,w,c,n,n,w,rep(ra,21),w,n,h,c,c))

colnames(proportions) <- c("populations", "prop", "N", "type")
proportions <- as.data.frame(proportions)
proportions$prop <- as.numeric(proportions$prop)
proportions$N <- as.numeric(proportions$N)


for(pop in populations){
	proportions[proportions[,1]==pop,2] <- sum(plants2012$Flower_before_Winter[plants2012$Code==pop], na.rm=T)/length(plants2012$Germ_Date[plants2012$Code==pop])
	proportions[proportions[,1]==pop,3] <- length(plants2012$Germ_Date[plants2012$Code==pop])
}

bytype <- order(proportions$prop,proportions$prop[proportions$type], decreasing=T)

shades <- brewer.pal(5,"Set1")

#  Plotting

par(c(1,1), las=3)

barplot(as.numeric(proportions$prop)[bytype],
	names.arg=proportions$populations[bytype],
	col=shades[proportions$type][bytype],
	ylab="Proportion Flowered, N=10", 
	main="Summer 2012",
	cex.names=0.8
) 

legend(40, 0.85,
	legend=levels(as.factor(proportions$type)),
	pch=16, col=shades, bty="n")
 

text(c(.7, seq(2, 14, by=1.18), seq(15.2, 32, by=1.18)), rep(-.02, 27), proportions$N[bytype], cex=.8, )
text(-.5, -.02, "N=", cex=.7)