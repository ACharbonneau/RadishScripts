#Prelimanary poking at summer 2013 dataset

require(RColorBrewer)
plants2013 <- read.csv("/Users/Amanda/Dropbox/RadishWeedEvolution/Summer2013/2013plantData.csv", sep=",", na.strings="")



summary(plants2013)
germtable <- table(plants2013$Name, plants2013$Germ.Date)
summary(germtable)
germinated <- apply(germtable, 1, sum)
germtable

diedtable <- table(plants2013$Name, plants2013$Died)
died <- apply(diedtable, 1, sum)
died
germinated - died

plants2013$GD <- strptime(plants2013$Germ.Date, format="%m/%d")
plants2013$FD <- strptime(plants2013$Flower..Date, format="%m/%d")

plants2013$DTF <- plants2013$FD - plants2013$GD

dtftable <- table(plants2013$Name, plants2013$DTF)
plot(plants2013$DTF[plants2013$Name=="RACA"], seq(1,10))
plants2013$DTF[plants2013$Name=="RACA"]

## Plot DTF
par(mfrow= c(6,5))
populations <- levels(plants2013$Name)

for(pop in populations){

MTpop <- rep(NA, 10)		

current <- sort(plants2013$DTF[plants2013$Name==pop])

if(length(current) > 0){
	MTpop[seq(1,length(current))] <- current
}
	
	plot(x=MTpop,
	y=seq(1,10),
	ylim=c(0,10), xlim=c(15, 60),
	xlab=c(pop, "DTF"), ylab=c(length(current), "flowered"),
	pch=16, col="blue")	
}

#  Plot DTF by HoFF

par(mfrow= c(6,5))

for(pop in populations){
	
current <- sort(plants2013$DTF[plants2013$Name==pop])

	plot(x=plants2013$DTF[plants2013$Name==pop],
	y=plants2013$Height.1st..Flower..cm.[plants2013$Name==pop],
	ylim= range(as.numeric(plants2013$Height.1st..Flower..cm.), na.rm=T),
	xlim=range(as.numeric(plants2013$DTF), na.rm=T),
	xlab=c(pop, "DTF"), ylab=c("flower height (cm)"),
	pch=16, col="blue")	
}

#  Plot proportion flowered w/o vernalization

par(mfrow= c(1,1), las=3)
palette <- brewer.pal(c(6),"Set1")
palette <- palette[c(1,3,5)]

w <- "Weed"
n <- "Native"
c <- "Crop"

proportions <- cbind(populations, rep(NA, length(populations)), rep(NA, length(populations)), c(w,w,c,n,rep(c,5),n,c,w,c,n,rep(c,9),n,c,c,c))
colnames(proportions) <- c("populations", "prop", "N", "type")
proportions <- as.data.frame(proportions)
proportions$prop <- as.numeric(proportions$prop)
proportions$N <- as.numeric(proportions$N)

for(pop in populations){
	proportions[proportions[,1]==pop,2] <- length(na.omit(plants2013$Flower..Date[plants2013$Name==pop]))/length(na.omit(plants2013$Germ.Date[plants2013$Name==pop]))
	proportions[proportions[,1]==pop,3] <- length(na.omit(plants2013$Germ.Date[plants2013$Name==pop]))
}

bytype <- order(proportions$prop,proportions$prop[proportions$type], decreasing=T)

barplot(as.numeric(proportions$prop)[bytype], names.arg= proportions$populations[bytype], col=palette[proportions$type][bytype], ylim=c(-.03,1), xlim=c(-.15, 32), ylab="Proportion Flowered", main="Spring 2013") 
legend(25, .8,legend=levels(as.factor(proportions$type)),pch=16, col=palette, bty="n")
