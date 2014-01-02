#Prelimanary poking at summer 2013 dataset

require(RColorBrewer)
source('/Volumes/Storage/RadishData/RadishScripts/Misc_scripts/Functionarium.R', chdir = TRUE)

plants2013 <- read.csv("/Users/Amanda/Dropbox/RadishWeedEvolution/Summer2013/2013plantData.csv", sep=",", na.strings="")

# Make dates into something R understands
plants2013$GD <- strptime(plants2013$Germ.Date, format="%d-%b")
plants2013$FD <- strptime(plants2013$Flower..Date, format="%d-%b")
plants2013$DTF <- plants2013$FD - plants2013$GD


# Look at some summary stats
summary(plants2013)

# Germination rates
germtable <- table(plants2013$Name, plants2013$Germ.Date)
summary(germtable)
germinated <- apply(germtable, 1, sum)
germtable

# Survival
diedtable <- table(plants2013$Name, plants2013$Died)
died <- apply(diedtable, 1, sum)
died
germinated - died

# DTF
dtftable <- table(plants2013$Name, plants2013$DTF)
plot(plants2013$DTF[plants2013$Name=="RACA"], seq(1,10))
plants2013$DTF[plants2013$Name=="RACA"]

## Plot DTF
par(mfrow= c(6,5))
populations <- levels(plants2013$Name)

for(pop in populations){

MTpop <- rep(NA, 10)	 #make an empty set for each population so they'll all have 10 data points even if they haven't 	

current <- sort(plants2013$DTF[plants2013$Name==pop]) #See if there are any that have flowered

if(length(current) > 0){
	MTpop[seq(1,length(current))] <- current
} #If there are some that flowered, replace that many NA's with their DTF
	
	plot(x=MTpop,
	y=seq(1,10),
	ylim=c(0,10), xlim=c(15, 60),
	xlab=c(pop, "DTF"), ylab=c(length(current), "flowered"),
	pch=16, col="blue")	
} #Plot all the ones that flowered, plus the NA's so all the graphs have the same dim.

#  Plot DTF by Height of first flower

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


######  Plot proportion flowered w/o vernalization rather than by DTF #####

par(mfrow= c(1,1), las=3)
palette <- brewer.pal(c(6),"Set1")
palette <- palette[c(1,3,5)] #using the same colors as the 2012 data set

w <- "Weed"
n <- "Native"
c <- "Crop"

	# Make a table to hold the graphing data
proportions <- cbind(
	populations, 
	rep(NA, length(populations)), 
	rep(NA, length(populations)), 
	c(w,w,c,n,rep(c,5),n,c,w,c,n,rep(c,9),n,c,c,c),
	rep(NA, length(populations))
)

	#Make the empty table into a dataframe with correct data types
colnames(proportions) <- c("populations", "prop", "N", "type", "SE")
proportions <- as.data.frame(proportions)
proportions$prop <- as.numeric(proportions$prop)
proportions$N <- as.numeric(proportions$N)
proportions$SE <- as.numeric(proportions$SE)

	#Fill the table with proportion flowered, N and SE
for(pop in populations){
	proportions[proportions[,1]==pop,2] <-CalcProp(
		plants2013$Flower..Date[plants2013$Name==pop],
		plants2013$Germ.Date[plants2013$Name==pop])
	proportions[proportions[,1]==pop,3] <- length(na.omit(plants2013$Germ.Date[plants2013$Name==pop]))
	proportions[proportions[,1]==pop,5] <- SEP(
		proportions[proportions[,1]==pop,2], proportions[proportions[,1]==pop,3])
}
	# Order the pops by their proportion flowered to make it pretty. Group pops.

bytype <- order(proportions$prop,proportions$prop[proportions$type], decreasing=T)

	#Plot as bargraph.
barplot(as.numeric(proportions$prop)[bytype], names.arg= proportions$populations[bytype], col=palette[proportions$type][bytype], ylim=c(-.03,1), xlim=c(-.15, 32), ylab="Proportion Flowered", main="Spring 2013") 
legend(25, .8,legend=levels(as.factor(proportions$type)),pch=16, col=palette, bty="n")


xcords <- seq(.7, 32, by=1.2)


	#This puts the N across the bottom of the graph, just above labels
text(xcords, rep(-.02, 27), proportions$N[bytype], cex=.8, )
text(-.5, -.02, "N=", cex=.7)

arrows(xcords,as.numeric(proportions$prop)[bytype], xcords, as.numeric(proportions$prop)[bytype] + as.numeric(proportions$SE)[bytype],length=.05, angle=90 )

arrows(xcords,as.numeric(proportions$prop)[bytype], xcords, as.numeric(proportions$prop)[bytype] - as.numeric(proportions$SE)[bytype],length=.05, angle=90 )
