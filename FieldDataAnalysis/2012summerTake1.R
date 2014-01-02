require(RColorBrewer)
source('/Volumes/Storage/RadishData/RadishScripts/Misc_scripts/Functionarium.R', chdir = TRUE)

plants2012 <- read.csv("/Volumes/Storage/RadishData/Summer2012Planting/2012FieldData.csv", sep=",", na.strings="")

populations <- levels(plants2012$Code)

w <- "Weed"
c <- "Crop"
n <- "Native"
h <- "Hybrid"
ra <- "RA"

proportions <- cbind(populations, rep(NA, length(populations)), rep(NA, length(populations)), c(w,h,w,w,h,n,c,c, n,w,n,w,c,n,n,w,rep(ra,21),w,n,h,c,c), rep(NA, length(populations)))

colnames(proportions) <- c("populations", "prop", "N", "type","SE")
proportions <- as.data.frame(proportions)
proportions$prop <- as.numeric(proportions$prop)
proportions$N <- as.numeric(proportions$N)
proportions$SE <- as.numeric(proportions$SE)

for(pop in populations){
	proportions[proportions[,1]==pop,2] <- sum(plants2012$Flower_before_Winter[plants2012$Code==pop], na.rm=T)/length(plants2012$Germ_Date[plants2012$Code==pop])
	proportions[proportions[,1]==pop,3] <- length(plants2012$Germ_Date[plants2012$Code==pop])
	proportions[proportions[,1]==pop,5] <- SEP(
	proportions[proportions[,1]==pop,2], proportions[proportions[,1]==pop,3])
}

bytype <- order(proportions$prop,proportions$prop[proportions$type], decreasing=T)

shades <- brewer.pal(5,"Set1")



#  Plotting proportion to match 2013 data

par(mfrow=c(1,1), las=3)

barplot(as.numeric(proportions$prop)[bytype],
	names.arg=proportions$populations[bytype],
	col=shades[proportions$type][bytype],
	ylab="Proportion Flowered", 
	main="Summer 2012",
	xlim=c(-.15, 53), ylim=c(-.03,1),
	cex.names=0.8
) 

legend(40, 0.85,
	legend=levels(as.factor(proportions$type)),
	pch=16, col=shades, bty="n", cex=1)
xcords <- seq(.7, 50, by=1.2)

text(xcords, rep(-.02, 27), proportions$N[bytype], cex=.8, )
text(-.5, -.02, "N=", cex=.8)
 
arrows(xcords,as.numeric(proportions$prop)[bytype], xcords, as.numeric(proportions$prop)[bytype] + as.numeric(proportions$SE)[bytype],length=.05, angle=90 )

arrows(xcords,as.numeric(proportions$prop)[bytype], xcords, as.numeric(proportions$prop)[bytype] - as.numeric(proportions$SE)[bytype],length=.05, angle=90 )






##################Native Flowering Time#########################
## This is an attempt to recreate the type of graph in Salhi with my 2012 data. It shows the distribution of bolting and flowering time for my plants, seperated by plant type(native, weed, crop).

noRAhybrid <- plants2012[plants2012$group!="unknown",] 
noRAhybrid <- droplevels(noRAhybrid[noRAhybrid$group!="cross",])

noRAhybrid$Bolt_int <- cut(noRAhybrid$Days_Germ_Bolt, breaks=seq(20,370, by=10))
noRAhybrid$Flow_int <- cut(noRAhybrid$Days_Germ_Flow, breaks=seq(20,370, by=10))


### Natives
natives <- noRAhybrid[noRAhybrid$group=="native",]

summary(natives$Days_Germ_Bolt)
nat_shades <- brewer.pal(6,"Accent")

nat_flow_table <- table(droplevels(natives$Code), natives$Flow_int)
nat_bolt_table <- table(droplevels(natives$Code), natives$Bolt_int)

#table(natives$Code, natives$Days_Germ_Flow)
#table(natives$Code, natives$Days_Germ_Bolt)

# Weeds

weeds <- noRAhybrid[noRAhybrid$group=="weed",]

summary(weeds$Days_Germ_Bolt)
weed_shades <- brewer.pal(7,"Accent")

weed_flow_table <- table(droplevels(weeds$Code), weeds$Flow_int)
weed_bolt_table <- table(droplevels(weeds$Code), weeds$Bolt_int)

# Crops

crops <- noRAhybrid[noRAhybrid$group=="crop",]

summary(crops$Days_Germ_Bolt)
crop_shades <- brewer.pal(6,"Accent")

crop_flow_table <- table(droplevels(crops$Code), crops$Flow_int)
crop_bolt_table <- table(droplevels(crops$Code), crops$Bolt_int)


#Plot all the things

##Plot of DTB
#par(mfrow=c(4,1), las=2)

## These make graphs with white writing for putting in dark powerpoints
#par(las=2, mar=c(8,4,4,2), col="white", col.axis="white", col.main="white", col.sub="white", fg="white")
barplot(table(noRAhybrid$group, noRAhybrid$Bolt_int), 
	col=shades, 
#	main="2012 Bolting Distribution", 
#	xlab="Days to Bolting", 
#	ylab="Individuals", 
cex.names=.8)

legend(20,25, legend=levels(droplevels(noRAhybrid$group)), bty="n", pch=15, col=shades, cex=2)

par(las=0)
mtext("Individuals", side=2, line=2, cex=1.5)
mtext("Days to bolting", side=1, line=5, cex=1.5)
mtext("2012 Bolting Distribution", side=3, cex=2)

#par(las=2, mar=c(8,4,4,2), col="white", col.axis="white", col.main="white", col.sub="white", fg="white")
barplot(nat_bolt_table,
	beside=FALSE, col=nat_shades,
#	main="Native Days to Bolting",
	xlim=c(0, length(levels(noRAhybrid$Bolt_int))+7),
	ylim=c(0,10), cex.name=.8)

nat_names <- levels(droplevels(natives$Code))
nat_names[4] <- "NZIL"	
legend(16, 8, 
	legend=nat_names,
	col=nat_shades, pch=15, bty="n", ncol=2, cex=1.5)

par(las=0)
mtext("Individuals", side=2, line=2, cex=1.5)
mtext("Days to bolting", side=1, line=5, cex=1.5)
mtext("2012 Native Bolting Distribution", side=3, cex=2)

barplot(weed_bolt_table,
	beside=FALSE, col=weed_shades,
	main="Weed Days to Bolting",
	xlim=c(0, length(levels(noRAhybrid$Bolt_int))+7),
	ylim=c(0,10))
	
legend(13, 10, 
	legend=levels(droplevels(weeds$Code)),
	col=weed_shades, pch=16, bty="n", ncol=3)

barplot(crop_bolt_table,
	beside=FALSE, col=crop_shades,
	main="Crop Days to Bolting",
	xlim=c(0, length(levels(noRAhybrid$Bolt_int))+7),
	ylim=c(0,10))
	
legend(13, 10, 
	legend=levels(droplevels(crops$Code)),
	col=crop_shades, pch=16, bty="n", ncol=3)


########### Plot of raphanistrum #############
par(mfrow=c(1,1), las=3, mar=c(7,4,4,2))
#par(las=3, mar=c(8,4,4,2), col="white", col.axis="white", col.main="white", col.sub="white", fg="white")
raphanistrum <- noRAhybrid[noRAhybrid$Code=="PBFR"| 
	noRAhybrid$Code=="CBES" | 
	noRAhybrid$Code=="MAES" | 
	noRAhybrid$Code=="SAES" | 
	noRAhybrid$group=="weed", ]
 raphanistrum$Code <- droplevels(  raphanistrum$Code )

levels( raphanistrum$Code )

typemap <- c(AFFR="Weed", AL="Weed", BINY="Weed", CBES="CBES", M3="Weed", MAES="MAES", N3="Weed", PBFR="PBFR", PG6="Weed", REIL="Weed", SAES="SAES")

raphanistrum$graphcode <- factor( typemap[raphanistrum$Code] )

summary(raphanistrum$graphcode)
raph_shades <- brewer.pal(6,"Accent")
raph_shades <- c(raph_shades[1:4], raph_shades[6])

barplot(table(raphanistrum$graphcode, raphanistrum$Bolt_int), 
	col=raph_shades, 
#	main="2012 Bolting Distribution", 
#	xlab="Days to Bolting", 
#	ylab="Individuals", 
cex.names=.8)

legend(20,20, legend=levels(droplevels(raphanistrum$graphcode)), bty="n", pch=15, col=raph_shades, cex=1.5)

par(las=0)
mtext("Individuals", side=2, line=2, cex=1.5)
mtext("Days to bolting", side=1, line=5, cex=1.5)
mtext("2012 Bolting Distribution", side=3, cex=2)


### Plots of DTF
par(mfrow=c(4,1), las=2)

barplot(table(noRAhybrid$group, noRAhybrid$Flow_int), 
	col=shades, main="2012 Flowering Distribution", 
#	xlab="Days to Flowering", 
	ylab="Individuals")

legend(20,25, legend=levels(droplevels(noRAhybrid$group)), bty="n", pch=16, col=shades, cex=1.2)

barplot(nat_flow_table,
	beside=FALSE, col=nat_shades,
	main="Native Days to Flowering",
	xlim=c(0, length(levels(noRAhybrid$Flow_int))+7),
	ylim=c(0,10))
	
legend(13, 10, 
	legend=levels(droplevels(natives$Code)),
	col=nat_shades, pch=16, bty="n", ncol=3)

barplot(weed_flow_table,
	beside=FALSE, col=weed_shades,
	main="Weed Days to Flowering",
	xlim=c(0, length(levels(noRAhybrid$Flow_int))+7),
	ylim=c(0,10))
	
legend(13, 10, 
	legend=levels(droplevels(weeds$Code)),
	col=weed_shades, pch=16, bty="n", ncol=3)

barplot(crop_flow_table,
	beside=FALSE, col=crop_shades,
	main="Crop Days to Flowering",
	xlim=c(0, length(levels(noRAhybrid$Flow_int))+7),
	ylim=c(0,10))
	
legend(13, 10, 
	legend=levels(droplevels(crops$Code)),
	col=crop_shades, pch=16, bty="n", ncol=3)




