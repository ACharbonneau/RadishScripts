require(RColorBrewer)
require(plyr)
require(lattice)
require(car)
source('/Volumes/Storage/RadishData/RadishScripts/Misc_scripts/Functionarium.R', chdir = TRUE)

plants2012 <- read.csv("/Volumes/Storage/RadishData/Summer2012Planting/2012FieldData.csv", sep=",", na.strings="")
lale2005 <- read.csv("/Volumes/Storage/RadishData/Manuscripts/2005markerPaper/OldDatasets/LaleField2005.csv")

############################ Configure the 2012 dataframe #################################
populations <- levels(plants2012$Code)

w <- "Weed"
c <- "Crop"
n <- "Native"
h <- "Hybrid"
ra <- "RA"
shades <- brewer.pal(5,"Set1")

plants2012$Germ_Date <- strptime(plants2012$Germ_Date, format="%m/%d/%y")
plants2012$Died_By <- strptime(plants2012$Died_By, format="%m/%d/%y")
plants2012$Bolted_Date <- strptime(plants2012$Bolted_Date, format="%m/%d/%y")
plants2012$Flowering_Date <- strptime(plants2012$Flowering_Date, format="%m/%d/%y")
plants2012$Date_Blossom_Collected <- strptime(plants2012$Date_Blossom_Collected, format="%m/%d/%y")
plants2012$second_bolt <- strptime(plants2012$second_bolt, format="%m/%d/%y")

#plants2012$Died_before_flowering <- factor(plants2012$Died_before_flowering, levels=c(0,1), labels=c("No", "Yes"))
#plants2012$Died_before_bolting <- factor(plants2012$Died_before_bolting, levels=c(0,1), labels=c("No", "Yes"))
#plants2012$Died_before_flowering <- factor(plants2012$Died_before_flowering, levels=c(0,1), labels=c("No", "Yes"))
#plants2012$Bolt_before_Winter <- factor(plants2012$Bolt_before_Winter, levels=c(0,1), labels=c("No", "Yes"))
#plants2012$Died_from_winter <- factor(plants2012$Died_from_winter, levels=c(0,1), labels=c("No", "Yes"))
#plants2012$Survive_winter <- factor(plants2012$Survive_winter, levels=c(0,1), labels=c("No", "Yes"))
#plants2012$Alive_1_5_13 <- factor(plants2012$Alive_1_5_13, levels=c(0,1), labels=c("No", "Yes"))
#plants2012$Alive_4_17_13 <- factor(plants2012$Alive_4_17_13, levels=c(0,1), labels=c("No", "Yes"))
#plants2012$Alive_5_3_13 <- factor(plants2012$Alive_5_3_13, levels=c(0,1), labels=c("No", "Yes"))
#plants2012$Alive_5_13_13 <- factor(plants2012$Alive_5_13_13, levels=c(0,1), labels=c("No", "Yes"))
#plants2012$Alive_5_28_13 <- factor(plants2012$Alive_5_28_13, levels=c(0,1), labels=c("No", "Yes"))
#plants2012$Tissue_Sample <- factor(plants2012$Tissue_Sample, levels=c(0,1), labels=c("No", "Yes"))
plants2012$germ_group <- factor(plants2012$germ_group)
plants2012$Row <- factor(plants2012$Row)

plants2012$X6_28_L1L <- plants2012$X6_28_L1L/plants2012$X6_28_Rosette_Leaf_Scale
plants2012$X6_28_L1W <- plants2012$X6_28_L1W/plants2012$X6_28_Rosette_Leaf_Scale
plants2012$X6_28_L2L <- plants2012$X6_28_L2L/plants2012$X6_28_Rosette_Leaf_Scale
plants2012$X6_28_L2W <- plants2012$X6_28_L2W/plants2012$X6_28_Rosette_Leaf_Scale
plants2012$X6_28_L3L <- plants2012$X6_28_L3L/plants2012$X6_28_Rosette_Leaf_Scale
plants2012$X6_28_L3W <- plants2012$X6_28_L3W/plants2012$X6_28_Rosette_Leaf_Scale


plants2012$X7_26_L1L <- plants2012$X7_26_L1L/plants2012$X7_26_Rosette_Leaf_Scale
plants2012$X7_26_L1W <- plants2012$X7_26_L1W/plants2012$X7_26_Rosette_Leaf_Scale
plants2012$X7_26_L2L <- plants2012$X7_26_L2L/plants2012$X7_26_Rosette_Leaf_Scale
plants2012$X7_26_L2W <- plants2012$X7_26_L2W/plants2012$X7_26_Rosette_Leaf_Scale
plants2012$X7_26_L3L <- plants2012$X7_26_L3L/plants2012$X7_26_Rosette_Leaf_Scale
plants2012$X7_26_L3W <- plants2012$X7_26_L3W/plants2012$X7_26_Rosette_Leaf_Scale

plants2012$AvgL_6_28 <- ave(plants2012$X6_28_L1L, plants2012$X6_28_L2L, plants2012$X6_28_L3L)
plants2012$AvgW_6_28 <- ave(plants2012$X6_28_L1W, plants2012$X6_28_L2W, plants2012$X6_28_L3W)
plants2012$AvgL_7_26 <- ave(plants2012$X7_26_L1L, plants2012$X7_26_L2L, plants2012$X7_26_L3L)
plants2012$AvgW_7_26 <- ave(plants2012$X7_26_L1W, plants2012$X7_26_L2W, plants2012$X7_26_L3W)
plants2012$rosetteJune <- .5* (plants2012$AvgL_6_28 * plants2012$AvgW_6_28) * plants2012$X6_28_Leaf_Number_Josh
plants2012$rosetteJuly <- .5* (plants2012$AvgL_7_26 * plants2012$AvgW_7_26) * plants2012$X7_26_Leaf_Number_Josh

#Standardize (s) and center (c) data

plants2012$DTB_s <- plants2012$Days_Germ_to_Bolt - mean(plants2012$Days_Germ_to_Bolt, na.rm=T)
plants2012$DTF_s <- plants2012$Days_Germ_to_Flow - mean(plants2012$Days_Germ_to_Flow, na.rm=T)
plants2012$rosetteJune_sc <- (plants2012$rosetteJune -mean(plants2012$rosetteJune, na.rm=T))/sd(plants2012$rosetteJune, na.rm=T)
plants2012$rosetteJuly_sc <- (plants2012$rosetteJuly -mean(plants2012$rosetteJuly, na.rm=T))/sd(plants2012$rosetteJuly, na.rm=T)
plants2012$Juneleaf_s <- plants2012$X6_28_Leaf_Number_Josh - mean(plants2012$X6_28_Leaf_Number_Josh, na.rm=T)
plants2012$Julyleaf_s <- plants2012$X7_26_Leaf_Number_Josh - mean(plants2012$X7_26_Leaf_Number_Josh, na.rm=T)
plants2012$Height_s <- plants2012$Height_1st_Flower_cm - mean(plants2012$Height_1st_Flower_cm, na.rm=T)

str(plants2012)

#################vConfigure the lale2005 dataframe #######################

lale2005$Planting_Date <- strptime(lale2005$Planting_Date, format="%m/%d/%y")
lale2005$Germination_Date <- strptime(lale2005$Germination_Date, format="%m/%d/%y")
lale2005$First_FlowerDate <- strptime(lale2005$First_FlowerDate, format="%m/%d/%y")
lale2005$Mortality_Date <- strptime(lale2005$Mortality_Date, format="%m/%d/%y")
lale2005$OvuleDate <- strptime(lale2005$OvuleDate, format="%m/%d/%y")
lale2005$family <- as.factor(lale2005$family)
lale2005$DTF_s <- lale2005$DaysGermToFlwr - mean(lale2005$DaysGermToFlwr, na.rm=T)



######################### Make a new dataframe that gives FT proportions ###################
###### I don't like this because it's a yes or no, and doesn't incorporate that some #######
###### populations take much longer than others. It even ignores year.               #######

proportions <- cbind(populations, rep(NA, length(populations)), rep(NA, length(populations)), c(w,h,w,w,h,n,c,c, n,w,n,w,c,n,n,w,rep(ra,21),w,n,h,c,c), rep(NA, length(populations)))

colnames(proportions) <- c("populations", "prop", "N", "type","SE")
proportions <- as.data.frame(proportions)
proportions$prop <- as.numeric(proportions$prop)
proportions$N <- as.numeric(proportions$N)
proportions$SE <- as.numeric(proportions$SE)

for(pop in populations){
	proportions[proportions[,1]==pop,2] <- sum(plants2012$Bolt_before_7_26[plants2012$Code==pop], na.rm=T)/(length(plants2012$Germ_Date[plants2012$Code==pop]) - sum(plants2012$Dead_7_1_12[plants2012$Code==pop], na.rm=T))
	proportions[proportions[,1]==pop,3] <- length(plants2012$Germ_Date[plants2012$Code==pop])
	proportions[proportions[,1]==pop,5] <- SEP(
	proportions[proportions[,1]==pop,2], proportions[proportions[,1]==pop,3])
}

proportions <- droplevels(proportions[proportions$type!="RA" & proportions$type!="Hybrid",])
bytype <- order(proportions$prop[proportions$type],proportions$prop, decreasing=T)

#################### Only really care about some natives, crops & weeds ##################

subset2012 <- droplevels(plants2012[plants2012$subpop != "confusus" & plants2012$subpop != "cross" & plants2012$subpop != "unknown",])


##########################################################################################
###### Some linear models to look at what predicts flowering time.   	           #######
###### DTB is to bolting, DTF is to flowering. Both have problems:                 #######
###### We only let the over-winter plants go to bolt, not flower, so DTB is more   #######
###### fair across populations. However, the most correlated/best predictor appears#######
###### to be height at first flower, which we only have for the summer flowering   #######
###### individuals. I have a lot of missing data plotting DTB ~ Height, but also   #######
###### miss most of the native flowers if I use DTF ~ Height                       #######

# Model to look at each life history trait, by population

DTF2012 <- lm(DTF_s ~ Code, data=subset2012)
rosettesize <- lm(rosetteJuly_sc ~ Code, data=subset2012)
height <- lm(Height_1st_Flower_cm ~ Code, data=subset2012)


DTF2005 <- lm(DTF_s ~ population, data=lale2005)




#Just_Means <- ddply(plants2012, .(Code), summarize, mean_DTF=mean(Days_Germ_to_Bolt, na.rm=T), sd_DTF=sd(Days_Germ_to_Bolt, na.rm=T), dead_from_winter=sum(Died_from_winter, na.rm=T), seedlings=length(Code))

##### Days to bolt, code is population, subpop is weed/native/crop

### Single predictor
DTB_Rjuly <- lm(DTB_s ~ rosetteJuly_sc, data=subset2012)
summary(DTB_Rjuly)
DTB_H <- lm(DTB_s ~ Height_1st_Flower_cm, data=subset2012)
summary(DTB_H)
DTB_pop <- lm(DTB_s ~ Code, data=subset2012)
summary(DTB_pop)
DTB_type <- lm(DTB_s ~ subpop, data=subset2012)
summary(DTB_type)

## Nested Height + Code

DTB_H_pop <- lm(DTB_s ~ Height_1st_Flower_cm + Code, data=subset2012)
summary(DTB_H_pop)
DTB_H_pop2 <- lm(DTB_s ~ Height_1st_Flower_cm + Code + Height_1st_Flower_cm*Code, data=subset2012)
summary(DTB_H_pop2)

## Nested Height + subpop

DTB_H_type <- lm(DTB_s ~ Height_1st_Flower_cm + subpop, data=subset2012)
summary(DTB_H_type)
DTB_H_type2 <- lm(DTB_s ~ Height_1st_Flower_cm + subpop + Height_1st_Flower_cm*subpop, data=subset2012)
summary(DTB_H_type2)

## Nested Height + rosette
DTB_H_R <- lm(DTB_s ~ Height_1st_Flower_cm + rosetteJuly_sc, data=subset2012)
summary(DTB_H_R)
DTB_H_R2 <- lm(DTB_s ~ (Height_1st_Flower_cm + rosetteJuly_sc)^2, data=subset2012)
summary(DTB_H_R2)
DTB_H_R_C <- lm(DTB_s ~ Height_1st_Flower_cm + rosetteJuly_sc +Code, data=subset2012)
summary(DTB_H_R_C)


par(mfrow=c(2,2))
plot(DTB_H)
plot(DTB_pop)
plot(DTB_type)
plot(DTB_H_pop)
plot(DTB_H_pop2)
plot(DTB_H_type)
plot(DTB_H_type2)


##### Days to flower, code is population, subpop is weed/native/crop

xyplot(DTF_s ~ Height_1st_Flower_cm | subpop, data = subset2012, 
  xlab = " Height", ylab = "DTF", lwd=3, pch=21, fill="grey",
  type = c("r", "p"),    auto.key = list(points = TRUE, lines = TRUE, columns = 2))

### Single predictor

DTF_pop <- lm(DTF_s ~ Code, data=subset2012)
summary(DTF_pop)
DTF_type <- lm(DTF_s ~ subpop, data=subset2012)
summary(DTF_type)
DTF_H <- lm(DTF_s ~ Height_1st_Flower_cm, data=subset2012)
summary(DTF_H)

## Nested Height + Code



## Nested Height + subpop
DTF_H_type <- lm(DTF_s ~ Height_1st_Flower_cm + subpop, data=subset2012)
summary(DTF_H_type)
DTF_H_type2 <- lm(DTF_s ~ Height_1st_Flower_cm + subpop + Height_1st_Flower_cm*subpop, data=subset2012)
summary(DTF_H_type2)

require(lme4)
MM_dtf <- lmer(DTB_s ~ Height_1st_Flower_cm + rosetteJuly_sc + (1|Code), data=subset2012)

par(mfrow=c(2,2))
plot(DTF_pop)

plot(DTF_H)
plot(DTF_H_type)
plot(DTF_H_type2)

#Graph of standard error for estimates based on the 3 models

plot(jitter(coef(summary(DTF_H))[, "Std. Error"]), 
	xlab="Parameter", ylab="Standard Error", 
	main="Standard Error from 3 Models", xlim=c(0, 25), 
	ylim=c(0, 40), pch=16, cex=2)
points(jitter(coef(summary(DTF_H_type))[, "Std. Error"]), 
	col="red", pch=16, add=T, cex=1.5)
points(jitter(coef(summary(DTF_H_type2))[, "Std. Error"]), 
	col="blue", pch=16, add=T)
legend(15, 35, legend=c("DTF_H", "DTF_H_type", "DTF_H_type2"), 
	col=c("black", "red", "blue"), pch=16, cex=1.2)

ConditionNumber(DTF_type)
ConditionNumber(DTF_H)
ConditionNumber(DTF_H_type)
ConditionNumber(DTF_H_type2)

confidenceEllipse(DTF_H)
#Look for correlation of input variables
cor(as.numeric(subset2012$subtype), subset2012$Height_1st_Flower_cm, use="pair")
#covariance and correlation tables

co_DTF_H_type <- vcov(DTF_H_type)
co_DTF_H_type2 <- vcov(DTF_H_type2)
cov2cor(co_DTF_H_type)
cov2cor(co_DTF_H_type2)

with(subset2012, table(subtype))
tapply(subset2012$Height_1st_Flower_cm, INDEX=subset2012$subtype, FUN=length2)
hist(resid(DTF_H_type), main="Histogram of Residuals", xlab="Residuals")
PRsq(DTF_H_type)
Anova(DTF_H_type)

#Effect Sizes
#Population
(coef(DTF_H_type)[1]+coef(DTF_H_type)[3:7])/coef(DTF_H_type)[1]








################################################################

#  Plotting proportion to match 2013 data

par(mfrow=c(1,1), las=3)
Crop.col <- "gray0"
Native.col <- "dodgerblue4"
Weedy.col <- "firebrick"

ordered_prop <- proportions[order(proportions$type,proportions$prop, decreasing=T),]

barplot(ordered_prop$prop,
	space=c(rep(0,7), 1, rep(0,5), 1, rep(0,4)),
	names.arg=ordered_prop$populations,
	col=c(Crop.col, Native.col, Weedy.col )[ordered_prop$type],
	ylab="Proportion Bolted", 
	main="Plants that bolted within 2 months of those alive at 30 days",
	xlim=c(-.15, 20), ylim=c(-.03,1),
	cex.names=0.8
) 

legend(15, 0.85,
	legend=levels(as.factor(ordered_prop$type)),
	pch=16, col=c(Crop.col, Native.col, Weedy.col ), bty="n", cex=1)
xcords <- seq(.5, 20, by=1)

text(xcords, rep(-.02, 18), c(ordered_prop$N[1:7],"", ordered_prop$N[8:13], "", ordered_prop$N[14:18]))
text(-.5, -.02, "N=", cex=.8)
 
arrows(c(xcords[1:7],xcords[9:14],xcords[16:20]),
	as.numeric(ordered_prop$prop), 
	c(xcords[1:7],xcords[9:14],xcords[16:20]), 
	as.numeric(ordered_prop$prop)+ as.numeric(ordered_prop$SE),
	length=.05, angle=90 )

arrows(c(xcords[1:7],xcords[9:14],xcords[16:20]),
	as.numeric(ordered_prop$prop), 
	c(xcords[1:7],xcords[9:14],xcords[16:20]), 
	as.numeric(ordered_prop$prop) - as.numeric(ordered_prop$SE),
	length=.05, angle=90 )





 
##################              Native Flowering Time               #########################
####### This is an attempt to recreate the type of graph in Salhi with my 2012 data. ########
####### It shows the distribution of bolting and flowering time for my plants,       ########
####### seperated by plant type(native, weed, crop).                                 ########

noRAhybrid <- plants2012[plants2012$group!="unknown",] 
noRAhybrid <- droplevels(noRAhybrid[noRAhybrid$group!="cross",])

noRAhybrid$Bolt_int <- cut(noRAhybrid$Days_Germ_to_Bolt, breaks=seq(20,370, by=10))
noRAhybrid$Flow_int <- cut(noRAhybrid$Days_Germ_to_Flow, breaks=seq(20,370, by=10))


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


#########################    Plot all the things    #########################

##########     Plot of DTB for all types   #########
#par(mfrow=c(4,1), las=2)

## These make graphs with white writing for putting in dark powerpoints
#par(las=2, mar=c(8,4,4,2), col="white", col.axis="white", col.main="white", col.sub="white", fg="white")
par(las=2)
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

##########     Plot of DTB for just natives   #########

#par(las=2, mar=c(8,4,4,2), col="white", col.axis="white", col.main="white", col.sub="white", fg="white")
par(las=2)
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

##########     Plot of DTB for just weeds   #########

par(las=2)
barplot(weed_bolt_table,
	beside=FALSE, col=weed_shades,
	main="Weed Days to Bolting",
	xlim=c(0, length(levels(noRAhybrid$Bolt_int))+7),
	ylim=c(0,25))
	
legend(13, 10, 
	legend=levels(droplevels(weeds$Code)),
	col=weed_shades, pch=16, bty="n", ncol=3)

##########     Plot of DTB for just crops   #########

barplot(crop_bolt_table,
	beside=FALSE, col=crop_shades,
	main="Crop Days to Bolting",
	xlim=c(0, length(levels(noRAhybrid$Bolt_int))+7),
	ylim=c(0,10))
	
legend(13, 10, 
	legend=levels(droplevels(crops$Code)),
	col=crop_shades, pch=16, bty="n", ncol=3)


########### Plot DTB of all raphanistrum #############

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


######### Plots of DTF distribution for crops, natives, and weeds, all in one giant graph ###

par(mfrow=c(4,1), las=2)

barplot(table(noRAhybrid$group, noRAhybrid$Bolt_int), 
	col=shades, main="2012 Flowering Distribution", 
#	xlab="Days to Flowering", 
	ylab="Individuals")

legend(20,25, legend=levels(droplevels(noRAhybrid$group)), bty="n", pch=16, col=shades, cex=1.2)

barplot(nat_bolt_table,
	beside=FALSE, col=nat_shades,
	main="Native Days to Flowering",
	xlim=c(0, length(levels(noRAhybrid$Bolt_int))+7),
	ylim=c(0,10))
	
legend(13, 10, 
	legend=levels(droplevels(natives$Code)),
	col=nat_shades, pch=16, bty="n", ncol=3)

barplot(weed_bolt_table,
	beside=FALSE, col=weed_shades,
	main="Weed Days to Flowering",
	xlim=c(0, length(levels(noRAhybrid$Bolt_int))+7),
	ylim=c(0,25))
	
legend(13, 10, 
	legend=levels(droplevels(weeds$Code)),
	col=weed_shades, pch=16, bty="n", ncol=3)

barplot(crop_bolt_table,
	beside=FALSE, col=crop_shades,
	main="Crop Days to Flowering",
	xlim=c(0, length(levels(noRAhybrid$Bolt_int))+7),
	ylim=c(0,10))
	
legend(13, 10, 
	legend=levels(droplevels(crops$Code)),
	col=crop_shades, pch=16, bty="n", ncol=3)



