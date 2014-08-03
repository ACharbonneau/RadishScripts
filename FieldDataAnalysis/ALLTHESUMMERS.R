#Preliminary poking at summer 2013 dataset

rm( list=ls())

require(lme4)
require(plyr)
require(RColorBrewer)
source('/Volumes/Storage/RadishData/RadishScripts/Misc_scripts/Functionarium.R', chdir = TRUE)

spring2013 <- read.csv("/Volumes/Storage/RadishData/Summer2013Planting/2013plantsSpring.csv", sep=",", na.strings="")

fall2013 <- read.csv("/Volumes/Storage/RadishData/Summer2013Planting/2013plantsFall.csv", sep=",", na.strings="")

plants2012 <- read.csv("/Volumes/Storage/RadishData/Summer2012Planting/2012FieldData.csv", sep=",", na.strings="")

lale2005 <- read.csv("/Volumes/Storage/RadishData/Manuscripts/2005markerPaper/OldDatasets/LaleField2005.csv")

greenhouseAll <- read.csv("/Volumes/Storage/RadishData/Manuscripts/2005markerPaper/GreenhouseFlwrT Height.csv")

# Make dates into something R understands


spring2013$GD <- as.POSIXct( strptime(spring2013$Germ_Date, format="%m/%d/%y"))
spring2013$BD <- as.POSIXct( strptime(spring2013$Bolt_Date, format="%m/%d/%y"))
spring2013$FD <- as.POSIXct( strptime(spring2013$Flower_Date, format="%m/%d/%y"))
spring2013$blossom <- as.POSIXct( strptime(spring2013$Blossom_Collected, format="%m/%d/%y"))
spring2013$DTB <- spring2013$BD - spring2013$GD
spring2013$DTF <- spring2013$FD - spring2013$GD


fall2013$GD <- as.POSIXct( strptime(fall2013$Germ_Date, format="%m/%d/%y"))
fall2013$BD <- as.POSIXct( strptime(fall2013$Bolt_Date, format="%m/%d/%y"))
fall2013$FD <- as.POSIXct( strptime(fall2013$Flower_Date, format="%m/%d/%y"))
fall2013$blossom <- as.POSIXct( strptime(fall2013$Blossom_Collected, format="%m/%d/%y"))
fall2013$DTB <- fall2013$BD - fall2013$GD
fall2013$DTF <- fall2013$FD - fall2013$GD


plants2012$Germ_Date <- as.POSIXct( strptime(plants2012$Germ_Date, format="%m/%d/%y"))
plants2012$Died_By <- as.POSIXct( strptime(plants2012$Died_By, format="%m/%d/%y"))
plants2012$Bolted_Date <- as.POSIXct( strptime(plants2012$Bolted_Date, format="%m/%d/%y"))
plants2012$Flowering_Date <- as.POSIXct( strptime(plants2012$Flowering_Date, format="%m/%d/%y"))
plants2012$Date_Blossom_Collected <- as.POSIXct( strptime(plants2012$Date_Blossom_Collected, format="%m/%d/%y"))
plants2012$second_bolt <- as.POSIXct( strptime(plants2012$second_bolt, format="%m/%d/%y"))
plants2012 <- droplevels(plants2012[plants2012$subpop != "unknown" & plants2012$subpop != "cross" & plants2012$subpop != "confusus", ])

lale2005$Planting_Date <- as.POSIXct( strptime(lale2005$Planting_Date, format="%m/%d/%y"))
lale2005$Germination_Date <- as.POSIXct( strptime(lale2005$Germination_Date, format="%m/%d/%y"))
lale2005$First_FlowerDate <- as.POSIXct( strptime(lale2005$First_FlowerDate, format="%m/%d/%y"))
lale2005$Mortality_Date <- as.POSIXct( strptime(lale2005$Mortality_Date, format="%m/%d/%y"))
lale2005$OvuleDate <- as.POSIXct( strptime(lale2005$OvuleDate, format="%m/%d/%y"))
lale2005$family <- as.factor(lale2005$family)


subnames <- c("Dataset", "Pop", "DTB", "DTF", "Year", "FD", "GD")
lalesub <- data.frame(rep("Lale05", length(lale2005$population)), lale2005$population, rep(NA, length(lale2005$population)), lale2005$DaysGermToFlwr, rep("lale2005", length(lale2005$population)), lale2005$First_FlowerDate,lale2005$Germination_Date)
p2012sub <- data.frame(rep("field2012", length(plants2012$Code)), plants2012$Code, plants2012$Days_Germ_to_Bolt, plants2012$Days_Germ_to_Flow, rep("Sum2012", length(plants2012$Code)), plants2012$Flowering_Date, plants2012$Germ_Date)
s2013sub <- data.frame(rep("field2013", length(spring2013$Name)), spring2013$Name, spring2013$DTB, spring2013$DTF, rep("Sum2013", length(spring2013$Name)), spring2013$FD, spring2013$GD)
#f2013sub <- data.frame(fall2013$Code, fall2013$DTB, fall2013$DTF, rep("Fall2013", length(fall2013$Code)), fall2013$FD, fall2013$GD)
colnames(lalesub) <- subnames
colnames(p2012sub) <- subnames
colnames(s2013sub) <- subnames
colnames(f2013sub) <- subnames

lalesub$Pop <- factor( lalesub$Pop )
p2012sub$Pop <- factor(p2012sub$Pop )
s2013sub$Pop <- factor(s2013sub$Pop )
f2013sub$Pop <- factor(f2013sub$Pop )

ecotypes <- c(AFFR="Weedy",  AL="Weedy", AUFI="Weedy",  BINY="Weedy",  CBBG="European",  CBES="maritimus",  CGBC="Luobo",  COAU="Weedy",  DAJO="European",  DEES="UnknownType",  ESNK="European",  FGBC="Luobo",  FRSI="European",  GHIL="UnknownType",  GMIL="rostratus",  HCES="UnknownType",  HMES="UnknownType",  HZIL="UnknownType",  KAMI="Weedy",  LBBC="Black",  M3AU="Weedy",  MABG="caudatus",  MAES="MAES",  MBBC="European",  MYJO="Daikon",  N3="Weedy",  NELO="Daikon",  NTJO="Black",  PABS="European",  PBFR="landra",  PG6="Weedy",  RABG="caudatus",  RABS="European",  RACA="caudatus",  RBBC="Black",  REIL="Weedy",  SAES="maritimus",  SPNK="European",  TOBG="Daikon",  WMBG="Daikon", ZYIL="UnknownType")

allthedata <- data.frame(rbind(lalesub, p2012sub, s2013sub, f2013sub))
allthedata$Pop <- as.factor(as.character(droplevels(allthedata$Pop)))
allthedata$eco <- ecotypes[allthedata$Pop]
#allthedata$eco <- as.factor(allthedata$eco)

newlist <- list(allthedata$Pop, allthedata$Year)
allthemeans <- tapply(allthedata$DTF, newlist, FUN=mean2)
allthevars <- tapply(allthedata$DTF, newlist, FUN=var2)
allthesds <- sqrt(allthevars)
allthedata$eco <- factor(allthedata$eco)

allthedata2 <- droplevels(allthedata[allthedata$eco != "UnknownType" & allthedata$Pop != "ZYIL",] )

natives_year <- droplevels(ddply(allthedata2[allthedata2$eco=="MAES" | allthedata2$eco=="maritimus" | allthedata2$eco=="Weedy",], .(eco, Year), summarize, mean=mean2(DTF), N=length2(GD), prop=(length2(DTF[DTF < 90])/ length2(GD)) ))


#######################  Proportions  ################################


####################### By Population ################################
proportions <- droplevels(ddply(allthedata2, 
	.(Pop,eco), summarize,  
	N=length2(GD), 
	prop=(length2(DTF[DTF < 90])/ length2(GD))
 ))

proportions$SE <- SEP(proportions$prop, proportions$N)

########################### By ecotype ########################
eco_prop <- droplevels(ddply(allthedata2, 
	.(eco), summarize, 
	N=length2(GD), 
	prop=(length2(DTF[DTF < 90])/ length2(GD))
 ))

eco_prop$SE <- SEP(eco_prop$prop, eco_prop$N)


#################### Plotting #########################

ecotypes2 <- ecotypes[ecotypes != "UnknownType"]


##Crops
Black <- "black"
caudatus <- "gray30"
Daikon <- "gray60"
European <- "gray80"
Luobo <- "gray100"

##Natives
landra <- "royalblue"
maritimus <- "royalblue3"
MAES <- "dodgerblue"
rostratus <- "deepskyblue3"

Weedy <- "firebrick"


palette <- c( Black, caudatus, Daikon, European, landra, Luobo, MAES, maritimus, rostratus, Weedy)



############# DTF Distribution plots #############

################## By Ecotype ##########################

allthedata2$eco <- factor(allthedata2$eco, c("Weedy", "caudatus", "rostratus", "Luobo", "European", "maritimus", "Black", "Daikon", "MAES", "landra") )

par( las=1)
plot(allthedata2$DTF ~ allthedata2$eco,
	xlab="Ecotype or Species", ylab="Days to Flowering",
	main="Distribution of radish flowering times by ecotype", 
	col=palette[c(10,2,9,6,4,8,1,3,7,5)])


pop_ns <- rowSums(table(allthedata2$eco, allthedata2$DTF))	
xcords <- seq(1, 10, by=1)
mtext(text=pop_ns, at=xcords, cex=.8, side=1, line=2)
mtext(text="N=", at=0.5, cex=1, side=1, line=2)


legend(1.5, 342,legend=c("Black", expression(italic("caudatus")), "Daikon", "European"),pch=16, col=c(Black, caudatus, Daikon, European), bty="n")	
legend(1.5, 285, legend="Luobo", pch=21, col=Black, bg=Luobo, bty="n")
legend(1.5, 270.5, legend=c(expression(italic("landra")), expression(italic("maritimus")), "MAES", expression(italic("rostratus")), "Weeds"), col=c(landra, maritimus, MAES, rostratus, Weedy), bty="n", pch=16)



##################################################################

######## Distribution of %flower by Ecotype #########
par(mfrow= c(1,1), las=3)
bytype <- order(proportions$prop, proportions$prop[proportions$eco], decreasing=T)

barplot( as.numeric(proportions$prop)[bytype], 
	names.arg= proportions$populations[bytype], 
	col=palette[proportions$eco][bytype], 
	ylim=c(-.03,1), xlim=c(-.15, 42), 
	ylab="Proportion Flowered", main="Proportion flowering within 90 days, all years, by population") 
	
legend(28, .9,legend=c("Black", expression(italic("caudatus")), "Daikon", "European"),pch=16, col=c(Black, caudatus, Daikon, European), bty="n")	
legend(28, .745, legend="Luobo", pch=21, col=Black, bg=Luobo, bty="n")
legend(28, .705, legend=c(expression(italic("landra")), expression(italic("maritimus")), "MAES", expression(italic("rostratus")), "Weeds"), col=c(landra, maritimus, MAES, rostratus, Weedy), bty="n", pch=16)

xcords <- seq(.7, 42, by=1.2)


	#This puts the N across the bottom of the graph, just above labels
text(xcords, rep(-.02, 27), proportions$N[bytype], cex=.6, )
text(-.5, -.02, "N=", cex=.7)

arrows(xcords,as.numeric(proportions$prop)[bytype], xcords, as.numeric(proportions$prop)[bytype] + as.numeric(proportions$SE)[bytype],length=.05, angle=90 )

arrows(xcords,as.numeric(proportions$prop)[bytype], xcords, as.numeric(proportions$prop)[bytype] - as.numeric(proportions$SE)[bytype],length=.05, angle=90 )



#######################################################

######## Distribution of %flower by Grouped Ecotype #########

byeco <- order(eco_prop$prop, decreasing=T)

barplot( as.numeric(eco_prop$prop)[byeco], 
	names.arg= eco_prop$populations[byeco], 
	col=palette[eco_prop$eco][byeco], 
	ylim=c(-.03,1), xlim=c(-.15, 13), 
	ylab="Proportion Flowered", main="Proportion flowering within 90 days, all years, by ecotype") 
	
legend(5, .9,legend=c("Black", expression(italic("caudatus")), "Daikon", "European"),pch=16, col=c(Black, caudatus, Daikon, European), bty="n")	
legend(5, .745, legend="Luobo", pch=21, col=Black, bg=Luobo, bty="n")
legend(5, .705, legend=c(expression(italic("landra")), expression(italic("maritimus")), "MAES", expression(italic("rostratus")), "Weeds"), col=c(landra, maritimus, MAES, rostratus, Weedy), bty="n", pch=16)

xcords2 <- seq(.7, 12, by=1.2)


	#This puts the N across the bottom of the graph, just above labels
text(xcords2, rep(-.02, 27), eco_prop$N[byeco], cex=.8, )
text(-.5, -.02, "N=", cex=.7)

arrows(xcords2,as.numeric(eco_prop$prop)[byeco], xcords2, as.numeric(eco_prop$prop)[byeco] + as.numeric(eco_prop$SE)[byeco],length=.05, angle=90 )

arrows(xcords2,as.numeric(eco_prop$prop)[byeco], xcords2, as.numeric(eco_prop$prop)[byeco] - as.numeric(eco_prop$SE)[byeco],length=.05, angle=90 )





#######################################################
#################### DTF_native_year ##################
par(las=2, mar=c(7,4,4,2))
plot(droplevels(allthedata2$Year[allthedata2$eco=="MAES" | 	
		allthedata2$eco=="maritimus" | allthedata2$eco=="Weedy"]):
	droplevels(allthedata2$eco[allthedata2$eco=="MAES" | 
		allthedata2$eco=="maritimus" | allthedata2$eco=="Weedy"]),
	allthedata2$DTF[allthedata2$eco=="MAES" | allthedata2$eco=="maritimus" | allthedata2$eco=="Weedy"],
	col= c(Weedy, maritimus, MAES), xaxt='n',
	main="Variation in DTF by year")
	
axis(side=1, at=1:12, labels=c( "Weedy S05","maritimus S05", "MAES S05", "Weeds S12", "maritimus S12", "MAES S12", "Weeds S13", "maritimus S13", "MAES S13", "Weeds F13", "maritimus F13", "MAES F13"))

#######################################################
	
par(las=3, mar=c(8,5,5,4))
barplot(natives_year$prop, col=c(MAES, maritimus, Weedy)[natives_year$eco],names.arg=c( "MAES S05",   "MAES S12",   "MAES S13",  	"MAES F13", "maritimus S05", "maritimus S12", "maritimus S13", "maritimus F13","Weedy S05", "Weeds S12", "Weeds S13", "Weeds F13" ), main="Proportion flowering in 90 days, by year" )
#axis( side=1, at=1:12, labels=natives_year$eco)



####################




m1 <- with(allthedata, lm(DTF ~ Pop + Year))




# ecotypes <- c(ADOL="Crop",  AFFR="Weedy",  AL="Weedy",  AROL="Crop",  AUFI="Weedy",  BINY="Weedy",  CBBG="Crop",  CBES="LanMar",  CGBC="Crop",  COAU="Weedy",  COOL="Crop",  DAJO="Crop",  DEES="UnknownType",  ESNK="Crop",  FGBC="Crop",  FRSI="Crop",  GHIL="UnknownType",  GMIL="Rostratus",  HCES="UnknownType",  HMES="UnknownType",  HZIL="UnknownType",  KAMI="Weedy",  LBBC="Crop",  M3="Weedy",  M3AU="Weedy",  MABG="Crop",  MAES="MAES",  MBBC="Crop",  MYJO="Crop",  N3="Weedy",  NCDE="Weedy",  NELO="Crop",  NTJO="Crop",  NZIL="Confusus",  PABS="Crop",  PBFR="LanMar",  PG6="Weedy",  RA173="UnknownType",  RA215="UnknownType",  RA219="UnknownType",  RA226="UnknownType",  RA264="UnknownType",  RA265="UnknownType",  RA399="UnknownType",  RA402="UnknownType",  RA432="UnknownType",  RA444="UnknownType",  RA447="UnknownType",  RA475="UnknownType",  RA494="UnknownType",  RA503="UnknownType",  RA761="UnknownType",  RA787="UnknownType",  RA835="UnknownType",  RA837="UnknownType",  RA838="UnknownType",  RA840="UnknownType",  RABG="Crop",  RABS="Crop",  RACA="Crop",  RBBC="Crop",  REIL="Weedy",  SAES="LanMar",  SPEU="Crop",  SPNK="Crop",  TOBG="Crop",  TYIL="Weedy",  WEAU="Weedy",  WMBG="Crop", ZYIL="UnknownType")
