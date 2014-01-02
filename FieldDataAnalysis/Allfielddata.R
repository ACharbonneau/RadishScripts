#Combined 2012 and 2013 data
source('/Volumes/Storage/RadishData/RadishScripts/Misc_scripts/Functionarium.R', chdir = TRUE)
require(RColorBrewer)
plants2013 <- read.csv("/Users/Amanda/Dropbox/RadishWeedEvolution/Summer2013/2013plantData.csv", sep=",", na.strings="")

plants2012 <- read.csv("/Volumes/Storage/RadishData/Summer2012Planting/2012FieldData.csv", sep=",", na.strings="")

populations13 <- levels(plants2013$Name)
populations12 <- levels(plants2012$Code)
alldatasets <- list(populations12,populations13)

w <- "Weed"
c <- "Crop"
n <- "Native"
h <- "Hybrid"
ra <- "RA"

proportions <- cbind(
	populations12, 
	rep(NA, length(populations12)),
	rep(NA, length(populations12)),
	rep(NA, length(populations12)), 
	rep(NA, length(populations12)),	
	rep(NA, length(populations12)),
	rep(NA, length(populations12)),
	rep(NA, length(populations12)), 
	rep(NA, length(populations12)),
	rep(NA, length(populations12)),
	c(w,h,w,w,h,n,c,c, n,w,n,w,c,n,n,w,rep(ra,21),w,n,h,c,c)
)

colnames(proportions) <- c("populations", "Avg_prop", "prop12", "N12", "SE12", "prop13", "N13", "SE13", "flow12", "flow13", "type")
proportions <- as.data.frame(proportions)
proportions$Avg_prop <- as.numeric(proportions$Avg_prop)
proportions$prop12 <- as.numeric(proportions$prop12)
proportions$N12 <- as.numeric(proportions$N12)
proportions$SE12 <- as.numeric(proportions$SE12)
proportions$prop13 <- as.numeric(proportions$prop13)
proportions$N13 <- as.numeric(proportions$N13)
proportions$SE13 <- as.numeric(proportions$SE13)
proportions$flow12 <- as.numeric(proportions$flow12)
proportions$flow13 <- as.numeric(proportions$flow13)


for(pop in proportions[,1]){
		proportions[proportions[,1]==pop,3] <- 
			sum(plants2012$Flower_before_Winter[plants2012$Code==pop], na.rm=T)/
			length(plants2012$Germ_Date[plants2012$Code==pop])
	proportions[proportions[,1]==pop,4] <- length(plants2012$Germ_Date[plants2012$Code==pop])
	proportions[proportions[,1]==pop,5] <- SEP(
		proportions[proportions[,1]==pop,3], 
		proportions[proportions[,1]==pop,4])

	proportions[proportions[,1]==pop,6] <-CalcProp(
		plants2013$Flower..Date[plants2013$Name==pop],
		plants2013$Germ.Date[plants2013$Name==pop])
	proportions[proportions[,1]==pop,7] <-
		length(na.omit(plants2013$Germ.Date[plants2013$Name==pop]))
	proportions[proportions[,1]==pop,8] <- SEP(
		proportions[proportions[,1]==pop,6], 
		proportions[proportions[,1]==pop,7])
		
		temptotal <- ave(c(proportions[proportions[,1]==pop,3], proportions[proportions[,1]==pop,6]))
		proportions[proportions[,1]==pop,2] <- temptotal[1]
		
		proportions[proportions[,1]==pop,9] <- 
			sum(plants2012$Flower_before_Winter[plants2012$Code==pop], na.rm=T)
			proportions[proportions[,1]==pop,10] <- length(na.omit(plants2013$Flower..Date[plants2013$Name==pop]))
		

}


pro_subset <- proportions[proportions$Avg_prop!="NaN",]

s12 <- pro_subset[,c(1,3,4,5,9)]
s12$year <- rep("12", length(s12[,1]))
s12$season <- rep("spring",length(s12[,1]))
s13 <- pro_subset[,c(1,6,7,8,9)]
s13$year <- rep("13", length(s12[,1]))
s13$season <- rep("spring",length(s13[,1]))
colnames(s12) <- c("pops","prop","N","SE","type","year","season" )
colnames(s13) <- c("pops","prop","N","SE","type","year","season" )
the_right_way <- rbind(s12,s13)

trial_subset <- cbind(proportions[,c(1,2,9,4,10,7,11)])

trial_subset <- trial_subset[trial_subset$Avg_prop!="NaN",]




shitty_model <- glm(prop ~ pops, data=the_right_way, family=quasibinomial)

glm(prop ~ pops + type + year, data=the_right_way, family=quasibinomial)
model_2 <- glm(prop ~ pops + year, data=the_right_way, family=binomial )

