#This takes a .csv file of ln probabilities from STRUCTURE made by doing a grep of all the 3 lines that give probability data in the STRUCTURE outfile: 
#   grep -A 2 "Estimated Ln Prob of Data" 
# find (\d+)_\w+-(\d+)_f\n.+= (-*\d+\.\d+)\n.+= (-*\d+\.\d+)\n.+= (-*\d+\.\d+)
# replace \1,\2,\3,\4,\5
#Then combining the greps from many runs at different Ks and making them into the format:
#   RunNumber Knumber Estimated_Ln_Prob_of_Data Mean_value_of_ln_likelihood Variance_of_ln_likelihood
#Plots the log likelihood for each K by run, then takes the best K from each run and makes a final density plot of the best Ks, excluding the runs that are missing a lot of data.
#This last plot should look roughly the same as the Mean LnP plot from the evanno method.

source('/Volumes/Storage/RadishData/RadishScripts/Misc_scripts/AmandaSource.R', chdir = TRUE)

#Prob_data <- read.csv("/Volumes/Storage/RadishData/2005MarkerData/STRUCTURE/RedoneStructure/EstimateK/Corrfreq/NoRACoNo/Probabilities/NoRACoNocombinedProbs.csv", header=T, sep="\t")

#Prob_data <- read.csv("/Volumes/Storage/RadishData/2005MarkerData/STRUCTURE/RedoneStructure/EstimateK/Corrfreq/NoRACoNoPopJackknife/Probabilities/combo.csv", header=F, sep=",")

Prob_data <- read.csv("/Volumes/Storage/RadishData/2005MarkerData/STRUCTURE/RedoneStructure/EstimateK/Corrfreq/NewPops2013/AlltheProbabilities.txt", header=F,sep=",")

colnames(Prob_data) <- c("RunNumber", "KNumber", "Estimated_Ln_Prob_of_Data", "Mean_value_of_ln_likelihood", "Variance_of_ln_likelihood")



probs <- list()
for(i in levels(as.factor(Prob_data$RunNumber))){
	probs[[i]] <- cbind(Prob_data$KNumber[Prob_data$RunNumber==i], 	Prob_data$Estimated_Ln_Prob_of_Data[Prob_data$RunNumber==i],
	ApproxBayesFac(Prob_data$Estimated_Ln_Prob_of_Data[Prob_data$RunNumber==i])
)
}

pdf(file="/Volumes/Storage/RadishData/2005MarkerData/STRUCTURE/RedoneStructure/EstimateK/Corrfreq/NewPops2013/newPops.pdf")

for(i in levels(as.factor(Prob_data$RunNumber))){
	plot(probs[[i]][ order(probs[[i]][,1]) ,3]#probs[[i]][,3]
	,xlab="Proposed K", ylab="Approximate Bayes Factor", 
	main=paste("Run", i, sep=" "))

text(x=seq(1,max(Prob_data$KNumber)), 
	y=probs[[i]][ order(probs[[i]][,1]),3]#probs[[i]][,3]
	, pos=1, cex=.7)
}

BestK = rep(NA, length(levels(as.factor(Prob_data$RunNumber))))

for(a in seq(1,length(levels(as.factor(Prob_data$RunNumber))))){
	BestK[a] <- probs[[a]][,1] [probs[[a]][,3]== max(probs[[a]][,3])]
}

BestK = BestK[seq(1,length(levels(as.factor(Prob_data$RunNumber))))]
plot(density(BestK, bw=.4), main= "Density Plot of Best Ks")


dev.off()

