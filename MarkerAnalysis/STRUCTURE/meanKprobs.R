#This takes the mean probability from each run and takes the mean for each K 

K <- 20

all4probs <- rep(NA, K)

for(i in 1:K){

#all4probs[i] <- paste("/Volumes/Storage/RadishData/2005MarkerData/STRUCTURE/RedoneStructure/EstimateK/Corrfreq/NoRACoNoPopJackknife/JustWeeds/Meanprob-", i, ".txt", sep="")

all4probs[i] <- paste("/Volumes/Storage/RadishData/2005MarkerData/STRUCTURE/RedoneStructure/EstimateK/Corrfreq/NoRACoNoPopJackknife/RaphinistrumNatives/Meanprob-", i, ".txt", sep="")

	
#	all4probs[i] <- paste("/Volumes/Storage/RadishData/2005MarkerData/STRUCTURE/RedoneStructure/EstimateK/Corrfreq/NoRACoNo/Probabilities/prob-", i, ".txt", sep="")
	
}

allprobs <- rep(NA, K)

for(i in 1:K){
	allprobs[i] <- read.csv(all4probs[i], header=F)	
	
}

meanprobs <- rep(NA, K)

for(i in 1:K){
	meanprobs[i] <- mean(allprobs[[i]])
	}
	
meanprobs

plot(meanprobs, xlab= "K value", ylab= "-ln likelihood", main="Mean of likelihood for each K")
legend(20, -10500, legend="N= 16 or 20", bty="n")