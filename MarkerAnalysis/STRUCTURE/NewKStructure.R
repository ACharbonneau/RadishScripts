#This is the redone STRUCTURE runs plotted to show their probabilities. It's essentially the same as Kstructure.R, but designed to work on a .csv with many runs rather than copy/paste data

source('~/Documents/RadishData/RadishScripts/Misc_scripts/AmandaSource.R', chdir = TRUE)

ln.prob <- read.csv("~/Documents/RadishData/2005MarkerData/STRUCTURE/RedoneStructure/EstimateK/RedoneLnProb.csv", header=TRUE)
row.names(ln.prob) <- ln.prob[,1]
ln.prob <- ln.prob[2:49]


bayes.prob <- apply(ln.prob, MARGIN=1, FUN=ApproxBayesFac)

par(mfrow=c(4,1))

plot(bayes.prob$CoLoc1, pch=16, ylab="Probability", xlab="", main="Pop gene freqs are correlated, species as prior")
text(bayes.prob$CoLoc1, pos=1)
plot(bayes.prob$CoNo1, pch=16, xlab="", ylab="Probability", main="Population gene frequencies are correlated, flat prior")
text(bayes.prob$CoNo1, pos=1)
plot(bayes.prob$CoLearnLoc1, pch=16, xlab="", ylab="Probability", main="Population gene frequencies are correlated, Species as learning set")
text(bayes.prob$CoLearnLoc1, pos=1)
plot(bayes.prob$CoLearnPh, pch=16, xlab="", ylab="Probability", main="Population gene frequencies are correlated, Phenotype as learning set")
text(bayes.prob$CoLearnPh, pos=1)



plot(bayes.prob$UnCoLo1, pch=16, ylab="Probability", xlab="", main="Population gene frequencies are uncorrelated, species as prior")
text(bayes.prob$UnCoLo1, pos=1)
plot(bayes.prob$UnCoNo1, pch=16, ylab="Probability",xlab="# of groups", main="Population gene frequencies are uncorrelated, flat prior")
text(bayes.prob$UnCoNo1, pos=1)
plot(bayes.prob$UnCoLearnLoc1, pch=16, xlab="", ylab="Probability", main="Population gene frequencies are uncorrelated, Species as learning set")
text(bayes.prob$UnCoLearnLoc1, pos=1)
plot(bayes.prob$UnCoLearnPh, pch=16, xlab="", ylab="Probability", main="Population gene frequencies are uncorrelated, Phenotype as learning set")
text(bayes.prob$UnCoLearnPh, pos=1)

par(mfrow=c(3,1))
plot(bayes.prob$NoRAUnCoNo1, pch=16, xlab="", ylab="Probability", main="Population gene frequencies are uncorrelated, rep 1")
text(bayes.prob$NoRAUnCoNo1, pos=1)
plot(bayes.prob$NoRAUnCoNo2, pch=16, xlab="", ylab="Probability", main="Population gene frequencies are uncorrelated, rep 2")
text(bayes.prob$NoRAUnCoNo2, pos=1)
plot(bayes.prob$NoRAUnCoNo3, pch=16, xlab="", ylab="Probability", main="Population gene frequencies are uncorrelated, rep 3")
text(bayes.prob$NoRAUnCoNo3, pos=1)

par(mfrow=c(3,1))
plot(bayes.prob$NoRACoNo1, pch=16, xlab="", ylab="Probability", main="Population gene frequencies are uncorrelated, rep 1")
text(bayes.prob$NoRACoNo1, pos=1)
plot(bayes.prob$NoRACoNo2, pch=16, xlab="", ylab="Probability", main="Population gene frequencies are uncorrelated, rep 2")
text(bayes.prob$NoRACoNo2, pos=1)
plot(bayes.prob$NoRACoNo3, pch=16, xlab="", ylab="Probability", main="Population gene frequencies are uncorrelated, rep 3")
text(bayes.prob$NoRACoNo3, pos=1)
