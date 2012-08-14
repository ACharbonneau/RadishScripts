setwd("~/Documents/RadishData/MarkerData/STRUCTURE/Knumber/500000-1M/")
#Needs comma seperated file with each row: HH,MM,SS
#and no header row
#predicts runtime of large dataset given runtimes of several subsets. Assumes linearity.
HPCtime <- read.csv("HPCCtimeHMS.csv", header=FALSE, colClasses=c(rep("numeric")))

run.num <- 1:nrow(HPCtime)
big.run <- list(run.num=c(48,100,200)) #change to size of potential bigger runs

hours <- HPCtime$V1 * 3600
minutes <- HPCtime$V2 * 60
seconds <- HPCtime$V3

totaltime = (hours+minutes+seconds)/3600

plot(totaltime~run.num, xlab="RunNumber", ylab="time in hours")
abline(lm(totaltime~run.num))
predtime <-lm(totaltime~run.num)
predict.lm(predtime, newdata=big.run)


