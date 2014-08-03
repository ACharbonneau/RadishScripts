rm( list=ls())

require(plyr)  #This is a fancy-ass apply function that can take multiple column arguments to do things

wingshit <- read.csv("~/Desktop/fakeflies.csv")

#you tell ddply which dataframe to use, then which variables to summerize by. I'm summarizing by population and individual number. So it will combine the data from whatever rows contain the same values for population and individual. that's the .(ind, pop) part. Then I'm telling it to summarise the data by doing all the things after "summarise" and I basically just list out that it should take the mean for each column. 

average_wings <- ddply(wingshit, .(ind, pop), summarise, mean(LM1), mean(LM2), mean(LM3), mean(LM4), mean(LM5), mean(LM6), mean(LM7), mean(LM8), mean(LM9), mean(LM10))

## To check that its behaving, you can spot check averages by asking for both rows from some individual:
wingshit[wingshit$ind==7 & wingshit$pop=="egfr",]
## then manually average a couple values and compare them to the average_wings answer. It works fine on my numbers