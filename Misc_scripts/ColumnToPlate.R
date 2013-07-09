setwd("~/Downloads/")
DataColumn <- read.csv("~/Downloads/tempColumn.csv")

Grid <- data.frame(matrix(DataColumn[,1], ncol=12, byrow=TRUE))

write.csv(Grid, file ="TempPlate.csv")
