library(ade4)
library(MASS)

testPCA <- read.csv("/Users/Amanda/Desktop/testMissingPCA.csv", header = T)

testPCA <- testPCA[ testPCA$X != "confusus" & testPCA$X != "rost",]

testPCA$color <- palette()[as.numeric(testPCA[,1])]


taco <- lm( as.matrix(prcomp( as.matrix( testPCA[, 4:24]) )$x[,1:16]) ~ testPCA$X )

anova( taco)

bob <- data.frame(as.matrix(prcomp( as.matrix( testPCA[, 4:24]) )$x[,1:16]))

bob$color <- testPCA$color
bob$species <- testPCA$X

plot(bob$PC1, bob$PC2, col=bob$color, pch=16)


plot(PopPCO$l1$RS1, (PopPCO$l1$RS2), col=c(1:8)[testPCA[,1]], pch=16) 
