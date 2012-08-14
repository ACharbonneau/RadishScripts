setwd("~/Documents/RadishData/MarkerData/SNPtestSet/")

PCA.dat.1 <- read.table("SNPmarker.pca", skip=11)

str(PCA.dat.1)

plot(PCA.dat.1[,3:4])

labels.dat.1 <- read.table("SNPmarker.ind", col.names= c("Individual", "Sex", "Pop"))

new.data <- data.frame(PCA.dat.1, labels.dat.1)
str(new.data)

plot(new.data[,1:2], col=c(1:7)[new.data$Pop])

plot( new.data$V1, new.data$V2, type='n' )
polygon( c(0.2,-0.2,-0.2,0.2), c(-0.1,-0.1,0.1,0.1), col="goldenrod" )
points(new.data[,1:2], col=c(1:7)[new.data$Pop])

plot( new.data$V1, new.data$V2, col="red" )

plot( new.data$V1, new.data$V2, pch=16, col=c(1:7)[new.data$Pop], xlim=c(-0.5, 0.5), ylim=c(-0.5, 0.5) )

legend( 0.35, -0.2, levels(new.data$Pop), pch=16, col=c(1:7) )

plot( new.data$V8, new.data$V9, pch=16, col=c(1:7)[new.data$Pop], xlim=c(-0.3, 0.5), ylim=c(-0.3, 0.3), xlab="PC8", ylab="PC9" )

legend( 0.35, -0.1, levels(new.data$Pop), pch=16, col=c(1:7) )



pop.model <- lm( as.matrix( new.data[,1:10] ) ~ new.data$Pop )
summary( manova( pop.model ), test='Wilks' )


pop.model.1 <- lm( new.data$V1 ~ new.data$Pop )
summary( pop.model.1 )

pop.model.2 <- lm( new.data$V2 ~ new.data$Pop )
summary( pop.model.2 )




