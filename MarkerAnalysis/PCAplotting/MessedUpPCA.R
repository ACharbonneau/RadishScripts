require(RColorBrewer)
col_pal1 = brewer.pal(12, "Set3")
col_pal2 = brewer.pal(8, "Dark2")
col_pal3 = brewer.pal(12, "Paired")
col_pal = c(col_pal1, col_pal2, col_pal3)

################################## PCA data as formatted for SmartPCA (fake biallelic)########################

s.pca <- read.csv("/Volumes/Storage/RadishData/2005MarkerData/SmartPCAformat.csv", header=T, na.strings="-9")


# Generate data subset with no missing values
dat_intermediate <- na.omit(s.pca[,3:136])
# However, this generates columns with no variation.
apply(dat_intermediate, 2, range)

# Get rid of columns that have no variation
crapFun <- function(x){
	min(x) - max(x)
}

# This generates a matrix that does not contrain columns with no variation
dat_intermediate2 <- dat_intermediate[,apply(dat_intermediate, 2, crapFun)!=0]

# all fixed
apply(dat_intermediate2, 2, range)


s.newpc <- prcomp(dat_intermediate2, scale.=F)
plot(s.newpc)
s.newpc$x
s.newRot <- s.newpc$rotation

plot( s.newpc$x[,2], s.newpc$x[,1], col=col_pal[s.pca$Pop], pch=16)

s.NoRa <- droplevels(s.pca[grep("RA\\d\\d\\d", s.pca$Pop, invert=TRUE),])

sno.newpca <- prcomp(na.omit(s.NoRa[,3:136]))

plot( sno.newpca$x[,2], sno.newpca$x[,1], col=col_pal[s.NoRa$Pop], pch=16)



################################## PCA data with cat(alleles) as numbers########################

pca.data <- read.csv("/Volumes/Storage/RadishData/2005MarkerData/STRUCTURE/SSRTest.csv", header=T, na.strings="nana")
head(pca.data)

str(pca.data)

newpc <- prcomp(na.omit(pca.data[,7:27]))
plot(newpc)

newpc$x
newRot <- newpc$rotation

plot(newpc$sdev)

percentage.variance <- ((newpc$sdev)^2 / sum( (newpc$sdev)^2 ))*100
plot(percentage.variance)

plot( -newpc$x[,1], newpc$x[,2] )

plot(newpc$x[1,])

summary( newpc )$importance

plot( summary( newpc )$importance[2,], xlab="PCA", ylab="% Variance", main="Explained Variance")

NoRa <- droplevels(pca.data[grep("RA\\d\\d\\d", pca.data$Pop, invert=TRUE),])

NoRapc <- prcomp(na.omit(NoRa[,7:27]))
plot( NoRapc$x[,2], -NoRapc$x[,1], col=col_pal[NoRa$Pop], pch=16, xlim=c(-1000, 2000) )
legend(1100, 100, legend=levels(NoRa$Pop), pch=16, col=col_pal, ncol=2 )


################################## PCA data with Not cat(alleles) ########################

f.pca.data <- read.csv("/Volumes/Storage/RadishData/2005MarkerData/STRUCTURE/NoRApheno.txt", skip=1, na.strings="-9", sep=" ", header=F)

str.labels <- read.csv("/Volumes/Storage/RadishData/2005MarkerData/MarkerPopEditOrder.csv", header=F, col.names=c("Individual", "Type", "Pop", "Order", "Name", "Species", "Color", "Vernalization", "DTF", "Bins"))
str.labels <- str.labels[str.labels$Type!="UnknownType",]

f.pca.data <- cbind(str.labels,f.pca.data[,8:49])

colnames(f.pca.data) <- c("Individual", "Type", "Pop", "Order", "Name", "Species", "Color", "Vernalization", "DTF", "Bins", "DWRD_124","2DWRD_124", "DWRD_112", "2DWRD_112", "DWRD_61", "2DWRD_61", "DWRD_177", "2DWRD_177", "DWRD_107", "2DWRD_107", "DWRD_123", "2DWRD_123", "DWRD_121", "2DWRD_121", "DWRD_158", "2DWRD_158", "DWRD_48", "2DWRD_48", "DWRD_180", "2DWRD_180", "DWRD_97", "2DWRD_97", "DWRD_205", "2DWRD_205", "DWRD_27", "2DWRD_27", "Bn26a", "2Bn26a", "BRMS005", "2BRMS005", "Na10H06",  "2Na10H06", "Ra1H08", "2Ra1H08", "Ra2E11", "2Ra2E11", "Na14E08", "2Na14E08", "Bn35d", "2Bn35d", "Na12E05", "Na12E05")

f.newpc <- prcomp(na.omit(f.pca.data[,8:49]))

plot( f.newpc$x[,1], f.newpc$x[,2], col=col_pal[f.pca.data$Pop], pch=16 )



# Generate data subset with no missing values
dat_intermediatetaco <- na.omit(f.pca.data)
# However, this generates columns with no variation.
apply(dat_intermediatetaco, 2, range)

# Get rid of columns that have no variation
crapFun <- function(x){
	min(x) - max(x)
}

# This generates a matrix that does not contrain columns with no variation
dat_intermediatetaco2 <- dat_intermediatetaco[,apply(dat_intermediatetaco, 2, crapFun)!=0]

# all fixed
apply(dat_intermediatetaco2, 2, range)

dat_intermediatetaco2 <- droplevels(dat_intermediatetaco2)

dat_intermediatetaco2$Pop <- factor(dat_intermediatetaco2$Pop)

f.newpc2 <- prcomp(dat_intermediatetaco2[,8:length(dat_intermediatetaco2)], scale.=T)

plot(f.newpc2)

plot( f.newpc2$x[,1], f.newpc2$x[,2], col=col_pal[f.pca.data$Pop], pch=16 )
legend(-5, 9, legend=levels(dat_intermediatetaco2$Pop), pch=16, col=col_pal, ncol=2 )



summary(f.newpc2)



