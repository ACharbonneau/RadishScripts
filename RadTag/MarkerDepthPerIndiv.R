#ChloroplastMarkers <- read.csv("RchlorSat/BAMchloro/ChloroMarkerIndDepth.txt", sep="\t", header=F)
#SativusMarkers <- read.csv("RsatRef/BAMsatG/SatMarkerIndDepth.txt", sep="\t", header=F)
#RrrMarkers <- read.csv("RrrRef/BAMrrr/RrrMarkerIndDepth.txt", sep="\t", header=F)

RrrMarkers <- read.csv("~/Downloads/RrrMarkerIndDepth.txt", sep="\t", header=F)

RrrMarkers <- cbind(1:length(RrrMarkers$V1), RrrMarkers)
Individuals <- c("Numeric", "Chromo", "Position", "12692", "12693", "12694", "12695", "12696", "12697", "12698", "12699", "12701", "12702", "12703", "12704", "12705", "12706", "12707", "12708", "12709", "12710", "12711", "12712", "12713", "12714", "12715", "12716", "12717", "12718", "12719", "12720", "12721", "12722", "12723", "12724", "12725", "12726", "12727", "12728", "12729", "12730", "12731", "12732", "12733", "12734", "12735", "12736", "12737", "12738", "12739", "12740", "12741", "12742", "12743", "12744", "12745", "12746", "12747", "12748", "12749", "12750", "12751", "12752", "12753", "12802", "12803", "12804", "12805", "12806", "12807", "12808", "12809", "12810", "12811", "12812", "12813", "12814", "12815", "12816", "12817", "12818", "12819", "12820", "12821", "12822", "12823", "12824", "12825", "12826", "12827", "12828", "12829", "12830", "12831", "12832", "12833", "12834", "12835", "Blank")

colnames(ChloroplastMarkers) <- Individuals
colnames(SativusMarkers) <- Individuals
colnames(RrrMarkers) <- Individuals


levels(RrrMarkers$Chromo)

pdf( "MarkersByIndividual.pdf", height = 4, width = 8 )

###Chloroplast
plot(ChloroplastMarkers$Position, rowSums(ChloroplastMarkers[3:length(ChloroplastMarkers)] > 5), type='n', 
     xlab="Marker Position", ylab="Individuals with >5 reads",
     main="Sativus Chloroplast")

for ( i in 1:length( ChloroplastMarkers$Position )){
lines( c(ChloroplastMarkers$Position[i], ChloroplastMarkers$Position[i] ), 
       c( 0, (rowSums(ChloroplastMarkers[3:length(ChloroplastMarkers)] > 5))[i] ) )
}

###Sativus
plot(SativusMarkers$Position, rowSums(SativusMarkers[3:length(SativusMarkers)] > 5), type='n', 
     xlab="Marker Position", ylab="Individuals with >5 reads",
     main="Sativus Genome")

for ( i in 1:length( SativusMarkers$Position )){
  lines( c(SativusMarkers$Position[i], SativusMarkers$Position[i] ), 
         c( 0, (rowSums(SativusMarkers[3:length(SativusMarkers)] > 5))[i] ) )
}

###Rrr
plot(RrrMarkers$Numeric[1:1000], rowSums(RrrMarkers[3:length(RrrMarkers)] > 5)[1:1000], type='n', 
     xlab="Marker Position", ylab="Individuals with >5 reads",
     main="Rrr Genome")

for ( i in 1:length( RrrMarkers$Numeric[1:1000] )){
  lines( c(RrrMarkers$Numeric[i], RrrMarkers$Numeric[i] ), 
         c( 0, (rowSums(RrrMarkers[3:length(RrrMarkers)] > 5))[i] ) )
}

dev.off()

###lines( c(start_x, end_x), c(start_y, end_y) )

taco <- RrrMarkers
str(taco)

tacocount <- rep( NA, dim(taco)[[1]] )
for (i in 1:dim(taco)[[1]] ) {
  thistaco <- taco[ i, 4:dim(taco)[[2]] ]
  tacocount[i] <- length( thistaco[ thistaco > 5 ]  )
}

hist(tacocount, main="Read depth by SNP", xlab="Individuals with read depth > 5", ylab="SNPs with depth/individual > 5")

FilteredRrr <- droplevels(RrrMarkers[tacocount > 60, ])

nacho <- unique(FilteredRrr[4:dim(taco)[2]], MARGIN=1)


