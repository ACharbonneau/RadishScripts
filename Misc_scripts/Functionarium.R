## This is the functionarium. The functions live here.

#Calculates a proportion of two things (percentage)
CalcProp <- function(numerator, denominator){
	return(length(na.omit(numerator))/length(na.omit(denominator)))
}

#Tell me the standard error!
CalcSE <- function(x){
	x <- x[!is.na(x)]
	sd(x)/sqrt(length(x))
}

#Standard Error of a proportion
SEP <- function( x, n ){
	sqrt( ( x * (1-x) ) / (n - 1) )
}
