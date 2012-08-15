ApproxBayesFac <- function(x) {
	max.probs <- max(x)
	new.probs <- x - max.probs
	trans.probs <- exp(new.probs)
	BayesProb <- trans.probs/sum(trans.probs)
	return(BayesProb)
}

std.err <- function(x){
	x <- x[!is.na(x)]
	sd(x)/sqrt(length(x))
}

