mergeNA <- function(...) {
	vars <- list(...)
	lengths <- sapply(vars,FUN=length)
	# check variable lengths
	if(!identical(rep(lengths[1],length(vars)), lengths))
		stop("Vectors specified in '...' have different lengths")
	# test for NAs in each vector
	a <- lapply(vars, function(x) is.na(x))
	amat <- do.call(cbind,a)
	# check for mutual missigness
	mutual <- apply(!amat, 1, sum) > 1
	if(any(mutual) & sum(mutual)>=10)
		stop("Missingness is not mutually exclusive at 10 or more indices")
	else if(any(mutual))
		stop("Missingness is not mutually exclusive at indices ",paste(which(mutual),collapse=","))
	# positions of non-NAs in each vector
	notNA <- apply(!amat, 2, which)
	if(!is.list(notNA))
		notNA <- lapply(seq_len(ncol(notNA)), function(i) notNA[,i])
	# pairs of NA positions and mergeable values
	p <- mapply(function(val,pos) val[pos], vars, notNA, SIMPLIFY=FALSE)
	pairs <- cbind(unlist(notNA),unlist(p))
	# start with first variable
	out <- vars[[1]]
	# replace NAs, if applicable with values from `pairs`
	out[pairs[,1]] <- pairs[,2]
	return(out)
}
