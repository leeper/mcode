mergeNA <- function(...) {
	vars <- list(...)
	if(length(vars)==1)
		return(vars[[1]])
	lengths <- sapply(vars,FUN=length)
	# check variable lengths
	if(!identical(rep(lengths[1],length(vars)), lengths))
		stop("Vectors specified have different lengths")
	# test for NAs in each vector
	a <- do.call(cbind,vars)
	amat <- is.na(a)
	# check for mutual missigness
	mutual <- rowSums(!amat) > 1 #mutual <- apply(!amat, 1, sum) > 1
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
	# replace NAs, if applicable
	vars[[1]][unlist(notNA)] <- unlist(p)
	return(vars[[1]])
}
