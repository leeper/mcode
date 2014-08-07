branch <- function(x, f, fill = 0){
    if (is.list(f)) 
        f <- interaction(f)
    else if (!is.factor(f)) 
        f <- as.factor(f)
    out <- x * model.matrix(~ 0 + f)
    attr(out,'assign') <- NULL
    attr(out,'contrasts') <- NULL
    if(!is.na(fill) && fill == 0){
        return(out)
    } else if(is.na(fill)) {
        out[out == 0] <- NA
        return(out)
    } else {
        out[out == 0] <- fill
        return(out)
    }
}

unbranch <- function(..., fill = 0){
    vars <- list(...)
    cls <- sapply(vars, class)
    
    # split matrices into column vectors
    for(i in which(cls == 'matrix')){
        tmp <- vars[[i]]
        vars[i] <- NULL
        vars <- append(vars, split(tmp, col(tmp)))
    }
    
    if(length(vars)==1)
        return(vars[[1]])
    lengths <- sapply(vars, length)
    # check variable lengths
    if(abs(max(lengths) - min(lengths)) > 1L)
        stop("Vectors specified have different lengths")
    
    # test for `fill` values in each vector
    if(is.na(fill)){
        m <- unlist(lapply(vars, function(x) which(is.na(x))))
    } else {
        m <- unlist(lapply(vars, function(x) which(x == fill)))
    }
    if(any(duplicated(m))) {
        w <- sort(unique(c(which(duplicated(z)), which(duplicated(z, fromLast = TRUE)))))
        if(length(w) >= 10)
            stop("Missingness is not mutually exclusive at 10 or more indices")
        else
            stop("Missingness is not mutually exclusive at indices ",paste(w,collapse=","))
    }
    
    # positions of non-NAs in each vector
    a <- do.call(cbind, vars)
    a_out <- apply(a, 1, function(x) {
        u <- unique(x[!is.na(x)])
        if(length(u) == 0)
            return(NA)
        else
            return(u)
    })
    
    return(a_out)
}
