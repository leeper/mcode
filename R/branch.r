branch <- function(x, f, .fill = 0){
    if (is.list(f)) {
        f <- interaction(f)
    } else if (!is.factor(f)) {
        f <- as.factor(f)
    }
    out <- x * model.matrix(~ 0 + f)
    attr(out,'assign') <- NULL
    attr(out,'contrasts') <- NULL
    if(!is.na(.fill) && .fill == 0){
        return(out)
    } else if(is.na(.fill)) {
        out[out == 0] <- NA
        return(out)
    } else {
        out[out == 0] <- .fill
        return(out)
    }
}

unbranch <- function(..., .ignore = 0, .fill = 0, .factors = c("character", "numeric")){
    vars <- list(...)
    ismat <- sapply(vars, is.matrix)
    for(i in which(ismat)){
        tmp <- vars[[i]]
        vars[i] <- NULL
        vars <- append(vars, split(tmp, col(tmp)))
    }
    isdf <- sapply(vars, is.data.frame)
    for(i in which(isdf)){
        tmp <- vars[[i]]
        vars[i] <- NULL
        vars <- append(vars, tmp)
    }
    if(length(vars) == 1)
        return(vars[[1]])
    classes <- sapply(vars, FUN = inherits, what = "factor")
    if(any(classes)) {
        .factors <- match.arg(.factors)
        if(.factors == "character") {
            vars[classes] <- lapply(vars[classes], as.character)
        } else {
            vars[classes] <- lapply(vars[classes], as.numeric)
        }
    }
    lengths <- sapply(vars, length)
    if(any(lengths > lengths[1] | lengths < lengths[1]))
        stop("Vectors specified have different lengths")
    a <- do.call(cbind, vars)
    amat <- structure(a %in% .ignore, dim = dim(a))
    mutual <- rowSums(!amat) > 1
    if(any(mutual) & sum(mutual) >= 10) {
        stop(".ignore values are not mutually exclusive at 10 or more indices")
    } else if(any(mutual)) {
        stop(".ignore values are not mutually exclusive at indices ",
             paste(which(mutual), collapse=","))
    }
    out <- apply(a, 1, function(x) {
        z <- x[!x %in% .ignore]
        if(length(z)) {
            return(z)
        } else {
            return(.fill)
        }
    })
    return(out)
}
