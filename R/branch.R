#' @rdname branch
#' @title Create and recombine variables based on a branching factor
#' @description Create and recombine variables from a source variable and a branching factor (or list of branching factors)
#' @param x A vector containing values to be divided into new variables.
#' @param f A factor in the sense that \code{as.factor(f)} defines the grouping, or a list of such factors in which case their \code{\link{interaction}} is used for the grouping.
#' @param .fill A single value to use to fill in missing values in the resulting branched variables.
#' @details These functions can be used to create dummy variables from a source variable, or to create multiple new variables from an existing variable where the values are mutually exclusive. This is useful when, for example, a survey involves two forms, with each form coded as separate variables that need to be merged together or, conversely, where a single variable contains data for different groups that need to be analyzed separately.
#' @return For \code{branch}, a matrix of the same number of rows as \code{length(x)} and number of columns equal to the number of levels in \code{f}.
#' 
#' For \code{unbranch}, a vector of length equal to all of \code{...}, which replaces missing values in the input vectors with the corresponding non-missing value from any other vector. If all vector items at a given position are in the \code{.ignore} set, the result vector at that index is \code{.fill}.
#' @examples 
#' # branch a vector in a matrix and unbranch the result
#' a <- sample(1:5, 20, TRUE)
#' b1 <- sample(1:2, 20, TRUE)
#' b2 <- sample(1:2, 20, TRUE)
#' branch(a, b1) # 2-column matrix
#' branch(a, list(b1, b2)) # 4-column matrix
#' 
#' # unbranch from a `branch` matrix
#' b <- branch(a, list(b1, b2))
#' u <- unbranch(b)
#' all.equal(a, u)
#' 
#' # unbranch multiple vectors with `NA` values
#' x <- c(NA,2,3,NA,NA,6,NA,NA,NA,10)
#' y <- c(NA,NA,NA,14,NA,NA,17,18,19,NA)
#' z <- c(NA,NA,NA,NA,25,NA,NA,NA,NA,NA)
#' unbranch(x,y, .ignore = NA)
#' unbranch(x,z, .ignore = NA)
#' unbranch(x,y,z, .ignore = NA)
#' # equivalent to `mergeNA`
#' mergeNA(x,y,z)
#' 
#' # unbranch multiple vectors with multiple `.ignore` values
#' m1 <- c(1,3,4,5,2)
#' m2 <- c(0,2,2,2,4)
#' unbranch(m1, m2, .ignore = c(1,2,3))
#' unbranch(m1, m2, .ignore = c(1,2,3), .fill = NA)
#' 
#' \dontrun{
#'   # fails for non-mutual exclusive missingness
#'   w <- c(NA,42,43,NA,25,NA,NA,NA,NA,NA)
#'   unbranch(x, w, .fill = NA) 
#' }
#' @seealso \code{\link{mergeNA}}
#' @export 
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

#' @rdname branch
#' @param .ignore A (potentially multi-item) vector of values to ignore when merging across the vectors in \code{\dots}.
#' @param .factors A character string indicating whether to treatment factors  in \code{\dots} as character (the default) or numeric.
#' @param \dots Two or more vectors of equal length, which are to be combined into one new vector. If any two vectors have values at the same index that are not specified in \code{.ignore}, the function will report an error. It is also possible to pass one or more data.frames and/or matrices (which will be coerced to a list of column vectors).
#' @export
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
