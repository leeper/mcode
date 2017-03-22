#' @title Multivariate recode
#' @description Recode one or more vectors to a single vector
#' @param \dots One or more vectors of equal length.
#' @param recodes A \sQuote{car}-like set of recode commands.
#' @param .fill A single value to use to fill in missing values in the resulting branched variables.
#' @param .result A character vector specifying the class of the resulting vector.
#' @details Recoding is a basic step in any analysis. It is fairly easy to recode a single variable (e.g. by replacing values in the vector or using the \code{recode} function in \bold{car} or \code{mapvalues} in \bold{plyr}), but it can be cumbersome to recode multiple variables into a single vector .This is useful when, for example, a factorial experiment has the group for each factor stored as separate variables, but analysis will be performed across the entire design (rather than factor-by-factor), or when it is necessary to create a factor representing multivariate combinations of demographic groups (e.g., an age-sex-race stratification) from a set of separate input vectors representing each demographic variable. That would normally require a series of \code{\link[base]{ifelse}} statements or complex use of boolean arguments. This function aims to make it simple to create a single vector from multiple input vectors in a manner more useful than \code{\link[base]{interaction}}.
#' 
#' The syntax borrows from the \code{recode} function in the \bold{car} package.
#' 
#' This really only works for categorical variables, but a continuous variable could be collapsed with a standard recode() command before being used with this.
#' @return A vector of length equal to the input vector(s) in \code{\dots}.
#' @examples 
#' # RECODE A SINGLE VARIABLE BASED ON A `car::recode`-STYLE SCHEME
#' r <- mcode(c(1,3,5,4,2), recodes = "5=1;4=2;3=3;2=4;1=5")
#' stopifnot(identical(r, c(5,3,1,2,4)))
#' 
#' # WORK WITH MISSING VALUES:
#' mcode(c(1,1,1,1,1,NA), c(1,1,2,2,NA,1), recodes = "c(1,1)=1;c(1,2)=2;c(1,NA)=3")
#' 
#' # COMPARE `mcode` TO VARIOUS ALTERNATIVES
#' a <- c(1,2,1,2,1,NA,2,NA)
#' b <- c(1,1,2,2,NA,1,NA,2)
#' 
#' # recode using `mcode`
#' m1 <- mcode(a, b, recodes = "c(1,1)=1;c(1,2)=2;c(2,1)=3;c(2,2)=4")
#' 
#' # compare to `ifelse`:
#' m2 <- ifelse(a == 1 & b == 1, 1, 
#'              ifelse(a == 1 & b == 2, 2, 
#'                     ifelse(a == 2 & b == 1, 3, 
#'                            ifelse(a == 2 & b == 2, 4, NA))))
#' identical(m1, m2)
#' 
#' # compare to a sequence of extraction statements
#' m3 <- rep(NA, length(a))
#' m3[a == 1 & b == 1] <- 1
#' m3[a == 1 & b == 2] <- 2
#' m3[a == 2 & b == 1] <- 3
#' m3[a == 2 & b == 2] <- 4
#' identical(m1, m3)
#' 
#' # compare to interaction
#' m4 <- interaction(a, b)
#' levels(m4) <- c("1.1" = 1, "1.2" = 2, "2.1" = 3, "2.2" = 4)[levels(m4)]
#' m4 <- as.numeric(as.character(m4))
#' identical(m1, m4)
#' 
#' r <- "c(1,1,1,1)=1;c(1,1,1,0)=2;c(1,1,0,1)=3;c(1,0,1,1)=4;c(0,1,1,1)=5"
#' mcode(c(rep(1,9),0), 
#'       c(rep(0,5),rep(1,5)), 
#'       c(rep(1,8),0,1), 
#'       c(rep(1,5),rep(0,2),rep(1,3)), 
#'       recodes = r)
#' @seealso \code{\link{mergeNA}}
#' @export 
mcode <- function(..., recodes, .fill = NA, .result = c("numeric", "character", "factor")){
    
    if (missing(recode)) {
        stop("'recodes' is missing with no default")
    }
    
    # process variables
    vars <- list(...)
    classes <- sapply(vars, FUN = inherits, what = "factor")
    if(any(classes)) {
        .factors <- match.arg(.factors)
        if(.factors == "character") {
            vars[classes] <- lapply(vars[classes], as.character)
        } else {
            vars[classes] <- lapply(vars[classes], as.numeric)
        }
    }
    lengths <- sapply(vars, FUN = length)
    if(any(lengths > lengths[1] | lengths < lengths[1]))
        stop("Vectors specified in '...' have different lengths")
    oldvar <- mapply(c, vars)
    
    # process recodes
    splitrecodes <- strsplit(gsub("\n|\t", "", recodes), split = ";")[[1]]
    x <- t(sapply(splitrecodes, function(x) strsplit(x, split = "=")[[1]]))
    outval <- unname(x[,2])
    inval1 <- gsub("[c()]", "", unname(x[,1])) # ignore bracketing
    inval <- unname(sapply(inval1, strsplit, split = ","))
    rlengths <- sapply(inval, length)
    if(any(rlengths < length(vars))) {
        w <- which(rlengths < length(vars))
        stop(ngettext(length(w), paste0("'recodes' entry ", w, " has less elements than ",length(vars)),
                                 paste0("'recodes' entries ", paste0(w, sep = ","), " has less element than ",length(vars))))
    } else if(any(rlengths > length(vars))) {
        w <- which(rlengths > length(vars))
        stop(ngettext(length(w), paste0("'recodes' entry ", w, " has more elements than ",length(vars)),
                                 paste0("'recodes' entries ", paste0(w, sep = ","), " has more elements than ",length(vars))))
    }
    if(any(rlengths > rlengths[1] | rlengths < rlengths[1])) {
        stop("'recodes' entries have inconsistent numbers of elements")
    }
    # parse special symbols
    parse_specials <- function(specials, var) {
        ## * - wildcard
        specials[specials == "*"] <- "*"
        ## NA - NA value
        specials[specials == "NA"] <- NA
        ## min - minimum of that variable
        specials[specials == "min"] <- min(var, na.rm = TRUE)
        ## max - maximum of that variable
        specials[specials == "max"] <- max(var, na.rm = TRUE)
        ## mean - mean of that variable
        specials[specials == "mean"] <- mean(var, na.rm = TRUE)
        ## median - median of that variable
        specials[specials == "median"] <- median(var, na.rm = TRUE)
        ## : - range of values
        
        return(specials)
    }
    
    if(FALSE) {
        invalmat <- matrix(character(), nrow = length(inval), ncol = rlengths[1])
        for(i in 1:ncol(invalmat)) {
            invalmat[,i] <- parse_specials(sapply(inval, `[`, i), vars[[i]])
        }
        
        
        sapply(seq_along(newvar), function(x) {
            v <- sapply(vars, `[`, x)
            
            
            #ranged <- grepl(":", , fixed = TRUE)
            return(x)
        })
        
        
        apply(oldvar, 1, function(x) {
            
        })
    }
    
    newvar <- vector(mode = "character", length = lengths[1]) # return vector
    for(i in 1:dim(oldvar)[1]){
        check <- unlist(lapply(inval, FUN=function(a) {
                                                s <- sum(oldvar[i,]==a,na.rm=TRUE)
                                                # sum code to deal with NAs
                                                if(is.na(s) || is.null(s))
                                                    s <- 0
                                                if(TRUE %in% is.na(oldvar[i,])){
                                                    for(k in 1:length(oldvar[i,])){
                                                        if(is.na(oldvar[i,])[k] & is.na(a)[k])
                                                            s <- s + 1
                                                    }
                                                }
                                                invisible(s)
                                            }
                                ))
        if(TRUE %in% (check>0)){
            # check for multiple matches (shouldn't happen)
            if(sum(check==length(oldvar[i,]))>1){
                newvar[i] <- NA
                warning("Multiple matches for case ",i," so NA used instead")
            }
            # check for no matches
            else if(max(check)<length(oldvar[i,]))
                newvar[i] <- .fill
            else
                newvar[i] <- outval[check==length(oldvar[i,])]
        }            
    }
    
    # format resulting variable
    newvar <- unlist(newvar)
    
    newvar <- eval(call(paste0("as.", match.arg(.result)), newvar))
    return(newvar)
}
