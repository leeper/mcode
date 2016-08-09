mcode <- function(..., recodes, .fill = NA, .result = c("numeric", "character", "factor"), .levels = NULL){
    
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
