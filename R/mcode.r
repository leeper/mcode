mcode <- function(..., recodes, else.val=NA, as.factor.result=NULL, as.numeric.result=TRUE, levels=NULL){
    
    ## STILL NEED TO FIGURE OUT A WILDCARD SYNTAX
    ### AN EXAMPLE TO WORK FROM: recodes <- "c(1,1)=1;c(1,2)=2;c(1,3)=3;c(2,'*')=4"

    
    vars <- list(...)
    lengths <- sapply(vars,FUN=length)
    # check variable lengths
    if(!identical(rep(lengths[1],length(vars)), lengths))
        stop("Vectors specified in '...' have different lengths")
    splitrecodes <- strsplit(recodes,";")[[1]]
    # check number of vars implied by recode vectors
    codesplit <- function(a)
        eval(parse(text=strsplit(a,"=")[[1]][1]))
    ncodes <- lapply(splitrecodes,FUN=function(a) length(codesplit(a)))
    for(i in 1:length(ncodes)){
        if(!ncodes[[i]]==length(vars))
            stop(paste("Recode",i,"has",ncodes[[i]],"items but should have",length(vars)))
    }
    # list of old values for each recode combination
    oldvals <- lapply(splitrecodes,FUN=codesplit)
    # check for duplicate recode combinations
    dup <- duplicated(oldvals)
    if(sum(dup)>0)
        stop("Duplicate recodes in positions:",seq(length(dup))[dup==TRUE])
    # list of new values for each recode combination
    newval <- lapply(splitrecodes,FUN=function(a) strsplit(a,"=")[[1]][2])
    # original variables as list
    oldvar <- mapply(FUN=c,vars)
    # create new var from original vars, based on splitrecodes
    newvar <- vector(length=lengths[1], mode=mode(vars[[1]]))
    for(i in 1:dim(oldvar)[1]){
        check <- unlist(lapply(oldvals, FUN=function(a) {
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
                newvar[i] <- else.val
            else
                newvar[i] <- newval[check==length(oldvar[i,])]
        }            
    }
    # format resulting variable
    newvar <- unlist(newvar)
    if(!is.null(as.numeric.result) && as.numeric.result)
        newvar <- as.numeric(newvar)
    if(!is.null(as.factor.result) && as.factor.result) {
        if(!is.null(levels))
            newvar <- factor(newvar, levels = levels)
        else
            as.factor(newvar)
    }
    return(newvar)
}
