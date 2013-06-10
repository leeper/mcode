# FUNCTION TO RECODE A NEW VARIABLE FROM MULTIPLE OLD VARIABLES

# Copyright (C) 2013  Thomas J. Leeper
# This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.
# This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.


# ... is one or more vectors of equal length
# recodes is a 'car'-like set of recode commands
# this is useful when, for example, a factorial experiment has the group for each factor stored as separate variables,
# but analysis will be performed across the entire design (rather than factor-by-factor)
# really only works for categorical variables, but a continuous variable could be collapsed with a standard recode() command before being used with this


## STILL NEED TO FIGURE OUT A WILDCARD SYNTAX
### AN EXAMPLE TO WORK FROM: recodes <- "c(1,1)=1;c(1,2)=2;c(1,3)=3;c(2,'*')=4"


mcode <- function(..., recodes, else.val=NA, as.factor.result=NULL, as.numeric.result=TRUE, levels=NULL){
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

# recodes <- "c(1,1)=1;c(1,2)=2;c(1,3)=3;c(2,1)=4;c(2,2)=5;c(2,3)=6;c(3,1)=7;c(3,2)=8;c(3,3)=9"
# mcode(c(1,2,1,2),c(1,1,2,2), recodes=recodes)

# recodes <- "c(1,1,1,1)=1;c(1,1,1,0)=2;c(1,1,0,1)=3;c(1,0,1,1)=4;c(0,1,1,1)=5"
# mcode(c(rep(1,9),0),c(rep(0,5),rep(1,5)),c(rep(1,8),0,1),c(rep(1,5),rep(0,2),rep(1,3)), recodes=recodes)

# WORK WITH MISSING VALUES:
# mcode(c(1,1,1,1,1,NA),c(1,1,2,2,NA,1), recodes="c(1,1)=1;c(1,2)=2;c(1,NA)=3")

