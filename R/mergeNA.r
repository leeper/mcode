# FUNCTION TO MERGE TWO VARIABLES (e.g., TWO VERSIONS OF A SURVEY QUESTION)

# Copyright (C) 2011  Thomas J. Leeper
# This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.
# This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.


# a and b are two vectors of equal length, which are to be combined into one new vector
# this is useful when a survey involves two forms, with each form coded separately

# NEED TO UPDATE TO AN INFINITE NUMBER OF VARIABLES

mergeNA <- function(a,b) {
	if(!length(a)==length(b))
		stop("length(a) != length(b)")
	x <- NA
	for(i in 1:length(a)){
		if(!is.na(a[i]) & is.na(b[i]))
			x[i] <- a[i]
		else if(is.na(a[i]) & !is.na(b[i]))
			x[i] <- b[i]
		else if(is.na(a[i]) & is.na(b[i]))
			x[i] <- NA
		else if(!is.na(a[i]) & !is.na(b[i])){
			if(a[i]==b[i])
				x[i] <- a[i]
			else{
				x[i] <- NA
				cat("Respondent ",i,": ",a[i],", ",b[i],"\n",sep="")
			}
		}
	}
	return(x)
}
