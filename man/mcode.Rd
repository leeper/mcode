\name{mcode}
\alias{mcode}
\title{Multivariate recode}
\description{Recode one or more vectors to a single vector}
\usage{
mcode(..., recodes, else.val=NA, 
      as.factor.result=NULL, as.numeric.result=TRUE, levels=NULL)
}
\arguments{
  \item{...}{One or more vectors of equal length.}
  \item{recodes}{A 'car'-like set of recode commands.}
  \item{else.val}{}
  \item{as.factor.result}{}
  \item{as.numeric.result}{}
  \item{levels}{}
}
\details{
Recoding is a basic step in any analysis. It's fairly easy to recode a single variable (e.g. by replacing values in the vector or using the \code{recode} function in \bold{car} or \code{mapvalues} in \bold{plyr}), but it can be cumbersome to recode multiple variables into a single vector .This is useful when, for example, a factorial experiment has the group for each factor stored as separate variables, but analysis will be performed across the entire design (rather than factor-by-factor), or when it is necessary to create a factor representing multivariate combinations of demographic groups (e.g., an age-sex-race stratification) from a set of separate input vectors representing each demographic variable. That would normally require a series of \code{\link[base]{ifelse}} statements or complex use of boolean arguments. This function aims to make it simple to create a single vector from multiple input vectors in a manner more useful than \code{\link[base]{interaction}}.

The syntax borrows from the \code{recode} function in the \bold{car} package.

This really only works for categorical variables, but a continuous variable could be collapsed with a standard recode() command before being used with this.
}
\value{A vector of length equal to the input vector(s) in \code{\dots}.}

%\references{}
\author{Thomas J. Leeper}
%\seealso{}
\examples{
a <- c(1,2,1,2,1,NA,2,NA)
b <- c(1,1,2,2,NA,1,NA,2)

# recode using `mcode`
m1 <- mcode(a, b, recodes = "c(1,1)=1;c(1,2)=2;c(2,1)=3;c(2,2)=4")

# compare to `ifelse`:
m2 <- ifelse(a == 1 & b == 1, 1, 
             ifelse(a == 1 & b == 2, 2, 
                    ifelse(a == 2 & b == 1, 3, 
                           ifelse(a == 2 & b == 2, 4, NA))))
identical(m1, m2)

# compare to a sequence of extraction statements
m3 <- rep(NA, length(a))
m3[a == 1 & b == 1] <- 1
m3[a == 1 & b == 2] <- 2
m3[a == 2 & b == 1] <- 3
m3[a == 2 & b == 2] <- 4
identical(m1, m3)

# compare to interaction
m4 <- interaction(a, b)
levels(m4) <- c("1.1" = 1, "1.2" = 2, "2.1" = 3, "2.2" = 4)[levels(m4)]
m4 <- as.numeric(as.character(m4))
identical(m1, m4)

r <- "c(1,1,1,1)=1;c(1,1,1,0)=2;c(1,1,0,1)=3;c(1,0,1,1)=4;c(0,1,1,1)=5"
mcode(c(rep(1,9),0), 
      c(rep(0,5),rep(1,5)), 
      c(rep(1,8),0,1), 
      c(rep(1,5),rep(0,2),rep(1,3)), 
      recodes = r)

# WORK WITH MISSING VALUES:
mcode(c(1,1,1,1,1,NA), c(1,1,2,2,NA,1), recodes="c(1,1)=1;c(1,2)=2;c(1,NA)=3")
}
%\keyword{}
