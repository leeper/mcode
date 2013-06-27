\name{mergeNA}
\alias{mergeNA}
\title{Merge variables with missigness}
\description{Merge multiple variables with mutually exclusive missingness}
\usage{mergeNA(...)}
\arguments{
  \item{...}{Two or more vectors of equal length, which are to be combined into one new vector. If any two vectors have non-NA values at the same index, the function will report an error.}
}
\details{This is useful when, for example, a survey involves two forms, with each form coded as separate variables that need to be merged together.}
\value{A vector of length equal to all of \code{...}, which replaces missing values in the input vectors with the corresponding non-missing value from any other vector. If all vectors are NA at a given index, the result vector at that index is NA.}
%\references{}
\author{Thomas J. Leeper}
%\seealso{}
\examples{
z <- c(NA,NA,NA,NA,25,NA,NA,NA,NA,NA)
y <- c(NA,NA,NA,14,NA,NA,17,18,19,NA)
x <- c(NA,2,3,NA,NA,6,NA,NA,NA,10)
w <- c(NA,42,43,NA,25,NA,NA,NA,NA,NA)
mergeNA(x,y)
mergeNA(x,z)
mergeNA(x,y,z)
mergeNA(x,w) # fails for non-mutual exclusive missingness
}
%\keyword{}