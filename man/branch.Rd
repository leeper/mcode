\name{branch}
\alias{branch}
\alias{unbranch}
\title{Create variables from a variable and a branching factor}
\description{Create variables from a source variable and a branching factor or recombine multiple variables with mutually exclusive missingness}
\usage{
branch(x, f, fill = 0)

unbranch(...)
}
\arguments{
  \item{x}{A vector containing values to be divided into new variables.}
  \item{f}{A factor in the sense that \code{as.factor(f)} defines the grouping, or a list of such factors in which case their \code{\link{interaction}} is used for the grouping.}
  \item{fill}{A single value to use to fill in missing values in the resulting branched variables.}
  \item{...}{Two or more vectors of equal length, which are to be combined into one new vector. If any two vectors have non-NA values at the same index, the function will report an error.}
}
\details{These functions can be used to create dummy variables from a source variable, or to create multiple new variables from an existing variable where the values are mutually exclusive. This is useful when, for example, a survey involves two forms, with each form coded as separate variables that need to be merged together.}
\value{
For \code{branch}, a matrix of the same number of rows as \code{length(x)} and number of columns equal to the number of levels in \code{f}.

For \code{unbranch}, a vector of length equal to all of \code{...}, which replaces missing values in the input vectors with the corresponding non-missing value from any other vector. If all vectors are NA at a given index, the result vector at that index is \code{NA}.
}
%\references{}
\author{Thomas J. Leeper}
\seealso{\code{\link{mergeNA}}}
\examples{
# branch a vector in a matrix and unbranch the result
set.seed(1)
a <- sample(1:5, 20, TRUE)
b1 <- sample(1:2, 20, TRUE)
b2 <- sample(1:2, 20, TRUE)
branch(a, b1)
branch(a, list(b1, b2))
all.equal(a, unbranch(branch(a, list(b1, b2), fill = NA)))

# unbranch multiple vectors
z <- c(NA,NA,NA,NA,25,NA,NA,NA,NA,NA)
y <- c(NA,NA,NA,14,NA,NA,17,18,19,NA)
x <- c(NA,2,3,NA,NA,6,NA,NA,NA,10)
w <- c(NA,42,43,NA,25,NA,NA,NA,NA,NA)
unbranch(x,y)
unbranch(x,z)
unbranch(x,y,z)

\dontrun{
unbranch(x,w, fill = NA) # fails for non-mutual exclusive missingness
}
}
%\keyword{}