\name{mcode}
\alias{mcode}
\title{Multivariate recode}
\description{Recode from multiple input variables}
\usage{mcode(..., recodes, else.val=NA, as.factor.result=NULL, as.numeric.result=TRUE, levels=NULL)}
\arguments{
  \item{...}{One or more vectors of equal length.}
  \item{recodes}{A 'car'-like set of recode commands.}
  \item{else.val}{}
  \item{as.factor.result}{}
  \item{levels}{}
}

\details{This is useful when, for example, a factorial experiment has the group for each factor stored as separate variables, but analysis will be performed across the entire design (rather than factor-by-factor). This really only works for categorical variables, but a continuous variable could be collapsed with a standard recode() command before being used with this.}
\value{A vector of length equal to all of \code{...}.}

%\references{}
\author{Thomas J. Leeper}
%\seealso{}
\examples{
recodes <- "c(1,1)=1;c(1,2)=2;c(1,3)=3;c(2,1)=4;c(2,2)=5;c(2,3)=6;c(3,1)=7;c(3,2)=8;c(3,3)=9"
mcode(c(1,2,1,2),c(1,1,2,2), recodes=recodes)

recodes <- "c(1,1,1,1)=1;c(1,1,1,0)=2;c(1,1,0,1)=3;c(1,0,1,1)=4;c(0,1,1,1)=5"
mcode(c(rep(1,9),0),c(rep(0,5),rep(1,5)),c(rep(1,8),0,1),c(rep(1,5),rep(0,2),rep(1,3)), recodes=recodes)

# WORK WITH MISSING VALUES:
mcode(c(1,1,1,1,1,NA),c(1,1,2,2,NA,1), recodes="c(1,1)=1;c(1,2)=2;c(1,NA)=3")
}
%\keyword{}