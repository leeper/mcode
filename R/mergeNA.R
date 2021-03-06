#' @title Merge variables with missingness
#' @description Merge multiple variables with mutually exclusive missingness
#' @param \dots Two or more vectors of equal length, which are to be combined into one new vector. If any two vectors have non-NA values at the same index, the function will report an error.
#' @param .factors A character string specifying whether to coerce factors in \code{\dots} to \dQuote{character} (i.e., their level labels), which is the default, or \dQuote{numeric} (the underlying integer factor levels). This may affect the return value of the function.
#' @details This is a wrapper around \code{\link{unbranch}} with the specified arguments: \code{.ignore = NA, .fill = NA}. This is useful when, for example, a survey involves two forms, with each form coded as separate variables that need to be merged together or in an experimental dataset where a single outcome measure is stored in a separate variable for each experimental condition.
#' @return A vector of length equal to the inputs, with missing values replaced when available in any other vector. If all vectors are \code{NA} at a given index, the result at that index remains \code{NA}.
#' @examples
#' # basic examples
#' x <- c(NA,2,3,NA,NA,6,NA,NA,NA,10)
#' y <- c(NA,NA,NA,14,NA,NA,17,18,19,NA)
#' z <- c(NA,NA,NA,NA,25,NA,NA,NA,NA,NA)
#' mergeNA(x,y)
#' mergeNA(x,z)
#' mergeNA(x,y,z)
#' 
#' # mergeNA and unbranch equivalent)
#' identical(mergeNA(x,y,z),
#'           unbranch(x,y,z, .ignore = NA, .fill = NA))
#' 
#' \dontrun{
#'   # non-mutually exclusive missingness
#'   w <- c(NA,42,43,NA,25,NA,NA,NA,NA,NA)
#'   mergeNA(x,w) 
#' }
#' 
#' # treat factors as character
#' mergeNA(c(1,NA,NA,NA), factor(c(NA,'a','b',NA)))
#' mergeNA(c(1,NA,NA,NA), factor(c(NA,'a','b',NA)), .factors = "character")
#' # treat factors as numeric
#' mergeNA(c(1,NA,NA,NA), factor(c(NA,'a','b',NA)), .factors = "numeric")
#' @seealso \code{\link{unbranch}} 
#' @export
mergeNA <- function(..., .factors = c("character", "numeric")) {
    unbranch(..., .ignore = NA, .fill = NA, .factors = .factors)
}
