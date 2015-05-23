mergeNA <- function(..., .factors = c("character", "numeric")) {
    unbranch(..., .ignore = NA, .fill = NA, .factors = .factors)
}
