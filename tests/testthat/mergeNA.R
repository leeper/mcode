context("Test mergeNA")

test_that("Output as expected from mergeNA", {
    x <- c(NA,2,3,NA,NA,6,NA,NA,NA,10)
    y <- c(NA,NA,NA,14,NA,NA,17,18,19,NA)
    z <- c(NA,NA,NA,NA,25,NA,NA,NA,NA,NA)
    expect_equal(mergeNA(x,y), c(NA, 2, 3, 14, NA, 6, 17, 18, 19, 10))
    expect_equal(mergeNA(x,z), c(NA, 2, 3, NA, 25, 6, NA, NA, NA, 10))
    expect_equal(mergeNA(x,y,z), c(NA, 2, 3, 14, 25, 6, 17, 18, 19, 10))
})

test_that("Non-mutually exclusive missingness errors mergeNA", {
    x <- c(NA,2,3,NA,NA,6,NA,NA,NA,10)
    w <- c(NA,42,43,NA,25,NA,NA,NA,NA,NA)
    expect_error(mergeNA(x,w))
})

test_that("Factors coerced correctly in mergeNA", {
    expect_equal(mergeNA(c(1,NA,NA,NA), factor(c(NA,'a','b',NA))), c("1", "a", "b", NA))
    expect_equal(mergeNA(c(1,NA,NA,NA), factor(c(NA,'a','b',NA)), .factors = "character"), c("1", "a", "b", NA))
    expect_equal(mergeNA(c(1,NA,NA,NA), factor(c(NA,'a','b',NA)), .factors = "numeric"), c(1, 1, 2, NA))
})
