context("Test branch")

test_that("Output as expected from branch", {
    set.seed(1)
    a <- c(5L, 2L, 3L, 2L, 4L)
    b1 <- c(1L, 1L, 2L, 1L, 2L)
    b2 <- c(1L, 2L, 1L, 1L, 1L)
    expect_equivalent(branch(a, b1), matrix(c(5, 2, 0, 2, 0, 0, 0, 3, 0, 4), nrow = 5))
    expect_equivalent(branch(a, list(b1, b2)), matrix(c(5, 0, 0, 2, 0, 0, 0, 3, 0, 4, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0), nrow = 5))
})

context("Test unbranch")

test_that("Output as expected from unbranch", {
    x <- c(NA,2,3,NA,NA,6,NA,NA,NA,10)
    y <- c(NA,NA,NA,14,NA,NA,17,18,19,NA)
    z <- c(NA,NA,NA,NA,25,NA,NA,NA,NA,NA)
    expect_equal(unbranch(x,y, .ignore = NA, .fill = NA), c(NA, 2, 3, 14, NA, 6, 17, 18, 19, 10))
    expect_equal(unbranch(x,y, .ignore = NA, .fill = NA), mergeNA(x,y))
    expect_equal(unbranch(x,z, .ignore = NA, .fill = NA), c(NA, 2, 3, NA, 25, 6, NA, NA, NA, 10))
    expect_equal(unbranch(x,z, .ignore = NA, .fill = NA), mergeNA(x,z))
    expect_equal(unbranch(x,y,z, .ignore = NA, .fill = NA), c(NA, 2, 3, 14, 25, 6, 17, 18, 19, 10))
    expect_equal(unbranch(x,y,z, .ignore = NA, .fill = NA), mergeNA(x,y,z))
})

test_that("Non-mutually exclusive ignore values errors unbranch", {
    x <- c(NA,1,2,NA,NA,2,NA,NA,NA,1)
    w <- c(NA,3,3,NA,1,NA,NA,NA,NA,NA)
    expect_error(unbranch(x,w, .ignore = NA))
    expect_equal(unbranch(x,w, .ignore = c(3,NA)), c(0, 1, 2, 0, 1, 2, 0, 0, 0, 1))
})

test_that("Factors coerced correctly in unbranch", {
    # need to write this
})

test_that("Unbranch a branched matrix", {
    # need to write this
})
