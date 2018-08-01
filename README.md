**This project is no longer being maintained!** For the core functionality of `mcode::mergeNA()`, use `dplyr::coalesce()` instead.

# Merge and recode across multiple variables

Recoding data to shape it into a form suitable for analysis is one of the first, most important, and most time-consuming parts of data analysis. Data rarely come in precisely the form needed to analyze and this is especially true for social science data such as that generated by surveys, experiments, and registry data. This package provides tools for simplifying some aspects of recoding, to expand upon the set of tools provided by base R such as `cut()`, `interaction()`, `[<-`, `ifelse()`, etc. and functions available in add-on packages such as the `recode()` from [**car**](https://cran.r-project.org/package=car), `mapvalues()` and `revalue()` from [**plyr**](https://cran.r-project.org/package=plyr), etc.


## Current Functionality

So far the package includes four functions: `branch()` and `unbranch()`, `mergeNA()`, and `mcode()`.

### `branch()` and `unbranch()`

The `branch()` function provides a generalization of a common expression: `model.matrix(~0 + x, data = dataset)`. That expression creates a matrix of indicator (dummy) variables from a categorical vector. `branch()` generalizes this so that it is possible to branch a vector of values into multiple vectors based on any arbitrarily branching rule.


```r
library("mcode")

a <- sample(1:5, 20, TRUE)
b1 <- sample(1:2, 20, TRUE)
b2 <- sample(1:2, 20, TRUE)
branch(a, b1) # 2-column matrix
```

```
##    f1 f2
## 1   0  1
## 2   1  0
## 3   2  0
## 4   1  0
## 5   0  1
## 6   3  0
## 7   2  0
## 8   4  0
## 9   5  0
## 10  3  0
## 11  1  0
## 12  0  1
## 13  5  0
## 14  1  0
## 15  1  0
## 16  2  0
## 17  1  0
## 18  0  4
## 19  3  0
## 20  0  4
```

```r
branch(a, list(b1, b2)) # 4-column matrix
```

```
##    f1.1 f2.1 f1.2 f2.2
## 1     0    1    0    0
## 2     0    0    1    0
## 3     2    0    0    0
## 4     0    0    1    0
## 5     0    1    0    0
## 6     0    0    3    0
## 7     2    0    0    0
## 8     4    0    0    0
## 9     5    0    0    0
## 10    0    0    3    0
## 11    0    0    1    0
## 12    0    1    0    0
## 13    5    0    0    0
## 14    0    0    1    0
## 15    1    0    0    0
## 16    2    0    0    0
## 17    0    0    1    0
## 18    0    0    0    4
## 19    0    0    3    0
## 20    0    0    0    4
```

`unbranch()` reverses this process by taking a multi-column matrix, or a data.frame, or a list of vectors, or some combination thereof, and collapsing them into a single vector. It inverts the behavior of `branch()`:


```r
a <- sample(1:5, 20, TRUE)
b1 <- sample(1:2, 20, TRUE)
b2 <- sample(1:2, 20, TRUE)

b <- branch(a, list(b1, b2))
u <- unbranch(b)
all.equal(a, u)
```

```
## [1] TRUE
```

`unbranch()` is especially useful for extracting relevant categories out of two or more vectors to create a single vector that ignores all irrelevant elements (replacing them with a specified value):


```r
m1 <- c(1,3,4,5,2)
m2 <- c(0,2,2,2,4)

# replace irrelevant categories with 0
unbranch(m1, m2, .ignore = c(1,2,3))
```

```
## [1] 0 0 4 5 4
```

```r
# replace irrelevant categories with NA
unbranch(m1, m2, .ignore = c(1,2,3), .fill = NA)
```

```
## [1]  0 NA  4  5  4
```

### `mergeNA()`

`mergeNA()` provides a wrapper for `unbranch()` that sets different default values. This makes it useful for merging multiple vectors (or vectors from data.frames and lists, or columns from matrices) into a single vector. It works on vectors that have "mutually exclusive missingness" (i.e., the vectors all have the same length and many missing values such that for each position (in each vector) only one vector has a valid, non-`NA` value.

This is especially useful for recoding complex survey questionnaire and experimental data, which often stores values to the same variable in different vectors within a data.frame (e.g., because survey respondents in each treatment condition were stored in their own data.frame column).

For example:


```r
mergeNA(c(1,2,NA,NA,NA), c(NA,NA,NA,4,5))
```

```
## [1]  1  2 NA  4  5
```

This is especially useful when it is necessary to merge a potentially large number of variables (e.g., from branched survey questions) into a single variable.

### `mcode()`

`mcode()` is still experimental. Building on `car::recode()`, the function aims to streamline recoding of variables in potentially complex ways. Where `car::recode()` converts an input vector into an output vector following a set of recoding rules, `mcode()` aims to recode an arbitrary number of vectors into a single output vector. For example one may want to create a categorical variable representing age and gender categories. Normally this would require two calls to `recode()` and/or a long set of additional operations (e.g., `cut()`, `[<-`, `ifelse()`, `*`, `+`, and/or `interaction()`). `mcode()` will consolidate these steps into a single operation.


```r
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
```

```
## [1] TRUE
```

```r
# compare to a sequence of extraction statements
m3 <- rep(NA, length(a))
m3[a == 1 & b == 1] <- 1
m3[a == 1 & b == 2] <- 2
m3[a == 2 & b == 1] <- 3
m3[a == 2 & b == 2] <- 4
identical(m1, m3)
```

```
## [1] TRUE
```

```r
# compare to interaction
m4 <- interaction(a, b)
levels(m4) <- c("1.1" = 1, "1.2" = 2, "2.1" = 3, "2.2" = 4)[levels(m4)]
m4 <- as.numeric(as.character(m4))
identical(m1, m4)
```

```
## [1] TRUE
```

## Installation

[![CRAN](https://www.r-pkg.org/badges/version/mcode)](https://cran.r-project.org/package=mcode)
[![Build Status](https://travis-ci.org/leeper/mcode.svg?branch=master)](https://travis-ci.org/leeper/mcode)
[![codecov.io](https://codecov.io/github/leeper/mcode/coverage.svg?branch=master)](https://codecov.io/github/leeper/mcode?branch=master)

This package is not yet on CRAN. To install the latest development version of **mcode** from GitHub:

```R
if (!require("ghit")) {
    install.packages("ghit")
    library("ghit")
}
install_github("leeper/mcode")
```

