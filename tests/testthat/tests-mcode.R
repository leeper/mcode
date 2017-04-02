context("Test mcode")

test_that("Output as expected from univariate numeric mcode()", {
    x <- c(1,3,5,4,2)
    expect_equivalent(mcode(x, recodes = "5=1;4=2;3=3;2=4;1=5"), c(5,3,1,2,4))
    expect_equivalent(mcode(x, recodes = "(5)=1;(4)=2;(3)=3;(2)=4;(1)=5"), c(5,3,1,2,4))
    expect_equivalent(mcode(x, recodes = "c(5)=1;c(4)=2;c(3)=3;c(2)=4;c(1)=5"), c(5,3,1,2,4))
    expect_equivalent(mcode(x, recodes = "5=1;4=2;3=3;2=   4;1=5"), c(5,3,1,2,4))
    expect_equivalent(mcode(x, recodes = "5=1;4=2;3=3;   2=4;1=5"), c(5,3,1,2,4))
    expect_equivalent(mcode(x, recodes = "5=1;4=2;3=3;2   =4;1=5"), c(5,3,1,2,4))
    expect_equivalent(mcode(x, recodes = "5=1;4=2;3=3;2 =  4;1=5"), c(5,3,1,2,4))
})

test_that("Output as expected from univariate character mcode()", {
    x <- letters[1:5]
    expect_equivalent(mcode(x, recodes = "a=d;b=e;c=a;d=x;e=y"), c("d","e","a","x","y"))
    expect_equivalent(mcode(x, recodes = "'a'='d';'b'='e';'c'='a';'d'='x';'e'='y'"), c("d","e","a","x","y"))
    expect_equivalent(mcode(x, recodes = '"a"="d";"b"="e";"c"="a";"d"="x";"e"="y"'), c("d","e","a","x","y"))
    expect_equivalent(mcode(x, recodes = "c('a')='d';c('b')='e';c('c')='a';c('d')='x';c('e')='y'"), c("d","e","a","x","y"))
    
    expect_equivalent(mcode(x, recodes = '"a" ="d";"b"="e";"c"="a";"d"="x";"e"="y"'), c("d","e","a","x","y"))
    expect_equivalent(mcode(x, recodes = '"a"= "d";"b"="e";"c"="a";"d"="x";"e"="y"'), c("d","e","a","x","y"))
    expect_equivalent(mcode(x, recodes = '"a"="d" ;"b"="e";"c"="a";"d"="x";"e"="y"'), c("d","e","a","x","y"))
    expect_equivalent(mcode(x, recodes = '"a"="d"; "b"="e";"c"="a";"d"="x";"e"="y"'), c("d","e","a","x","y"))
    expect_equivalent(mcode(x, recodes = '"a"=" d";"b"="e";"c"="a";"d"="x";"e"="y"'), c(" d","e","a","x","y"))
})

test_that("Output as expected from multivariate numeric mcode()", {
    x <- c(1,3,5,4,2)
    y <- c(1,1,1,0,0)
    R <- "(5,0)=1;(4,0)=2;(3,0)=3;(2,0)=4;(1,0)=5;(5,1)=95;(4,1)=96;(3,1)=97;(2,1)=98;(1,1)=99"
    expect_equivalent(mcode(x, y, recodes = R), c(99,97,95,2,4))
})

test_that("Output as expected from multivariate character mcode()", {

})

test_that("Output as expected from mixed multivariate mcode()", {

})

test_that("Output from single-vector mcode matches recode", {

})

test_that("Output from single-vector mcode matches ifelse sequence", {

})
