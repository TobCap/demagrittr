
<!-- README.md is generated from README.Rmd. Please edit that file -->
demagrittr
==========

What is this package?
---------------------

`demagrittr()` and `demagrittr_source()` converts magrittr's syntax to eager evaluation syntax for the purpose of:

-   understanding quite complicated and nested piped sentences
-   debugging when an error occurs
-   run-time reduction (if `%>%` is heavily used inside a long loop)

This is experimental and not fully tested, so I would be glad if you could inform me of any misunderstandings or mistakes.

Installation
------------

``` r
# install.packages("devtools")
devtools::install_github("tobcap/demagrittr")
library("demagrittr")
```

Usage
-----

``` r
# NSE
demagrittr(x %>% f %>% g %>% h)
#> {
#>     `#tmp0` <- x
#>     `#tmp1` <- f(`#tmp0`)
#>     `#tmp2` <- g(`#tmp1`)
#>     `#tmp3` <- h(`#tmp2`)
#>     `#tmp3`
#> }

# Manipulation for a language object
expr0 <- quote(x %>% f %>% g %>% h)
demagrittr(expr0, FALSE)
#> {
#>     `#tmp0` <- x
#>     `#tmp1` <- f(`#tmp0`)
#>     `#tmp2` <- g(`#tmp1`)
#>     `#tmp3` <- h(`#tmp2`)
#>     `#tmp3`
#> }
```

Compiling and evaluation
------------------------

``` r
compiled0 <- demagrittr(1:10 %>% sum %>% log %>% sin)
print(compiled0)
#> {
#>     `#tmp0` <- 1:10
#>     `#tmp1` <- sum(`#tmp0`)
#>     `#tmp2` <- log(`#tmp1`)
#>     `#tmp3` <- sin(`#tmp2`)
#>     `#tmp3`
#> }
eval(compiled0)
#> [1] -0.7615754
```

Building (unary) functions
--------------------------

``` r
demagrittr(f <- . %>% cos %>% sin)
#> f <- function(.) {
#>     `#tmp4` <- .
#>     `#tmp5` <- cos(`#tmp4`)
#>     `#tmp6` <- sin(`#tmp5`)
#>     `#tmp6`
#> }

# The resul is just a language object. You need to eval().
eval(demagrittr(f <- . %>% cos %>% sin))
f(1)
#> [1] 0.5143953
sin(cos(1))
#> [1] 0.5143953
```

Tee operations
--------------

``` r
demagrittr(
  rnorm(200) %>%
  matrix(ncol = 2) %T>%
  plot %>% # plot usually does not return anything.
  colSums
)
#> {
#>     `#tmp0` <- rnorm(200)
#>     `#tmp1` <- matrix(`#tmp0`, ncol = 2)
#>     plot(`#tmp1`)
#>     `#tmp3` <- colSums(`#tmp1`)
#>     `#tmp3`
#> }
```

Pipe with exposition of variables
---------------------------------

``` r
demagrittr({
iris %>%
  subset(Sepal.Length > mean(Sepal.Length)) %$%
  cor(Sepal.Length, Sepal.Width)

data.frame(z = rnorm(100)) %$%
  ts.plot(z)
})
#> {
#>     {
#>         `#tmp0` <- iris
#>         `#tmp1` <- subset(`#tmp0`, Sepal.Length > mean(Sepal.Length))
#>         `#tmp2` <- with(`#tmp1`, cor(Sepal.Length, Sepal.Width))
#>         `#tmp2`
#>     }
#>     {
#>         `#tmp3` <- data.frame(z = rnorm(100))
#>         `#tmp4` <- with(`#tmp3`, ts.plot(z))
#>         `#tmp4`
#>     }
#> }
```

Compound assignment pipe operations
-----------------------------------

``` r
demagrittr({
iris$Sepal.Length <- 
  iris$Sepal.Length %>%
  sqrt
  
iris$Sepal.Length %<>% sqrt   
})
#> {
#>     iris$Sepal.Length <- {
#>         `#tmp0` <- iris$Sepal.Length
#>         `#tmp1` <- sqrt(`#tmp0`)
#>         `#tmp1`
#>     }
#>     {
#>         `#tmp2` <- iris$Sepal.Length
#>         `#tmp3` <- sqrt(`#tmp2`)
#>         iris$Sepal.Length <- `#tmp3`
#>     }
#> }
```

Benchmarking
------------

``` r
e <- quote(
 for (i in 1:10000) {
   i %>%
     identity %>%
     identity %>%
     identity %>%
     identity
 }
)

system.time(eval(e))
#>    user  system elapsed 
#>    5.61    0.02    5.64
system.time(eval(demagrittr(e, FALSE)))
#>    user  system elapsed 
#>    0.09    0.00    0.10
```

``` r
library("microbenchmark")
library("magrittr")
library("pipeR")
library("demagrittr")

expr1 <- quote(1:10 %>% sum %>% log %>% sin)
expr2 <- demagrittr(expr1, FALSE)
expr3 <- quote(1:10 %>>% sum %>>% log %>>% sin)

microbenchmark(
    "%>%" = eval(expr1)
  , demagrittr = eval(expr2)
  , "%>>%" = eval(expr3)
  , times = 1e3)
#> Unit: microseconds
#>        expr     min      lq      mean   median      uq      max neval
#>         %>% 337.021 369.564 481.86102 437.3245 482.127 6280.788  1000
#>  demagrittr  11.145  16.049  22.11929  18.7240  21.176 1982.445  1000
#>        %>>% 164.053 187.234 243.93021 217.1020 241.175 3678.691  1000

identical(eval(expr1), eval(expr2))
#> [1] TRUE
```

``` r
# from http://renkun.me/blog/2014/08/08/difference-between-magrittr-and-pipeR.html#performance
set.seed(1)
expr4 <- quote(
  lapply(1:100000, function(i) {
    sample(letters,6,replace = T) %>%
      paste(collapse = "") %>%
      "=="("rstats")
  })
)
expr5 <- demagrittr(expr4, FALSE)

set.seed(1)
expr6 <- quote(
  lapply(1:100000, function(i) {
    sample(letters,6,replace = T) %>>%
      paste(collapse = "") %>>%
      "=="("rstats")
  })
)

# My poor laptop takes huge time. The unit is 'seconds'.
microbenchmark(
    "%>%" = eval(expr4)
  , demagrittr = eval(expr5)
  , "%>>%" = eval(expr6)
  , times = 1)
#> Unit: seconds
#>        expr       min        lq      mean    median        uq       max
#>         %>% 64.468622 64.468622 64.468622 64.468622 64.468622 64.468622
#>  demagrittr  4.907491  4.907491  4.907491  4.907491  4.907491  4.907491
#>        %>>% 21.258164 21.258164 21.258164 21.258164 21.258164 21.258164
#>  neval
#>      1
#>      1
#>      1

identical(eval(expr4), eval(expr5))
#> [1] TRUE
```

Compiling source code
=====================

``` r
tmp_dir <- tempdir()
in_path <- file.path(tmp_dir, "test_in.r")
out_path <- file.path(tmp_dir, "test_out.r")

writeLines(
"
x <- data.frame(a = 1:5, b = 6:10)
y <- x %>%
  select(b) %>%
  filter(b >= 8)
", in_path)

demagrittr_source(in_path, out_path, ask = FALSE)
# input file
cat(paste0(readLines(in_path), collapse="\n"))
#> 
#> x <- data.frame(a = 1:5, b = 6:10)
#> y <- x %>%
#>   select(b) %>%
#>   filter(b >= 8)

# output file
cat(paste0(readLines(out_path), collapse="\n"))
#> x <- data.frame(a = 1:5, b = 6:10)
#> y <- {
#>     `#tmp0` <- x
#>     `#tmp1` <- select(`#tmp0`, b)
#>     `#tmp2` <- filter(`#tmp1`, b >= 8)
#>     `#tmp2`
#> }
```

Known problems
==============

-   Not guaranteed to preserve the same visibility of a result when evaluating (printing the result or not in your console)
-   `#tmp{n}` is used for the prefix-name of temporary symbols in the converted language object. So there will be overwritting if you have already created such a symbol in the environment where you want to evaluate a language object convertedy by `demagrittr()`. (hope nobody uses such a tricky name as a symbol)

To-Do
=====

-   `demagrittr_lazy()`: convert lazy evaluation like

    ``` r
    demagrittr_lazy(x %>% f %>% g)
    #> g(f(x))
    ```
