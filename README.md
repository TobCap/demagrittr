
<!-- README.md is generated from README.Rmd. Please edit that file -->
demagrittr
==========

What is this package?
---------------------

`demagrittr()` and `demagrittr_source()` convert magrittr's syntax to eager evaluation syntax (by default) for the purpose of:

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
demagrittr(x %>% f %>% g %>% h) # mode = "eager" by default
#> {
#>     `#0` <- x
#>     `#1` <- f(`#0`)
#>     `#2` <- g(`#1`)
#>     h(`#2`)
#> }
demagrittr(x %>% f %>% g %>% h, mode = "lazy")
#> h(g(f(x)))
demagrittr(x %>% f %>% g %>% h, mode = "promise")
#> (function(`#2`) h(`#2`))((function(`#1`) g(`#1`))((function(`#0`) f(`#0`))(x)))

# Manipulation for a language object
expr0 <- quote(x %>% f %>% g %>% h)
demagrittr(expr0, FALSE)
#> {
#>     `#0` <- x
#>     `#1` <- f(`#0`)
#>     `#2` <- g(`#1`)
#>     h(`#2`)
#> }

# The out put in `mode = "promise"` seems redundant but is essential in 
# this example.
demagrittr({set.seed(1); rnorm(1) %>% sum(., .)}, mode = "lazy")
#> {
#>     set.seed(1)
#>     sum(rnorm(1), rnorm(1))
#> }
demagrittr({set.seed(1); rnorm(1) %>% sum(., .)}, mode = "promise")
#> {
#>     set.seed(1)
#>     (function(`#0`) sum(`#0`, `#0`))(rnorm(1))
#> }
```

Precompiling and evaluation
---------------------------

``` r
compiled0 <- demagrittr(1:10 %>% sum %>% log %>% sin)
print(compiled0)
#> {
#>     `#0` <- 1:10
#>     `#1` <- sum(`#0`)
#>     `#2` <- log(`#1`)
#>     sin(`#2`)
#> }
eval(compiled0)
#> [1] -0.7615754
```

Building (unary) functions
--------------------------

``` r
demagrittr(f <- . %>% cos %>% sin)
#> f <- function(..) {
#>     `#0` <- ..
#>     `#1` <- cos(`#0`)
#>     sin(`#1`)
#> }
demagrittr(f <- . %>% cos %>% sin, mode = "lazy")
#> f <- function(..) sin(cos(..))
demagrittr(f <- . %>% cos %>% sin, mode = "promise")
#> f <- function(..) (function(`#1`) sin(`#1`))((function(`#0`) cos(`#0`))(..))

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
#>     `#0` <- rnorm(200)
#>     `#1` <- matrix(`#0`, ncol = 2)
#>     plot(`#1`)
#>     colSums(`#1`)
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
#>         `#0` <- iris
#>         `#1` <- subset(`#0`, Sepal.Length > mean(Sepal.Length))
#>         with(`#1`, cor(Sepal.Length, Sepal.Width))
#>     }
#>     {
#>         `#2` <- data.frame(z = rnorm(100))
#>         with(`#2`, ts.plot(z))
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
#>         `#0` <- iris$Sepal.Length
#>         sqrt(`#0`)
#>     }
#>     iris$Sepal.Length <- {
#>         `#1` <- iris$Sepal.Length
#>         sqrt(`#1`)
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
#>    5.99    0.00    6.08
system.time(eval(demagrittr(e, FALSE)))
#>    user  system elapsed 
#>    0.09    0.00    0.09
```

``` r
library("microbenchmark")
library("magrittr")
library("pipeR")
library("demagrittr")

expr1 <- quote(1:10 %>% sum %>% log %>% sin)
expr2e <- demagrittr(expr1, FALSE, mode = "eager")
expr2l <- demagrittr(expr1, FALSE, mode = "lazy")
expr2p <- demagrittr(expr1, FALSE, mode = "promise")
expr3 <- quote(1:10 %>>% sum %>>% log %>>% sin)

microbenchmark(
    "%>%" = eval(expr1)
  , "demagrittr eager" = eval(expr2e)
  , "demagrittr lazy" = eval(expr2l)
  , "demagrittr promise" = eval(expr2p)
  , "%>>%" = eval(expr3)
  , times = 1e3)
#> Unit: microseconds
#>                expr     min       lq      mean   median      uq      max
#>                 %>% 453.374 466.7475 534.22088 475.2180 507.315 5131.987
#>    demagrittr eager  13.374  15.1575  19.36210  18.2780  20.507  165.391
#>     demagrittr lazy  10.254  11.5910  14.21407  12.9290  15.158  140.872
#>  demagrittr promise  15.158  17.3870  21.03561  19.1700  21.845  181.439
#>                %>>% 207.295 216.2110 250.29301 229.1395 237.609 3868.605
#>  neval
#>   1000
#>   1000
#>   1000
#>   1000
#>   1000

Reduce(function(x, y) if (identical(x, y)) y else FALSE,
       lapply(list(expr1, expr2e, expr2l, expr2p, expr3), eval))
#> [1] -0.7615754
```

``` r
# from http://renkun.me/blog/2014/08/08/difference-between-magrittr-and-pipeR.html#performance

expr4 <- quote({
  set.seed(1)
  lapply(1:100000, function(i) {
    sample(letters, 6, replace = T) %>%
      paste(collapse = "") %>%
      "=="("rstats")
  })
})

expr5e <- demagrittr(expr4, FALSE, mode = "eager")
expr5l <- demagrittr(expr4, FALSE, mode = "lazy")
expr5p <- demagrittr(expr4, FALSE, mode = "promise")

expr6 <- quote({
  set.seed(1)
  lapply(1:100000, function(i) {
    sample(letters, 6, replace = T) %>>%
      paste(collapse = "") %>>%
      "=="("rstats")
  })
})

# My poor laptop takes huge time. The unit is 'seconds'.
microbenchmark(
    "%>%" = eval(expr4)
  , "demagrittr eager" = eval(expr5e)
  , "demagrittr lazy" = eval(expr5l)
  , "demagrittr promise" = eval(expr5p)
  , "%>>%" = eval(expr6)
  , times = 1)
#> Unit: seconds
#>                expr       min        lq      mean    median        uq
#>                 %>% 83.380868 83.380868 83.380868 83.380868 83.380868
#>    demagrittr eager  5.115426  5.115426  5.115426  5.115426  5.115426
#>     demagrittr lazy  4.952262  4.952262  4.952262  4.952262  4.952262
#>  demagrittr promise  6.994586  6.994586  6.994586  6.994586  6.994586
#>                %>>% 24.697093 24.697093 24.697093 24.697093 24.697093
#>        max neval
#>  83.380868     1
#>   5.115426     1
#>   4.952262     1
#>   6.994586     1
#>  24.697093     1
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
#>     `#0` <- x
#>     `#1` <- select(`#0`, b)
#>     filter(`#1`, b >= 8)
#> }
```

Known problems
==============

-   Not guaranteed to preserve the same visibility of a result when evaluating (printing the result or not in your console)
-   `#{n}` is used for the prefix-name of temporary symbols in the converted language object. So there will be overwritting if you have already created such a symbol in the environment where you want to evaluate a language object convertedy by `demagrittr()`. (hope nobody uses such a tricky name as a symbol)

To-Do
=====

-   Please suggest problems in issue.
