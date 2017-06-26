
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
demagrittr(expr0, is_NSE = FALSE)
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
#>    5.88    0.00    6.14
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
#>                expr     min       lq       mean  median       uq
#>                 %>% 441.337 504.1945 1322.75913 689.422 892.2585
#>    demagrittr eager  13.374  18.2785   34.01045  22.291  29.8690
#>     demagrittr lazy   9.808  13.3750   31.02140  16.496  22.2910
#>  demagrittr promise  14.266  19.6160   39.83118  24.073  31.6520
#>                %>>% 183.668 238.9460  561.67915 317.852 418.3790
#>         max neval
#>  104989.511  1000
#>    5510.021  1000
#>    5145.361  1000
#>    3981.837  1000
#>   19157.128  1000

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
#>                 %>% 75.461134 75.461134 75.461134 75.461134 75.461134
#>    demagrittr eager  6.138932  6.138932  6.138932  6.138932  6.138932
#>     demagrittr lazy  3.210860  3.210860  3.210860  3.210860  3.210860
#>  demagrittr promise  6.350760  6.350760  6.350760  6.350760  6.350760
#>                %>>% 19.559809 19.559809 19.559809 19.559809 19.559809
#>        max neval
#>  75.461134     1
#>   6.138932     1
#>   3.210860     1
#>   6.350760     1
#>  19.559809     1
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
-   The results where `return()` appears in middle of pipe stream differs by the mode.

``` r
expr_return <- quote(1:10 %>% sum %>% return %>% log)
expr_return
#> 1:10 %>% sum %>% return %>% log
demagrittr(expr_return, is_NSE = FALSE, mode = "eager")
#> {
#>     `#0` <- 1:10
#>     `#1` <- sum(`#0`)
#>     `#2` <- return(`#1`)
#>     log(`#2`)
#> }
demagrittr(expr_return, is_NSE = FALSE, mode = "lazy")
#> log(return(sum(1:10)))
demagrittr(expr_return, is_NSE = FALSE, mode = "promise")
#> (function(`#2`) log(`#2`))((function(`#1`) return(`#1`))((function(`#0`) sum(`#0`))(1:10)))

eval(expr_return)
#> [1] 4.007333
eval(demagrittr(expr_return, is_NSE = FALSE, mode = "eager"))
#> [1] 55
eval(demagrittr(expr_return, is_NSE = FALSE, mode = "lazy"))
#> [1] 55
eval(demagrittr(expr_return, is_NSE = FALSE, mode = "promise"))
#> [1] 4.007333

# not exit 
1:10 %>% sum %>% return %>% log
#> [1] 4.007333

# but error in global environment
{
    `#0` <- 1:10
    `#1` <- sum(`#0`)
    `#2` <- return(`#1`)
    log(`#2`)
}
#> [1] 55
## execute in console 
## Error: no function to return from, jumping to top level

# error in global environment
log(return(sum(1:10)))
#> [1] 55
## execute in console
## Error: no function to return from, jumping to top level

# runs, but expected result?
(function(`#2`) log(`#2`))((function(`#1`) return(`#1`))((function(`#0`) sum(`#0`))(1:10)))
#> [1] 4.007333
```

\`\`\`

To-Do
=====

-   Please suggest problems in issue.
