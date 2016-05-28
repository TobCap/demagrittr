# demagrittr

## What is this package?
demagrittr() converts magrittr's syntax to eager evaluation syntax for
the purpose of: 

+ understanding quite complicated and nested piped sentences
+ debugging when an error occurs
+ run-time reduction (if `%>%` is heavily used inside a long loop)

This is experimental and not fully tested, so I would be glad
if you could inform me of any misunderstandings or mistakes.  

## Installation
``` r
# install.packages("devtools")
devtools::install_github("tobcap/demagrittr")
library("demagrittr")
```

## Usage
``` r
# direct passing 
demagrittr(x %>% f %>% g %>% h)
# {
#     `#tmp0` <- x
#     `#tmp1` <- f(`#tmp0`)
#     `#tmp2` <- g(`#tmp1`)
#     `#tmp3` <- h(`#tmp2`)
#     `#tmp3`
# }

# language object can be treated
expr <- quote(x %>% f %>% g %>% h)
demagrittr(expr)

# compile and evaluate
demagrittr(1:10 %>% sum %>% log %>% sin, eval_ = TRUE)
# [1] -0.7615754
```

# Benchmarking
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
system.time(eval(demagrittr(e)))

# > system.time(eval(e))
#    user  system elapsed 
#    5.44    0.02    5.54 
# > system.time(eval(demagrittr(e)))
#    user  system elapsed 
#    0.08    0.00    0.07 
```

``` r
library("microbenchmark")
library("magrittr")
library("pipeR")
library("demagrittr")

expr1 <- quote(1:10 %>% sum %>% log %>% sin)
expr2 <- demagrittr(expr1)
expr3 <- quote(1:10 %>>% sum %>>% log %>>% sin)

microbenchmark(
    "%>%" = eval(expr1)
  , demagrittr = eval(expr2)
  , "%>>%" = eval(expr3)
  , times = 1e3)
# Unit: microseconds
#        expr     min       lq      mean   median       uq      max neval
#         %>% 320.071 334.7810 372.66629 347.2635 364.2025 2766.955  1000
#  demagrittr  12.036  14.2660  17.21454  16.0490  17.3860  453.804  1000
#        %>>%  79.349  85.1445  98.48822  90.9395  96.2890 2281.055  1000

identical(eval(expr1), eval(expr2))
# [1] TRUE
```

```r
## from http://renkun.me/blog/2014/08/08/difference-between-magrittr-and-pipeR.html#performance
set.seed(1)

expr4 <- quote(
  lapply(1:100000, function(i) {
    sample(letters,6,replace = T) %>%
      paste(collapse = "") %>%
      "=="("rstats")
  })
)
expr5 <- demagrittr(expr4)
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
  , times = 1) # may produce an unstable result
# Unit: seconds
#        expr       min        lq      mean    median        uq       max neval
#         %>% 50.781067 50.781067 50.781067 50.781067 50.781067 50.781067     1
#  demagrittr  3.615714  3.615714  3.615714  3.615714  3.615714  3.615714     1
#        %>>% 10.414592 10.414592 10.414592 10.414592 10.414592 10.414592     1

identical(eval(expr4), eval(expr5))
# [1] TRUE
```

## Known problems
* Not guaranteed to preserve the same visibility of a result when evaluating
(printing the result or not in your console)
* `#tmp{n}` is used for the prefix-name of temporary symbols in the converted
language object. So there will be overwritting if you have already created
such a symbol in the environment where you want to evaluate a language object
convertedy by `demagrittr()`. (hope nobody uses such a tricky name as a symbol)

## To-Do
* make a wrapper function just like source() or sys.source()
