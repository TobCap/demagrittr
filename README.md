# demagrittr

## What is this package?
demagrittr() converts magrittr's syntax to eager evaluation syntax whose 
advantages are:

+ easy to read
+ easy to debug
+ run-time reduction (if you heavily use `%>%` inside a loop)

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
# directly passing 
demagrittr(x %>% f %>% g %>% h)

# call object can be treated
expr <- quote(x %>% f %>% g %%> h)
demagritr(expr)

# compile and evaluate
demagrittr(1:10 %>% sum %>% log %>% sin, eval_ = TRUE)
# [1] -0.7615754
```

# benchmarking
``` r
expr <- quote(1:10 %>% sum %>% log %>% sin)
library(microbenchmark)
microbenchmark("%>%" = eval(expr), demagrittr = eval(demagrittr(expr)))
# Unit: microseconds
#        expr     min       lq     mean   median       uq     max neval
#         %>% 329.436 339.4660 363.9934 352.6170 374.4605 643.269   100
#  demagrittr 469.413 485.2385 517.8562 496.6055 518.6715 963.343   100

expr2 <- demagrittr(expr)
microbenchmark("%>%" = eval(expr), demagrittr = eval(expr2))
# Unit: microseconds
#        expr     min      lq      mean  median      uq     max neval
#         %>% 324.086 329.436 354.10145 335.454 365.322 669.571   100
#  demagrittr  12.037  13.374  16.73109  16.495  16.941  73.555   100

identical(eval(expr), eval(expr2))
# [1] TRUE
```

## Not supported 
* visibility of the result of a last function
 (pringint the result or not)

## ToDo
* make a wrapper function just like source() or sys.source()
