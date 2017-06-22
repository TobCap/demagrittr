context("make anonymous fun")
suppressMessages(library("magrittr"))

test_that("equiv value6", {
  test_demagrittr <- function(expr, eager = TRUE, lazy = TRUE) {
    stopifnot(is.call(expr))
    pipes <- c("%>%", "%T>%", "%$%", "%<>%")
    eager_eval <- demagrittr(expr, FALSE, as_lazy = FALSE)
    lazy_eval <- demagrittr(expr, FALSE, as_lazy = TRUE)
    if (eager) {
      expect_identical(eval(expr), eval(eager_eval))
      expect_false(any(all.names(eager_eval) %in% pipes))
    }
    if (lazy) {
      expect_identical(eval(expr), eval(lazy_eval))
      expect_false(any(all.names(lazy_eval) %in% pipes))
    }
  }


  e1 <- quote(. %>% add(1) %>% log)
  expect_identical(eval(e1)(1), eval(demagrittr(e1, FALSE))(1))
  expect_identical(eval(e1)(1), eval(demagrittr(e1, FALSE, as_lazy = TRUE))(1))
  expect_identical(eval(e1)(1:10), eval(demagrittr(e1, FALSE))(1:10))
  expect_identical(eval(e1)(1:10), eval(demagrittr(e1, FALSE, as_lazy = TRUE))(1:10))

  e2 <- quote(2 %>% (1 %>% (function(x) function(y) x + y)))
  expect_error(eval(e2))
  expect_identical(3, eval(demagrittr(e2, FALSE)))
  expect_identical(3, eval(demagrittr(e2, FALSE, as_lazy = TRUE)))

  e3 <- quote(2 %>% (1 %>% (function(x) {force(x); function(y) x + y})))
  expect_identical(3, eval(e3))
  expect_identical(3, eval(demagrittr(e3, FALSE)))
  expect_identical(3, eval(demagrittr(e3, FALSE, as_lazy = TRUE)))

  # for a primitive function
  e4 <- quote(1:10 %>% (substitute(f(), list(f = sum))))
  expect_identical(eval(e4), eval(demagrittr(e4, FALSE)))
  expect_identical(eval(e4), eval(demagrittr(e4, FALSE, as_lazy = TRUE)))

  e5 <- quote(1:10 %>% (substitute(f(), list(f = quote(sum)))))
  expect_identical(eval(e5), eval(demagrittr(e5, FALSE)))
  expect_identical(eval(e5), eval(demagrittr(e5, FALSE, as_lazy = TRUE)))

  e6 <- quote(1:10 %>% (substitute(f, list(f = quote(sum(na.rm = TRUE))))))
  expect_identical(eval(e6), eval(demagrittr(e6, FALSE)))
  expect_identical(eval(e6), eval(demagrittr(e6, FALSE, as_lazy = TRUE)))

  # other closures
  e7 <- quote(1:10 %>% (substitute(f(), list(f = mean))))
  expect_identical(eval(e7), eval(demagrittr(e7, FALSE)))
  expect_identical(eval(e7), eval(demagrittr(e7, FALSE, as_lazy = TRUE)))

  e8 <- quote(1:10 %>% (substitute(f(), list(f = quote(mean)))))
  expect_identical(eval(e8), eval(demagrittr(e8, FALSE)))
  expect_identical(eval(e8), eval(demagrittr(e8, FALSE, as_lazy = TRUE)))

  e9 <- quote(1:10 %>% (substitute(f, list(f = quote(mean(na.rm = TRUE))))))
  expect_identical(eval(e9), eval(demagrittr(e9, FALSE)))
  expect_identical(eval(e9), eval(demagrittr(e9, FALSE, as_lazy = TRUE)))


})

