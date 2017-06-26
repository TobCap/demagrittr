context("make anonymous fun")
suppressMessages(library("magrittr"))

test_that("equiv value6", {
  test_demagrittr <- function(expr, eager = TRUE, lazy = TRUE, promise = TRUE) {
    stopifnot(is.call(expr))
    pipes <- c("%>%", "%T>%", "%$%", "%<>%")
    mode_eager <- demagrittr(expr, FALSE, mode = "eager")
    mode_lazy <- demagrittr(expr, FALSE, mode = "lazy")
    mode_promise <- demagrittr(expr, FALSE, mode = "promise")

    if (eager) {
      expect_identical(eval(expr), eval(mode_eager))
      expect_false(any(all.names(mode_eager) %in% pipes))
    }
    if (lazy) {
      expect_identical(eval(expr), eval(mode_lazy))
      expect_false(any(all.names(mode_lazy) %in% pipes))
    }
    if (promise) {
      expect_identical(eval(expr), eval(mode_promise))
      expect_false(any(all.names(mode_promise) %in% pipes))
    }
  }



  e1 <- quote(. %>% add(1) %>% log)
  expect_identical(eval(e1)(1), eval(demagrittr(e1, FALSE, mode = "eager"))(1))
  expect_identical(eval(e1)(1), eval(demagrittr(e1, FALSE, mode = "lazy"))(1))
  expect_identical(eval(e1)(1), eval(demagrittr(e1, FALSE, mode = "promise"))(1))
  expect_identical(eval(e1)(1:10), eval(demagrittr(e1, FALSE, mode = "eager"))(1:10))
  expect_identical(eval(e1)(1:10), eval(demagrittr(e1, FALSE, mode = "lazy"))(1:10))
  expect_identical(eval(e1)(1:10), eval(demagrittr(e1, FALSE, mode = "promise"))(1:10))

  e2 <- quote(2 %>% (1 %>% (function(x) function(y) x + y)))
  expect_error(eval(e2))
  expect_identical(3, eval(demagrittr(e2, FALSE, mode = "eager")))
  expect_identical(3, eval(demagrittr(e2, FALSE, mode = "lazy")))
  expect_identical(3, eval(demagrittr(e2, FALSE, mode = "promise")))

  e3 <- quote(2 %>% (1 %>% (function(x) {force(x); function(y) x + y})))
  expect_identical(3, eval(e3))
  expect_identical(3, eval(demagrittr(e3, FALSE, mode = "eager")))
  expect_identical(3, eval(demagrittr(e3, FALSE, mode = "lazy")))
  expect_identical(3, eval(demagrittr(e3, FALSE, mode = "promise")))

  # for a primitive function
  e4 <- quote(1:10 %>% (substitute(f(), list(f = sum))))
  test_demagrittr(e4)

  e5 <- quote(1:10 %>% (substitute(f(), list(f = quote(sum)))))
  test_demagrittr(e5)

  e6 <- quote(1:10 %>% (substitute(f, list(f = quote(sum(na.rm = TRUE))))))
  test_demagrittr(e6)

  # other closures
  e7 <- quote(1:10 %>% (substitute(f(), list(f = mean))))
  test_demagrittr(e7)

  e8 <- quote(1:10 %>% (substitute(f(), list(f = quote(mean)))))
  test_demagrittr(e8)

  e9 <- quote(1:10 %>% (substitute(f, list(f = quote(mean(na.rm = TRUE))))))
  test_demagrittr(e9)

})

