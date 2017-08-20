context("closure transformation test")
suppressMessages(library("dplyr"))

test_that("closure-transformation", {
  e <- new.env()
  e$f <- evalq(function(x) x %>% identity, envir = e)
  f_e <- demagrittr(e$f, is_NSE = FALSE, mode = "e")
  f_p <- demagrittr(e$f, is_NSE = FALSE, mode = "p")
  f_l <- demagrittr(e$f, is_NSE = FALSE, mode = "l")

  expect_identical(e$f(0L), f_e(0L))
  expect_identical(e$f(0L), f_p(0L))
  expect_identical(e$f(0L), f_l(0L))
  expect_identical(e, environment(f_e))
  expect_identical(e, environment(f_p))
  expect_identical(e, environment(f_l))
  expect_equal(body(f_e), quote({`#0`<-x;identity(`#0`)}))
  expect_equal(body(f_p), quote((function(`#0`) identity(`#0`))(x)))
  expect_equal(body(f_l), quote(identity(x)))

})
