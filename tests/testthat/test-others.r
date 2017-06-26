context("test others:")
suppressMessages(library("magrittr"))

test_that("test-others.r", {
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

  e1 <- quote(1 %>% (. %>% exp)())
  test_demagrittr(e1)

  e2 <- quote(print %>% .())
  #test_demagrittr(e2)
  out_regexp <- 'function \\(x, \\.\\.\\.\\) \nUseMethod\\(\\"print\\"\\)'
  expect_output(str(eval(demagrittr(e2, FALSE, mode = "eager"))), out_regexp)
  expect_output(str(eval(demagrittr(e2, FALSE, mode = "lazy"))), out_regexp)
  expect_output(str(eval(demagrittr(e2, FALSE, mode = "promise"))), out_regexp)
})