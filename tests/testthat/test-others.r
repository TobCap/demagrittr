context("test others:")
suppressMessages(library("magrittr"))

test_that("test-others.r", {
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

  e1 <- quote(1 %>% (. %>% exp)())
  test_demagrittr(e1)
})