context("make anonymous fun")
suppressMessages(library("magrittr"))

test_that("equiv value6", {
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
})

