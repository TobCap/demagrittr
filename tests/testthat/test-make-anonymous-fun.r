context("make anonymous fun")
suppressMessages(library("magrittr"))

test_that("equiv value6", {
  e1 <- quote(. %>% add(1) %>% log)
  expect_identical(eval(e1)(1), eval(demagrittr(e1, FALSE))(1))
  expect_identical(eval(e1)(1:10), eval(demagrittr(e1, FALSE))(1:10))
})

