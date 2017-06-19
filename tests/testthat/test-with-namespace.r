context("explicit usage of namespace function with '::' or ':::'")
suppressMessages(library("magrittr"))

test_that("equiv value7", {
  e1 <- quote(Nile %>% base::mean) # error
  e2 <- quote(Nile %>% base::mean()) # run
  e3 <- quote(Nile %>% base::mean(.)) # run

  expect_error(eval(demagrittr(e1, FALSE)))
  expect_identical(eval(e2), eval(demagrittr(e2, FALSE)))
  expect_identical(eval(e3), eval(demagrittr(e3, FALSE)))

  expect_error(eval(demagrittr(e1, FALSE, as_lazy = TRUE)))
  expect_identical(eval(e2), eval(demagrittr(e2, FALSE, as_lazy = TRUE)))
  expect_identical(eval(e3), eval(demagrittr(e3, FALSE, as_lazy = TRUE)))
})