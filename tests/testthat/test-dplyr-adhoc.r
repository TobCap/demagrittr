context("adhoc dplyr")
suppressMessages(library("magrittr"))
suppressMessages(library("dplyr"))

test_that("equiv value3", {
  e1 <- quote(iris %>% filter(Sepal.Width %>% is_greater_than(4.3)))
  expect_identical(eval(e1), eval(demagrittr(e1)))

  e2 <- quote(filter(iris, Sepal.Width %>% is_greater_than(4.3)))
  expect_identical(eval(e2), eval(demagrittr(e2)))
})