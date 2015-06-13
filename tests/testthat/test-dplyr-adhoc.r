context("test for examples of magrittr's vegnettes")
suppressMessages(library("magrittr"))
suppressMessages(library("dplyr"))

testthat::test_that("equiv value3", {
  e1 <- quote(iris %>% filter(Sepal.Width %>% is_greater_than(4.3)))
  expect_identical(eval(e1), eval(demagrittr(e1)))
})
