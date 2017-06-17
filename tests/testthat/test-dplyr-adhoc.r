context("adhoc dplyr")
suppressMessages(library("dplyr"))

test_that("equiv value3", {
  e1 <- quote(iris %>% filter(Sepal.Width %>% `>`(4.3)))
  expect_identical(eval(e1), eval(demagrittr(e1, FALSE)))

  e2 <- quote(filter(iris, Sepal.Width %>% `>`(4.3)))
  expect_identical(eval(e2), eval(demagrittr(e2, FALSE)))
})
