context("other than %>%")
suppressMessages(library("magrittr"))
suppressMessages(library("dplyr"))

test_that("equiv value8", {
  # http://qiita.com/uri/items/17c213bf2bca7c95b154
  # check above examples
  e1 <- quote(
    mtcars %>% lm("mpg ~ wt", data = .) %$% {
      print(.)
      #broom::tidy(.) %>% knitr::kable()
      mean(coefficients) %>% base::abs()
    }
  )

  expect_identical(eval(e1), eval(demagrittr(e1, FALSE)))

  e2 <- quote(
    iris %>% dplyr::filter(Species == "setosa") %$% {
      plot(Sepal.Length, Petal.Width)
      mean(Sepal.Length)
      res <<- (.)
    }
  )
  expect_identical(eval(e2), eval(demagrittr(e2, FALSE)))

})
