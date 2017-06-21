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
  expect_identical(eval(e1), eval(demagrittr(e1, FALSE, as_lazy = TRUE)))

  e2 <- quote(
    iris %>% dplyr::filter(Species == "setosa") %$% {
      plot(Sepal.Length, Petal.Width)
      mean(Sepal.Length)
      res <<- (.)
    }
  )
  expect_identical(eval(e2), eval(demagrittr(e2, FALSE)))
  expect_identical(eval(e2), eval(demagrittr(e2, FALSE, as_lazy = TRUE)))

  # compounded pipe %<>%
  e3 <- quote(local({
    iris$Sepal.Length %<>% sqrt
    iris
  }))
  expect_identical(eval(e3), eval(demagrittr(e3, FALSE)))
  expect_identical(eval(e3), eval(demagrittr(e3, FALSE, as_lazy = TRUE)))

  e4 <- quote(local({
    x <- 1:10
    x[1:5] %<>% sqrt
    x
  }))
  expect_identical(eval(e4), eval(demagrittr(e4, FALSE)))
  expect_identical(eval(e4), eval(demagrittr(e4, FALSE, as_lazy = TRUE)))

  ## magrittr's vignette
  # tee pipe %T>%
  e5 <- quote({
    set.seed(1)
    rnorm(200) %>%
      matrix(ncol = 2) %T>%
      plot %>% # plot usually does not return anything.
      colSums
  })
  expect_identical(eval(e5), eval(demagrittr(e5, FALSE)))
  # The random variables which have different values passed to rhs failes
  # next test.
  #expect_identical(eval(e5), eval(demagrittr(e5, FALSE, as_lazy = TRUE)))

  e51 <- quote(
    1:5 %>%
      {. < 3} %T>%
      print() %>%
      sum() %>%
      log()
  )
  expect_identical(eval(e51), eval(demagrittr(e51, FALSE)))
  expect_identical(eval(e51), eval(demagrittr(e51, FALSE, as_lazy = TRUE)))
  # dollar pipe %$%
  e6 <- quote(
    iris %>%
      subset(Sepal.Length > mean(Sepal.Length)) %$%
      cor(Sepal.Length, Sepal.Width)
  )
  expect_identical(eval(e6), eval(demagrittr(e6, FALSE)))
  expect_identical(eval(e6), eval(demagrittr(e6, FALSE, as_lazy = TRUE)))

  e7 <- quote({
    set.seed(1)
    data.frame(z = rnorm(100)) %$%
      ts.plot(z)
  })
  expect_identical(eval(e7), eval(demagrittr(e7, FALSE)))
  expect_identical(eval(e7), eval(demagrittr(e7, FALSE, as_lazy = TRUE)))

})
