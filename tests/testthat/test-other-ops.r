context("other-ops")
suppressMessages(library("magrittr"))
suppressMessages(library("dplyr"))

test_that("other-ops", {
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


  # http://qiita.com/uri/items/17c213bf2bca7c95b154
  # check above examples
  e1 <- quote(
    mtcars %>% lm("mpg ~ wt", data = .) %$% {
      message(summary(.))
      #broom::tidy(.) %>% knitr::kable()
      mean(coefficients) %>% base::abs()
    }
  )
  test_demagrittr(e1)

  out_message <- '^lm\\(formula = "mpg ~ wt", data = '
  expect_message(eval(demagrittr(e1, FALSE, mode = "eager")), out_message)
  expect_message(eval(demagrittr(e1, FALSE, mode = "lazy")), out_message)
  expect_message(eval(demagrittr(e1, FALSE, mode = "promise")), out_message)

  e2 <- quote(
    iris %>% dplyr::filter(Species == "setosa") %$% {
      plot(Sepal.Length, Petal.Width)
      mean(Sepal.Length)
      res <<- (.)
    }
  )
  test_demagrittr(e2)

  # compounded pipe %<>%
  e3 <- quote(local({
    iris$Sepal.Length %<>% sqrt
    iris
  }))
  test_demagrittr(e3)

  e4 <- quote(local({
    x <- 1:10
    x[1:5] %<>% sqrt
    x
  }))
  test_demagrittr(e4)

  ## magrittr's vignette
  # tee pipe %T>%
  e5 <- quote({
    set.seed(1)
    rnorm(200) %>%
      matrix(ncol = 2) %T>%
      plot %>% # plot usually does not return anything.
      colSums
  })
  test_demagrittr(e5, lazy = FALSE)
  # The random variables which have different values passed to rhs failes
  # next test.
  #expect_identical(eval(e5), eval(demagrittr(e5, FALSE, as_lazy = TRUE)))

  e51 <- quote(
    1:5 %>%
      {. < 3} %T>%
      str() %>%
      sum() %>%
      log()
  )
  test_demagrittr(e51)

  # dollar pipe %$%
  e6 <- quote(
    iris %>%
      subset(Sepal.Length > mean(Sepal.Length)) %$%
      cor(Sepal.Length, Sepal.Width)
  )
  test_demagrittr(e6)

  e7 <- quote({
    set.seed(1)
    data.frame(z = rnorm(100)) %$%
      ts.plot(z)
  })
  test_demagrittr(e7)

  # last tee op
  e8 <- quote({
    1:5 %>%
      sum() %T>%
      log()
  })
  test_demagrittr(e8)
})
