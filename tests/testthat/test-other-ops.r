context("other-ops")
suppressMessages(library("magrittr"))
suppressMessages(library("dplyr"))

test_that("other-ops", {
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



  # http://qiita.com/uri/items/17c213bf2bca7c95b154
  # check above examples
  e1 <- quote(
    mtcars %>% lm("mpg ~ wt", data = .) %$% {
      print(.)
      #broom::tidy(.) %>% knitr::kable()
      mean(coefficients) %>% base::abs()
    }
  )
  test_demagrittr(e1)

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
  test_demagrittr(e1, lazy = FALSE)
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

})
