context("magrittr-vignettes")
suppressMessages(library("magrittr"))

test_that("equiv value", {
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

  # http://cran.r-project.org/web/packages/magrittr/vignettes/magrittr.html
  e1 <- quote({
    car_data <-
      mtcars %>%
      subset(hp > 100) %>%
      aggregate(. ~ cyl, data = ., FUN = . %>% mean %>% round(2)) %>%
      transform(kpl = mpg %>% multiply_by(0.4251))
  })
  test_demagrittr(e1)

  e2 <- quote({
    car_data <-
      mtcars %>%
      subset(hp > 100) %>%
      aggregate(. ~ cyl, data = ., FUN = . %>% mean %>% round(2)) %>%
      transform(kpl = mpg %>% multiply_by(0.4251))

    car_data %>%
      (function(x) {
        if (nrow(x) > 2)
          rbind(head(x, 1), tail(x, 1))
        else x
      })
  })
  test_demagrittr(e2)

  e3 <- quote({
    car_data <-
      mtcars %>%
      subset(hp > 100) %>%
      aggregate(. ~ cyl, data = ., FUN = . %>% mean %>% round(2)) %>%
      transform(kpl = mpg %>% multiply_by(0.4251))

    car_data %>%
    {
      if (nrow(.) > 0)
        rbind(head(., 1), tail(., 1))
      else .
    }
  })
  test_demagrittr(e3)

  e4 <- quote({
    1:10 %>% (substitute(f(), list(f = sum)))
  })
  test_demagrittr(e4)

  e41 <- quote({
    1:5 %>% (call("sum", 100))
  })
  test_demagrittr(e41)

  ### tests for addtional pipe are written in `test-other-ops.r`

  e5 <- quote({
    set.seed(1)
    rnorm(1000)      %>%
      multiply_by(5) %>%
      add(5)         %>%
      {
        #cat("Mean:", mean(.), "Variance:", var(.), "\n")
        head(.)
      }
  })
  test_demagrittr(e5)

  e6 <- quote({
    set.seed(1)
    rnorm(100) %>% `*`(5) %>% `+`(5) %>%
    {
      #cat("Mean:", mean(.), "Variance:", var(.),  "\n")
      head(.)
    }
  })
  test_demagrittr(e6)

})