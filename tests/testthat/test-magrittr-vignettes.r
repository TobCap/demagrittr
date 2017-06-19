context("magrittr vignettes")
suppressMessages(library("magrittr"))

test_that("equiv value", {
  # http://cran.r-project.org/web/packages/magrittr/vignettes/magrittr.html
  e1 <- quote({
    car_data <-
      mtcars %>%
      subset(hp > 100) %>%
      aggregate(. ~ cyl, data = ., FUN = . %>% mean %>% round(2)) %>%
      transform(kpl = mpg %>% multiply_by(0.4251))
  })
  expect_identical(eval(e1), eval(demagrittr(e1, FALSE)))
  expect_identical(eval(e1), eval(demagrittr(e1, FALSE, as_lazy = TRUE)))

  e2 <- quote({
    car_data %>%
      (function(x) {
        if (nrow(x) > 2)
          rbind(head(x, 1), tail(x, 1))
        else x
      })
  })
  expect_identical(eval(e2), eval(demagrittr(e2, FALSE)))
  expect_identical(eval(e2), eval(demagrittr(e2, FALSE, as_lazy = TRUE)))

  e3 <- quote({
    car_data %>%
    {
      if (nrow(.) > 0)
        rbind(head(., 1), tail(., 1))
      else .
    }
  })
  expect_identical(eval(e3), eval(demagrittr(e3, FALSE)))
  expect_identical(eval(e3), eval(demagrittr(e3, FALSE, as_lazy = TRUE)))

  e4 <- quote({
    1:10 %>% (substitute(f(), list(f = sum)))
  })
  expect_identical(eval(e4), eval(demagrittr(e4, FALSE)))
  expect_error(eval(demagrittr(e4, FALSE, as_lazy = TRUE)))

  e41 <- quote({
    1:5 %>% (call("sum", 100))
  })
  expect_identical(eval(e41), eval(demagrittr(e41, FALSE)))
  expect_error(eval(demagrittr(e41, FALSE, as_lazy = TRUE)))

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
  expect_identical(eval(e5), eval(demagrittr(e5, FALSE)))
  expect_identical(eval(e5), eval(demagrittr(e5, FALSE, as_lazy = TRUE)))

  e6 <- quote({
    set.seed(1)
    rnorm(100) %>% `*`(5) %>% `+`(5) %>%
    {
      #cat("Mean:", mean(.), "Variance:", var(.),  "\n")
      head(.)
    }
  })
  expect_identical(eval(e6), eval(demagrittr(e6, FALSE)))
  expect_identical(eval(e6), eval(demagrittr(e6, FALSE, as_lazy = TRUE)))


})