context("test for examples of magrittr's vegnettes")
library("magrittr")

testthat::test_that("equiv value", {
  # http://cran.r-project.org/web/packages/magrittr/vignettes/magrittr.html
  e1 <- quote({
    car_data <-
      mtcars %>%
      subset(hp > 100) %>%
      aggregate(. ~ cyl, data = ., FUN = . %>% mean %>% round(2)) %>%
      transform(kpl = mpg %>% multiply_by(0.4251))
  })
  expect_identical(eval(e1), eval(demagrittr(e1)))

  e2 <- quote({
    car_data %>%
      (function(x) {
        if (nrow(x) > 2)
          rbind(head(x, 1), tail(x, 1))
        else x
      })
  })
  expect_identical(eval(e2), eval(demagrittr(e2)))

  e3 <- quote({
    car_data %>%
    {
      if (nrow(.) > 0)
        rbind(head(., 1), tail(., 1))
      else .
    }
  })
  expect_identical(eval(e3), eval(demagrittr(e3)))

  e4 <- quote({
    1:10 %>% (substitute(f(), list(f = sum)))
  })
  expect_identical(eval(e4), eval(demagrittr(e4)))

  e5 <- quote({
    set.seed(1)
    rnorm(1000)      %>%
      multiply_by(5) %>%
      add(5)         %>%
      {
        cat("Mean:", mean(.), "Variance:", var(.), "\n")
        head(.)
      }
  })
  expect_identical(eval(e5), eval(demagrittr(e5)))

  e6 <- quote({
    set.seed(1)
    rnorm(100) %>% `*`(5) %>% `+`(5) %>%
    {
      cat("Mean:", mean(.), "Variance:", var(.),  "\n")
      head(.)
    }
  })
  expect_identical(eval(e6), eval(demagrittr(e6)))


})