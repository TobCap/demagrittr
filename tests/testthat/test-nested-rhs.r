context("nested rhs")
suppressMessages(library("magrittr"))

test_that("nested rhs", {
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



  e1 <- quote(1:5 %>% sum(., (. + 1) %>% c(1, .))) # sum(1:5, c(1, 1:5 + 1))
  test_demagrittr(e1)

  e11 <- quote(1:5 %>% sum((. + 1) %>% c(1, .)))
  test_demagrittr(e11)

  e2 <- quote(1:5 %>% sum(., length((. + 1) %>% c(1, .))))
  test_demagrittr(e2)

  e3 <- quote(mtcars %>% {
    (.$cyl + .$carb) %>% `*`(., 2) %>% sum
  })
  test_demagrittr(e3)

  e4 <- quote(1:3 %>% {(. + 10) %>% sum})
  test_demagrittr(e4)

  e5 <- quote((1:3 %>% `*`(2)) %>% {(. + 10) %>% sum})
  test_demagrittr(e5)

  e6 <- quote(cars %>% .$speed %>% sum)
  test_demagrittr(e6)

  e7 <- quote(1:3 %>% sum(., (. - 1) %>% `^`(sum(.))))
  test_demagrittr(e7)

  e8 <- quote(iris %>% filter(Sepal.Width %>% `>`(4.3)))
  test_demagrittr(e1)

  e9 <- quote(filter(iris, Sepal.Width %>% `>`(4.3)))
  test_demagrittr(e2)

  e10 <- quote(1.2345 %>% (. %>% round(2)))
  test_demagrittr(e10)
})
