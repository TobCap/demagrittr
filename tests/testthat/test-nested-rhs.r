context("nested rhs")
suppressMessages(library("magrittr"))

test_that("equiv value5", {


  e1 <- quote(1:5 %>% sum(., (. + 1) %>% c(1, .))) # sum(1:5, c(1, 1:5 + 1))
  expect_identical(eval(e1), eval(demagrittr(e1, FALSE)))
  expect_identical(eval(e1), eval(demagrittr(e1, FALSE, as_lazy = TRUE)))

  e11 <- quote(1:5 %>% sum((. + 1) %>% c(1, .)))
  expect_identical(eval(e11), eval(demagrittr(e11, FALSE)))
  expect_identical(eval(e11), eval(demagrittr(e11, FALSE, as_lazy = TRUE)))

  e2 <- quote(1:5 %>% sum(., length((. + 1) %>% c(1, .))))
  expect_identical(eval(e2), eval(demagrittr(e2, FALSE)))
  expect_identical(eval(e2), eval(demagrittr(e2, FALSE, as_lazy = TRUE)))

  e3 <- quote(mtcars %>% {
    (.$cyl + .$carb) %>% `*`(., 2) %>% sum
  })
  expect_identical(eval(e3), eval(demagrittr(e3, FALSE)))
  expect_identical(eval(e3), eval(demagrittr(e3, FALSE, as_lazy = TRUE)))

  e4 <- quote(1:3 %>% {(. + 10) %>% sum})
  expect_identical(eval(e4), eval(demagrittr(e4, FALSE)))
  expect_identical(eval(e4), eval(demagrittr(e4, FALSE, as_lazy = TRUE)))

  e5 <- quote((1:3 %>% `*`(2)) %>% {(. + 10) %>% sum})
  expect_identical(eval(e5), eval(demagrittr(e5, FALSE)))
  expect_identical(eval(e5), eval(demagrittr(e5, FALSE, as_lazy = TRUE)))

  e6 <- quote(cars %>% .$speed %>% sum)
  expect_identical(eval(e6), eval(demagrittr(e6, FALSE)))
  expect_identical(eval(e6), eval(demagrittr(e6, FALSE, as_lazy = TRUE)))

  e7 <- quote(1:3 %>% sum(., (. - 1) %>% `^`(sum(.))))
  expect_identical(eval(e7), eval(demagrittr(e7, FALSE)))
  expect_identical(eval(e7), eval(demagrittr(e7, FALSE, as_lazy = TRUE)))

})
