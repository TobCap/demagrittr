context("nested rhs")
suppressMessages(library("magrittr"))

test_that("equiv value5", {


  e1 <- quote(1:5 %>% sum(., (. + 1) %>% c(1, .)))
  expect_identical(eval(e1), eval(demagrittr(e1, FALSE)))

  e11 <- quote(1:5 %>% sum((. + 1) %>% c(1, .)))
  expect_identical(eval(e11), eval(demagrittr(e11, FALSE)))

  e2 <- quote(1:5 %>% sum(., length((. + 1) %>% c(1, .))))
  expect_identical(eval(e2), eval(demagrittr(e2, FALSE)))

  e3 <- quote(mtcars %>% {
    (.$cyl + .$carb) %>% `*`(., 2) %>% sum
  })
  expect_identical(eval(e3), eval(demagrittr(e3, FALSE)))

  e4 <- quote(1:3 %>% {(. + 10) %>% sum})
  expect_identical(eval(e4), eval(demagrittr(e4, FALSE)))

  e5 <- quote((1:3 %>% `*`(2)) %>% {(. + 10) %>% sum})
  expect_identical(eval(e5), eval(demagrittr(e5, FALSE)))

  e6 <- quote(cars %>% .$speed %>% sum)
  expect_identical(eval(e6), eval(demagrittr(e6, FALSE)))

  e7 <- quote(1:3 %>% sum(., (. - 1) %>% `^`(sum(.))))
  expect_identical(eval(e7), eval(demagrittr(e7, FALSE)))

})
