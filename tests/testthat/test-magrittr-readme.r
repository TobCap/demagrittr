context("magrittr readme")
suppressMessages(library("magrittr"))

test_that("equiv value", {
  # https://github.com/smbache/magrittr/blob/master/README.md

  # More advanced right-hand sides and lambdas
  e1 <- quote({
    set.seed(1)
    iris %>%
      {
        n <- sample(1:10, size = 1)
        H <- head(., n)
        T <- tail(., n)
        rbind(H, T)
      } %>%
      summary
    })
  expect_identical(eval(e1), eval(demagrittr(e1)))

  # Tee operations
  e2 <- quote({
    set.seed(1)
    rnorm(200) %>%
      matrix(ncol = 2) %T>%
      plot %>% # plot usually does not return anything.
      colSums
  })
  expect_identical(eval(e2), eval(demagrittr(e2)))

  # Pipe with exposition of variables
  e3 <- quote({
    iris %>%
      subset(Sepal.Length > mean(Sepal.Length)) %$%
      cor(Sepal.Length, Sepal.Width)
  })
  expect_identical(eval(e3), eval(demagrittr(e3)))

  # Compound assignment pipe operations
  e4 <- quote({
    if (exists("iris")) rm("iris")
    iris$Sepal.Length %<>% sqrt
    iris
  })
  expect_identical(eval(e4), eval(demagrittr(e4)))


})