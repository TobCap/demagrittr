context("magrittr readme")
suppressMessages(library("magrittr"))

test_that("equiv value", {
  # https://github.com/tidyverse/magrittr/blob/9c75666e70e97a51fa13a831c7a5ea43134123a8/README.md
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
  test_demagrittr(e1)

  # Tee operations
  e2 <- quote({
    set.seed(1)
    rnorm(200) %>%
      matrix(ncol = 2) %T>%
      plot %>% # plot usually does not return anything.
      colSums
  })
  test_demagrittr(e2, lazy = FALSE)

  # Pipe with exposition of variables
  e3 <- quote({
    iris %>%
      subset(Sepal.Length > mean(Sepal.Length)) %$%
      cor(Sepal.Length, Sepal.Width)
  })
  test_demagrittr(e3)


  # Compound assignment pipe operations
  e4 <- quote({
    if (exists("iris", inherits = FALSE)) rm("iris")
    iris$Sepal.Length %<>% sqrt
    iris
  })
  test_demagrittr(e4)

})
