## https://github.com/smbache/magrittr/blob/59eae6bffa37a5672b9686ba759bf2ceb1a12e30/inst/doc/magrittr.md
context("test for examples of magrittr's previous readme")
library("magrittr")

testthat::test_that("equiv value2", {
  weekly <- quote(
    airquality %>%
    transform(Date = paste(1973, Month, Day, sep = "-") %>% as.Date) %>%
    aggregate(. ~ Date %>% format("%W"), ., mean)
  )
  ## unname() is required
  expect_identical(unname(eval(weekly)), unname(eval(demagrittr(weekly))))

  windy.weeks <- quote(
    airquality %>%
    transform(Date = paste(1973, Month, Day, sep = "-") %>% as.Date) %>%
    aggregate(. ~ Date %>% format("%W"), ., mean) %>%
    subset(Wind > 12, c(Ozone, Solar.R, Wind))
  )

  expect_identical(unname(eval(windy.weeks)), unname(eval(demagrittr(windy.weeks))))
})

