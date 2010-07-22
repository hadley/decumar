context("texweave")

test_that("Simple weaving accomplished correctly", {
  woven <- texweave(evaluate(file("weave.r")))
  cache <- str_c(c(readLines("weave.rout"), ""), collapse = "\n")
  
  expect_that(woven, equals(cache))
})