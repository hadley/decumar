context("Decumar")

test_that("code weaving works as expected", {
  actual <- process_file("code.tex")
  expected <- str_c(c(readLines("code-out.tex"), ""), collapse = "\n")
  
  expect_that(actual, equals(expected))
})

strip_graphics_path <- function(x) {
  str_replace(x, "\\{[0-9a-f]+\\}", "{}")
}

test_that("figure and graphic weaving works as expected", {
  actual <- strip_graphics_path(process_file("plot.tex"))
  expected <- str_c(c(readLines("plot-out.tex"), ""), collapse = "\n")
  expected <- strip_graphics_path(expected)
  
  expect_that(actual, equals(expected))
})