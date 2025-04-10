# Define some sample functions to use in tests
func1 <- function(a, b, ...) {}
func2 <- function(x, y, z) {}
func3 <- function(p, q) {}
func4 <- function(...) {}

test_that("combine_param_names returns correct parameter names", {
  # Test with two functions having distinct parameter names
  result <- combine_param_names(func1, func2)
  expect_equal(result, c("a", "b", "x", "y", "z"))

  # Test with one function having no parameters
  result <- combine_param_names(func3, func4)
  expect_equal(result, c("p", "q"))

  # Test with overlapping parameter names
  func5 <- function(a, b, x) {}
  result <- combine_param_names(func1, func5)
  expect_equal(result, c("a", "b", "x"))

  # Test with empty parameter names
  func6 <- function() {}
  result <- combine_param_names(func6, func4)
  expect_equal(result, character(0))
})

test_that("combine_param_names handles '...' correctly", {
  # Test with '...' in parameter names
  result <- combine_param_names(func1, func4)
  expect_equal(result, c("a", "b"))
})