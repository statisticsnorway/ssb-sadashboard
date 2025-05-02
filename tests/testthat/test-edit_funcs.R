# Tests for functions in edit_funcs.R for editing constraints files

test_that("read_yaml_constraints reads YAML file correctly", {
  # Define the path to the test YAML file
  yaml_path <- system.file("tests","testthat","test-vhi.yaml", package = "sadashboard")

  # Call the function
  result <- read_yaml_constraints(yaml_path)

  # Define and check some results of different types
  expect_true(all(result$name == c("Serie1", "Serie2")))
  expect_true(all(result$outlier.ao == c("TRUE", "TRUE")))
  expect_true(all(result$transform.function == c('"Log"', '"Log"')))

})

