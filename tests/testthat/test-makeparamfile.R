# Example data (generated as per the function's comment)
set.seed(123)
years <- 2000:2024
ts1 <- runif(length(years), min = 50, max = 150)
ts2 <- runif(length(years), min = 50, max = 150)
ts3 <- runif(length(years), min = 50, max = 150)

inndata <- data.frame(
  year = years,
  tidsserie_1 = ts1,
  tidsserie_2 = ts2,
  tidsserie_3 = ts3
)

# Test 1: Default behavior when no additional parameters are passed
test_that("makeparamfile sets default spec and userdefined", {
  result <- makeparamfile(inndat = inndata)

  # Check that the 'spec' is set to "RSA5c"
  expect_equal(result$spec[1], '\"RSA5c\"')

  # Check that the 'userdefined' column is correctly set
  expected_userdefined <- paste0('c("decomposition.a1", "decomposition.a6", "decomposition.a7", "decomposition.b1", "decomposition.d10", "decomposition.d11", "decomposition.d12", "decomposition.d13", "decomposition.d18", "diagnostics.seas-sa-friedman", "residuals.independence.value")')
  #expected_userdefined_clean <- gsub("\n", "", expected_userdefined)
  #expected_userdefined_cat <- gsub("\\s+", " ", expected_userdefined_clean)
  expect_equal(result$userdefined[1], expected_userdefined)
})

# Test 2: Check that a custom 'spec' and other parameters are passed and set correctly
test_that("makeparamfile accepts and processes custom parameters", {
  result <- makeparamfile(inndat = inndata, spec = "RSA3", identification_end = "c(identaar,12)", usrdef.var = "custom_var")

  # Check that the spec is correctly set to "RSA3"
  expect_equal(result$spec[1], '\"RSA3"')

  # Check that the identification_end is correctly set
  expect_equal(result$identification_end[1], "c(identaar,12)")

  # Check that the usrdef.var is correctly set
  expect_equal(result$usrdef.var[1], "custom_var")
})

# Test 3: Check that warnings are given for unknown parameters
test_that("makeparamfile warns for unknown parameters", {
  # Capture the warning
  expect_warning({
    result <- makeparamfile(inndat = inndata, unknown_param = "unknown_value")
  }, "The parameter unknown_param is included, but is unknown in the list of parameters used in x13_both.")
})

# Test 4: Check that the resulting data frame has the correct structure and data
test_that("makeparamfile generates the correct data frame structure", {
  result <- makeparamfile(inndat = inndata)

  # Check that the number of rows matches the number of time series columns in inndata
  expect_equal(nrow(result), ncol(inndata) - 1) # Exclude 'year' column

  # Check that the columns include 'name' and other parameters like 'spec'
  expect_true("name" %in% colnames(result))
  expect_true("spec" %in% colnames(result))
  expect_true("userdefined" %in% colnames(result))
})

# Test 5: Check that empty input data generates the correct output (edge case)
test_that("makeparamfile handles empty input data", {
  empty_data <- data.frame(year = integer(0))

  result <- makeparamfile(inndat = empty_data)

  # The result should still return a data frame with no rows but correct column names
  expect_equal(nrow(result), 0)
  expect_true("name" %in% colnames(result))
  expect_true("spec" %in% colnames(result))
})

# Test 6: Check when userdefined parameters are not passed
test_that("makeparamfile adds default userdefined when not provided", {
  result <- makeparamfile(inndat = inndata)

  # Check that the default userdefined values are applied
  expected_userdefined <- paste0('c("decomposition.a1", "decomposition.a6", "decomposition.a7", "decomposition.b1", "decomposition.d10", "decomposition.d11", "decomposition.d12", "decomposition.d13", "decomposition.d18", "diagnostics.seas-sa-friedman", "residuals.independence.value")')
  expect_equal(result$userdefined[1], expected_userdefined)
})

# Test 7: Check for handling of logical and numeric inputs (edge cases)
test_that("makeparamfile handles logical and numeric parameters", {
  result <- makeparamfile(inndat = inndata, outlier.ao = TRUE, outlier.cv = 4)

  # Check that logical and numeric parameters are correctly converted to strings
  expect_equal(result$outlier.ao[1], "TRUE")
  expect_equal(result$outlier.cv[1], "4")
})