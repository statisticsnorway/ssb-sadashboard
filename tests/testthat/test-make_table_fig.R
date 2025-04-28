
test_that("make_table_fig works correctly with series_now", {

  view_now <- data.frame(
    Navn = c("A", "B", "C"),
    Sesong = c("Present","Present","Present"),
    M7 = c(0.5, NA, 1.5),
    M10 = c(0.8, 1.2, NA),
    M11 = c(0.9, 1.3, 0.7),
    Q = c(0.6, 1.1, 0.9),
    qs = c(0.05, 0.2, NA),
    fried = c(0.02, 0.15, 0.08),
    f_reg = c(0.03, 0.12, 0.07),
    f_td = c(0.04, 0.11, 0.06)
  )

  series_now <- c("A", "C")
  result <- make_table_fig(view_now, series_now)
  expect_true("kableExtra" %in% class(result))
  result <- make_table_fig(view_now)
  expect_true("kableExtra" %in% class(result))

  view_now <- data.frame(
    Navn = c("A", "B", "C"),
    log = c("yes","yes","no"),
    ARIMA = c(rep("(0,1,1),(0,1,1)",6)),
    outliers = c(1,2,3),
    Td_p = c(0.05, -99, 0.15),
    indRes = c(0.08, 0.12, 0.05),
    ok = c(TRUE, FALSE, TRUE),
    ok_final = c(TRUE, TRUE, FALSE),
    mdl_nr = c(1, 3, 6)
  )
  result <- make_table_fig(view_now, series_now)
  expect_true("kableExtra" %in% class(result))
  result <- make_table_fig(view_now)
  expect_true("kableExtra" %in% class(result))




})

test_that("is.nogood works correctly", {
  expect_true(is.nogood(NA))
  expect_true(is.nogood(NaN))
  expect_error(is.nogood(NULL))
  expect_false(is.nogood(5))
  expect_false(is.nogood("text"))
  expect_false(is.nogood(TRUE))
  expect_false(is.nogood(FALSE))
})
