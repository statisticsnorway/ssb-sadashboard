test_that("make_ts_df works well",{
  myseries <- pickmdl::pickmdl_data("myseries")
  my_model <- RJDemetra::x13(myseries,spec="RSA5c")
  expect_warning(make_ts_df(my_model,cal_adjust=TRUE))
  expect_warning(make_ts_df(my_model,linearized=TRUE))
})
