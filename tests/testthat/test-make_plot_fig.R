
test_that("make_plot_fig returns a list of plotly objects", {

  data_monthly <- sadashboard::vhi[,"47.3"]
  spec_wd <- RJDemetra::x13_spec("RSA3",tradingdays.option="WorkingDays")
  spec_td <- RJDemetra::x13_spec("RSA3",tradingdays.option="TradingDays")

  model_1 <-  RJDemetra::x13(data_monthly,spec=spec_wd)
  model_2 <-  RJDemetra::x13(data_monthly,spec=spec_td)

  models_in <- list(series1=model_1,series2=model_2)
  series_now <- c("series1", "series2")
  result <- make_plot_fig(models_in, series_now)
  expect_true(is.list(result))
  expect_true(all(sapply(result, inherits, "plotly")))

  result <- make_plot_fig(models_in, series_now, plot_start = NULL)
  expect_true(is.list(result))

})


