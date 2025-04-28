
test_that("sa_quality_report generates a report file", {
  data_monthly <- sadashboard::vhi[,"47.3"]
  spec_wd <- RJDemetra::x13_spec("RSA3",tradingdays.option="WorkingDays")
  spec_td <- RJDemetra::x13_spec("RSA3",tradingdays.option="TradingDays")

  model_1 <-  RJDemetra::x13(data_monthly,spec=spec_wd)
  model_2 <-  RJDemetra::x13(data_monthly,spec=spec_td)

  models_in <- list(series1=model_1,series2=model_2)
  series_now <- c("series1", "series2")

  report_file <- tempfile(fileext = ".html")
  sa_quality_report(models_in, report_file)
  expect_true(file.exists(report_file))
})
