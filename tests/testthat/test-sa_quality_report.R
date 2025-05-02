
test_that("sa_quality_report generates a report file", {

  data_monthly <- sadashboard::vhi[,"47.3"]
  data_quarterly <- sadashboard::ledige[,"01-03"]

  spec_td <- RJDemetra::x13_spec("RSA3",tradingdays.option="TradingDays")
  spec_wd <- RJDemetra::x13_spec("RSA3",tradingdays.option="WorkingDays")
  model_td <- pickmdl::x13_pickmdl(data_monthly,spec=spec_td,pickmdl_method="first_tryautomdl",
                                                 userdefined = c("decomposition.a8","decomposition.b1",
                                                          "diagnostics.seas-sa-friedman","residuals.independence.value"))
  model_wd <- pickmdl::x13_pickmdl(data_monthly,spec=spec_wd,pickmdl_method="first_tryautomdl",
                                                 userdefined = c("decomposition.a8","decomposition.b1",
                                                          "diagnostics.seas-sa-friedman","residuals.independence.value"))

  models_in <- list(series1=model_td,series2=model_wd)
  series_now <- c("series1", "series2")

  report_file <- tempfile(fileext = ".html")
  sa_quality_report(models_in, report_file)
  expect_true(file.exists(report_file))
})
