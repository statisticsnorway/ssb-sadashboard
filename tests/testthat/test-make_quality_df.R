test_that("make_quality_df works well", {

  data_monthly <- sadashboard::vhi[,"47"]
  data_quarterly <- sadashboard::ledige[,"01-03"]
  spec_td <- RJDemetra::x13_spec("RSA3",tradingdays.option="TradingDays")
  spec_wd <- RJDemetra::x13_spec("RSA3",tradingdays.option="WorkingDays")
  model_td <- pickmdl::x13_pickmdl(data_monthly,spec=spec_td)
  model_wd <- pickmdl::x13_pickmdl(data_monthly,spec=spec_wd)

  models_now <-  list(model_td)
  names(models_now) <- "model_td"

  expect_warning(make_quality_df(models_now),"Friedman-test and/or independence of residuals not included. NA inserted.")

  models_now <- list(model_td,model_wd)
  expect_error(make_quality_df(models_now))
})
