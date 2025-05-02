test_that("make_quality_df works well", {

  data_monthly <- sadashboard::vhi[,"47.3"]
  data_quarterly <- sadashboard::ledige[,"01-03"]
  spec_td <- RJDemetra::x13_spec("RSA3",tradingdays.option="TradingDays")
  spec_wd <- RJDemetra::x13_spec("RSA3",tradingdays.option="WorkingDays")
  model_td <- pickmdl::x13_pickmdl(data_monthly,spec=spec_td)
  model_wd <- pickmdl::x13_pickmdl(data_monthly,spec=spec_wd)

  models_now <-  list(model_td)
  names(models_now) <- "model_td"

  expect_warning(make_quality_df(models_now),"Friedman-test and/or independence of residuals not included. NA inserted.")

  models_now <- list(model_td,model_wd)
  expect_warning(expect_error(make_quality_df(models_now)))

  names(models_now) <- c("model_td","model_wd")
  tables_now <- suppressWarnings(make_quality_df(models_now))
  expect_equal(nrow(tables_now[[1]]),2)
  expect_equal(ncol(tables_now[[1]]),10)
  expect_equal(nrow(tables_now[[2]]),2)
  expect_equal(ncol(tables_now[[2]]),9)

  model_td <- pickmdl::x13_pickmdl(data_quarterly,spec=spec_td)
  model_wd <- pickmdl::x13_pickmdl(data_quarterly,spec=spec_wd)

  models_now <-  list(model_td)
  names(models_now) <- "model_td"

  expect_warning(make_quality_df(models_now),"Friedman-test and/or independence of residuals not included. NA inserted.")

  models_now <- list(model_td,model_wd)
  expect_warning(expect_error(make_quality_df(models_now)))

  names(models_now) <- c("model_td","model_wd")
  tables_now <- suppressWarnings(make_quality_df(models_now))
  expect_equal(nrow(tables_now[[1]]),2)
  expect_equal(ncol(tables_now[[1]]),10)
  expect_equal(nrow(tables_now[[2]]),2)
  expect_equal(ncol(tables_now[[2]]),9)

})
