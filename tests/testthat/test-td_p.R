test_that("td_p works for monthly and quarterly data ", {

  data_monthly <- sadashboard::vhi[,"47"]
  data_quarterly <- sadashboard::ledige[,"01-03"]

  spec_wd <- RJDemetra::x13_spec("RSA3",tradingdays.option="WorkingDays")
  spec_td <- RJDemetra::x13_spec("RSA3",tradingdays.option="TradingDays")
  spec_no <- RJDemetra::x13_spec("RSA3",tradingdays.option="None")

  model_month_wd <-  RJDemetra::x13(data_monthly,spec=spec_wd)
  model_month_td <-  RJDemetra::x13(data_monthly,spec=spec_td)
  model_month_no <-  RJDemetra::x13(data_monthly,spec=spec_no)

  model_quarter_wd <-  RJDemetra::x13(data_quarterly,spec=spec_wd)
  model_quarter_td <-  RJDemetra::x13(data_quarterly,spec=spec_td)
  model_quarter_no <-  RJDemetra::x13(data_quarterly,spec=spec_no)

  expect_lte(td_p(model_month_wd),1)
  expect_gte(td_p(model_month_wd),0)
  expect_lte(td_p(model_month_td),1)
  expect_gte(td_p(model_month_td),0)
  expect_equal(td_p(model_month_no),NA)
  expect_lte(td_p(model_quarter_wd),1)
  expect_gte(td_p(model_quarter_wd),0)
  expect_lte(td_p(model_quarter_td),1)
  expect_gte(td_p(model_quarter_td),0)
  expect_equal(td_p(model_quarter_no),NA)

  td6 <- pickmdl::konstruksjon(forste_ar = 2000,siste_ar=2050,k_td= TRUE, td_type="TD6")
  td5 <- pickmdl::konstruksjon(forste_ar = 2000,siste_ar=2050,k_td= TRUE, td_type="TD5")
  td16 <- pickmdl::konstruksjon(forste_ar = 2000,siste_ar=2050,k_td= TRUE, td_type="TD16")
  td15 <- pickmdl::konstruksjon(forste_ar = 2000,siste_ar=2050,k_td= TRUE, td_type="TD15")
  tdg1 <- pickmdl::konstruksjon(forste_ar = 2000,siste_ar=2050,k_td= TRUE, k_grupper=TRUE,monster=rep(1,6))
  tdgx <- pickmdl::konstruksjon(forste_ar = 2000,siste_ar=2050,k_td= TRUE, k_grupper=TRUE,monster=c(1,1,2,3,4,4))

  td6_mnd <- ts(td6$samle_mnd,freq=12,start=c(2000,1))[,-1]
  td5_mnd <- ts(td5$samle_mnd,freq=12,start=c(2000,1))[,-1]
  td16_mnd <- ts(td16$samle_mnd,freq=12,start=c(2000,1))[,-1]
  td15_mnd <- ts(td15$samle_mnd,freq=12,start=c(2000,1))[,-1]
  tdg1_mnd <- ts(tdg1$samle_mnd,freq=12,start=c(2000,1))[,-1]
  tdgx_mnd <- ts(tdgx$samle_mnd,freq=12,start=c(2000,1))[,-1]

  spec_6mnd <- RJDemetra::x13_spec("RSA3",usrdef.varEnabled = TRUE,usrdef.varType = "Calendar",usrdef.var=td6_mnd)
  spec_5mnd <- RJDemetra::x13_spec("RSA3",usrdef.varEnabled = TRUE,usrdef.varType = "Calendar",usrdef.var=td5_mnd)
  spec_16mnd <- RJDemetra::x13_spec("RSA3",usrdef.varEnabled = TRUE,usrdef.varType = "Calendar",usrdef.var=td16_mnd)
  spec_15mnd <- RJDemetra::x13_spec("RSA3",usrdef.varEnabled = TRUE,usrdef.varType = "Calendar",usrdef.var=td15_mnd)
  spec_g1mnd <- RJDemetra::x13_spec("RSA3",usrdef.varEnabled = TRUE,usrdef.varType = "Calendar",usrdef.var=tdg1_mnd)
  spec_gxmnd <- RJDemetra::x13_spec("RSA3",usrdef.varEnabled = TRUE,usrdef.varType = "Calendar",usrdef.var=tdgx_mnd)

  model_month_6 <-  RJDemetra::x13(data_monthly,spec=spec_6mnd)
  model_month_5 <-  RJDemetra::x13(data_monthly,spec=spec_5mnd)
  model_month_16 <-  RJDemetra::x13(data_monthly,spec=spec_16mnd)
  model_month_15 <-  RJDemetra::x13(data_monthly,spec=spec_15mnd)
  model_month_g1 <-  RJDemetra::x13(data_monthly,spec=spec_g1mnd)
  model_month_gx <-  RJDemetra::x13(data_monthly,spec=spec_gxmnd)

  expect_lte(td_p(model_month_6),1)
  expect_gte(td_p(model_month_6),0)
  expect_lte(td_p(model_month_5),1)
  expect_gte(td_p(model_month_5),0)
  expect_lte(td_p(model_month_16),1)
  expect_gte(td_p(model_month_16),0)
  expect_lte(td_p(model_month_15),1)
  expect_gte(td_p(model_month_15),0)
  expect_lte(td_p(model_month_g1),1)
  expect_gte(td_p(model_month_g1),0)
  expect_lte(td_p(model_month_gx),1)
  expect_gte(td_p(model_month_gx),0)

  td6_kv <- ts(td6$samle_kv,freq=4,start=c(2000,1))[,-1]
  td5_kv <- ts(td6$samle_kv,freq=4,start=c(2000,1))[,-1]
  td16_kv <- ts(td6$samle_kv,freq=4,start=c(2000,1))[,-1]
  td15_kv <- ts(td6$samle_kv,freq=4,start=c(2000,1))[,-1]
  tdg1_kv <- ts(td6$samle_kv,freq=4,start=c(2000,1))[,-1]
  tdgx_kv <- ts(td6$samle_kv,freq=4,start=c(2000,1))[,-1]

  spec_6kv <- RJDemetra::x13_spec("RSA3",usrdef.varEnabled = TRUE,usrdef.varType = "Calendar",usrdef.var=td6_kv)
  spec_5kv <- RJDemetra::x13_spec("RSA3",usrdef.varEnabled = TRUE,usrdef.varType = "Calendar",usrdef.var=td5_kv)
  spec_16kv <- RJDemetra::x13_spec("RSA3",usrdef.varEnabled = TRUE,usrdef.varType = "Calendar",usrdef.var=td16_kv)
  spec_15kv <- RJDemetra::x13_spec("RSA3",usrdef.varEnabled = TRUE,usrdef.varType = "Calendar",usrdef.var=td15_kv)
  spec_g1kv <- RJDemetra::x13_spec("RSA3",usrdef.varEnabled = TRUE,usrdef.varType = "Calendar",usrdef.var=tdg1_kv)
  spec_gxkv <- RJDemetra::x13_spec("RSA3",usrdef.varEnabled = TRUE,usrdef.varType = "Calendar",usrdef.var=tdgx_kv)

  model_kv_6 <-  RJDemetra::x13(data_quarterly,spec=spec_6kv)
  model_kv_5 <-  RJDemetra::x13(data_quarterly,spec=spec_5kv)
  model_kv_16 <-  RJDemetra::x13(data_quarterly,spec=spec_16kv)
  model_kv_15 <-  RJDemetra::x13(data_quarterly,spec=spec_15kv)
  model_kv_g1 <-  RJDemetra::x13(data_quarterly,spec=spec_g1kv)
  model_kv_gx <-  RJDemetra::x13(data_quarterly,spec=spec_gxkv)

  expect_lte(td_p(model_kv_6),1)
  expect_gte(td_p(model_kv_6),0)
  expect_lte(td_p(model_kv_5),1)
  expect_gte(td_p(model_kv_5),0)
  expect_lte(td_p(model_kv_16),1)
  expect_gte(td_p(model_kv_16),0)
  expect_lte(td_p(model_kv_15),1)
  expect_gte(td_p(model_kv_15),0)
  expect_lte(td_p(model_kv_g1),1)
  expect_gte(td_p(model_kv_g1),0)
  expect_lte(td_p(model_kv_gx),1)
  expect_gte(td_p(model_kv_gx),0)
})
