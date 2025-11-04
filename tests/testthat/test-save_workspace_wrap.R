
test_that("save_workspace_wrap works for single and multiple SA-processing units", {
  skip_if_not_installed("RJDemetra")
  skip_if_not_installed("sadashboard")
  skip_if_not_installed("pickmdl")

  library(RJDemetra)
  library(sadashboard)
  library(pickmdl)

  # --- Single SA-processing unit ---
  vhi_ts <- sadashboard::vhi
  spec_file_vhi <- make_paramfile(inndat = vhi_ts, spec = "RSA3")
  sa_model_single <- x13_text_frame(spec_file_vhi, series = "vhi_ts")

  wk_file_single <- tempfile(fileext = ".xml")

  expect_silent(
    save_workspace_wrap(models_in = sa_model_single, wk_file = wk_file_single)
  )
  expect_true(file.exists(wk_file_single))
  unlink(wk_file_single)

  # --- Multiple SA-processing units ---
  ledige_ts <- sadashboard::ledige
  spec_file_ledige <- make_paramfile(inndat = ledige_ts, spec = "RSA3")
  sa_model_multi <- list(
    vhi = sa_model_single,
    ledige = x13_text_frame(spec_file_ledige, series = "ledige_ts")
  )

  wk_file_multi <- tempfile(fileext = ".xml")

  expect_silent(
    save_workspace_wrap(models_in = sa_model_multi,
                        wk_file = wk_file_multi,
                        wk_names = c("vhi", "ledige"),
                        multi = TRUE)
  )
  expect_true(file.exists(wk_file_multi))
  unlink(wk_file_multi)
})