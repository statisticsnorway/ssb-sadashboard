
test_that("save_workspace_wrap works for single and multiple SA-processing units", {
  skip_if_not_installed("RJDemetra")
  skip_if_not_installed("sadashboard")
  skip_if_not_installed("pickmdl")

  library(RJDemetra)
  library(sadashboard)
  library(pickmdl)

  # --- Single SA-processing unit ---
  sa_model_single <- list(vhi=RJDemetra::x13(sadashboard::vhi[,1],spec="RSA3"))


  wk_file_single <- tempfile(fileext = ".xml")

  expect_silent(
    save_workspace_wrap(models_in = sa_model_single, wk_file = wk_file_single)
  )
  expect_true(file.exists(wk_file_single))
  unlink(wk_file_single)

  # --- Multiple SA-processing units ---

  sa_model_multi <- list(
    list(vhi=RJDemetra::x13(sadashboard::vhi[,1],spec="RSA3")),
    list(ledige=RJDemetra::x13(sadashboard::ledige[,1],spec="RSA3"))
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