context("Test explain function")

test_that("explain_gam works on a gam trained on splitted data", {
  test_task <- get_test_task(stage = "train", learner = mlr3::lrn("classif.gam"))
  data_to_explain <- head(data.table::as.data.table(test_task$mlr3task$data()))
  explain_gam(test_task, data_to_explain)
})
