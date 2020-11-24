context("Test explain function")

test_that("explain_gam works on a gam trained on splitted data", {
  requireNamespace("mlr3extralearners")
  test_task <- get_test_task(stage = "train", learner = mlr3::lrn("classif.gam"))
  data_to_explain <- head(data.table::as.data.table(test_task$mlr3task$data()))
  explanation <- explain_gam(test_task, data_to_explain)
  expect_equal(explanation, structure(
    c(-4.68669029419069, -0.781115049031782, -7.22531420354398, -2.63626329048226, -6.63947791677015, -5.27252658096453),
    .Dim = c(6L, 1L),
    .Dimnames = list(c("1", "2", "3", "4", "5", "6"), "feature"),
    constant = c(`(Intercept)` = 4.58820931948092)
  ))
})
