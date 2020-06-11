context("Test functions to optimize parameters")


test_that("optimize_hyperparameters works as expected", {
  task_to_tune <- get_test_task(stage = "train", learner = lrn("classif.rpart"))

  task_to_tune <- optimize_hyperparameters(
    task_to_tune,
    param_set = paradox::ParamSet$new(
      list(
        paradox::ParamDbl$new("classif.rpart.cp", lower = 0.001, upper = 0.1),
        paradox::ParamInt$new("classif.rpart.minsplit", lower = 1, upper = 10)
      )
    )
  )
  expect_true("mlr3auto_tuner" %in% names(task_to_tune))
})
