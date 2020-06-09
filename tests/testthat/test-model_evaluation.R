context("Test model evaluation")

test_that("evaluate works as expected on sf_tasks",  {
  requireNamespace("mlr3measures")
  trained_task <- get_test_task(
    stage = "train",
    learner = mlr3::LearnerClassifFeatureless$new()
  )
  benchmark_result <- evaluate(trained_task, measures = msr("classif.acc"))
  expect_is(benchmark_result, "data.table")
  expect_true("classif.acc" %in% names(benchmark_result))
  expect_equal(benchmark_result$classif.acc, 1 / 3)
})


test_that("evaluate works as expected on two sf_tasks",  {
  requireNamespace("mlr3measures")
  trained_task <- get_test_task(
    stage = "train",
    learner = mlr3::LearnerClassifFeatureless$new()
  )
  other_task <- get_test_task(
    stage = "train",
    learner = mlr3::LearnerClassifFeatureless$new()
  )

  benchmark_result <- evaluate(
    other_task,
    trained_task,
    measures = msr("classif.acc")
  )
  expect_is(benchmark_result, "data.table")
  expect_true("classif.acc" %in% names(benchmark_result))
  expect_equal(benchmark_result$classif.acc, c(1 / 3, 1 / 3))
})


test_that("evaluate works as expected with two measures",  {
  requireNamespace("mlr3measures")
  trained_task <- get_test_task(
    stage = "train",
    learner = mlr3::LearnerClassifFeatureless$new()
  )
  benchmark_result <- evaluate(
    trained_task,
    measures = msrs(c("classif.acc", "classif.ce"))
  )
  expect_is(benchmark_result, "data.table")
  expect_true("classif.acc" %in% names(benchmark_result))
  expect_equal(benchmark_result$classif.acc, c(1 / 3))
  expect_equal(benchmark_result$classif.ce, c(2 / 3))
})
