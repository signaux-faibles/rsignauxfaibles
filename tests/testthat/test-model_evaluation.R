context("Test model evaluation")

test_that("evaluate works as expected on sf_tasks", {
  trained_task <- get_test_task(
    stage = "train",
    learner = mlr3::LearnerClassifFeatureless$new()
  )
  benchmark_result <- evaluate(
    trained_task,
    measures = mlr3::msr("classif.acc"),
    should_remove_strong_signals = FALSE
  )
  expect_is(benchmark_result, "data.table")
  expect_true("classif.acc" %in% names(benchmark_result))
  expect_equal(benchmark_result$classif.acc, 1 / 3)
})

test_that("evaluate works as expected on sf_tasks without strong signals", {
  skip("Test skipped while remove strong signals has not been repaired")
  trained_task <- get_test_task(
    stage = "train",
    learner = mlr3::LearnerClassifFeatureless$new()
  )
  trained_task[["hist_data"]]$time_til_outcome <- c(0:8, NA)
  benchmark_result <- evaluate(
    trained_task,
    measures = mlr3::msr("classif.acc"),
    should_remove_strong_signals = TRUE
  )
  expect_is(benchmark_result, "data.table")
  expect_true("classif.acc" %in% names(benchmark_result))
  expect_equal(benchmark_result$classif.acc, 0)
})

test_that("evaluate works as expected on sf_tasks with cv", {
  trained_task <- get_test_task(
    stage = "train",
    resampling_strategy = "cv",
    learner = mlr3::LearnerClassifFeatureless$new()
  )
  benchmark_result <- evaluate(
    trained_task,
    measures = mlr3::msr("classif.acc"),
    should_remove_strong_signals = FALSE
  )
  expect_is(benchmark_result, "data.table")
  expect_true("classif.acc" %in% names(benchmark_result))
  expect_equal(benchmark_result$classif.acc, 0.1)
})

test_that("evaluate works as expected on sf_tasks with cv without strong signals", {
  skip("Test skipped while remove strong signals has not been repaired")
  trained_task <- get_test_task(
    stage = "train",
    resampling_strategy = "cv",
    learner = mlr3::LearnerClassifFeatureless$new()
  )
  trained_task[["hist_data"]]$time_til_outcome <- c(rep(1, 5), 0, rep(1, 4))
  benchmark_result <- evaluate(
    trained_task,
    measures = mlr3::msr("classif.acc"),
    should_remove_strong_signals = TRUE
  )
  expect_is(benchmark_result, "data.table")
  expect_true("classif.acc" %in% names(benchmark_result))
  expect_equal(benchmark_result$classif.acc, 0)
})

test_that("evaluate works as expected on two sf_tasks", {
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
    measures = mlr3::msr("classif.acc"),
    should_remove_strong_signals = FALSE
  )
  expect_is(benchmark_result, "data.table")
  expect_true("classif.acc" %in% names(benchmark_result))
  expect_equal(benchmark_result$classif.acc, c(1 / 3, 1 / 3))
})


test_that("evaluate works as expected on two sf_tasks without strong signals", {
  skip("Test skipped while remove strong signals has not been repaired")
  trained_task <- get_test_task(
    stage = "train",
    learner = mlr3::LearnerClassifFeatureless$new()
  )
  other_task <- get_test_task(
    stage = "train",
    learner = mlr3::LearnerClassifFeatureless$new()
  )
  trained_task[["hist_data"]]$time_til_outcome <- c(rep(1, 5), rep(0, 5))
  other_task[["hist_data"]]$time_til_outcome <- c(rep(1, 5), rep(0, 5))
  benchmark_result <- evaluate(
    other_task,
    trained_task,
    measures = mlr3::msr("classif.acc"),
    should_remove_strong_signals = TRUE
  )
  expect_is(benchmark_result, "data.table")
  expect_true("classif.acc" %in% names(benchmark_result))
  expect_equal(benchmark_result$classif.acc, c(1, 1))
})

test_that("evaluate works as expected on two sf_tasks with mixed cv and holdout", {
  trained_task <- get_test_task(
    stage = "train",
    resampling_strategy = "cv",
    learner = mlr3::LearnerClassifFeatureless$new()
  )
  other_task <- get_test_task(
    stage = "train",
    learner = mlr3::LearnerClassifFeatureless$new()
  )

  benchmark_result <- evaluate(
    other_task,
    trained_task,
    measures = mlr3::msr("classif.acc"),
    should_remove_strong_signals = FALSE
  )
  expect_is(benchmark_result, "data.table")
  expect_true("classif.acc" %in% names(benchmark_result))
  expect_equal(benchmark_result$classif.acc, c(1 / 3, 1 / 10))
})


test_that("evaluate works as expected with two measures", {
  trained_task <- get_test_task(
    stage = "train",
    learner = mlr3::LearnerClassifFeatureless$new()
  )
  benchmark_result <- evaluate(
    trained_task,
    measures = mlr3::msrs(c("classif.acc", "classif.ce")),
    should_remove_strong_signals = FALSE
  )
  expect_is(benchmark_result, "data.table")
  expect_true("classif.acc" %in% names(benchmark_result))
  expect_equal(benchmark_result$classif.acc, c(1 / 3))
  expect_equal(benchmark_result$classif.ce, c(2 / 3))
})


test_that("evaluate returns a benchmark", {
  test_task <- get_test_task(stage = "evaluate")
  expect_is(test_task, "data.table")
})

test_that("Les logs de la fonction 'evaluate' fonctionnent correctement", {
  task <- get_test_task(stage = "train")
  task[["tracker"]] <- new.env()
  with_mock(
    evaluate(task, should_remove_strong_signals = FALSE),
    log_metric = mock_log_metric,
    log_param = mock_log_param
  )
  expect_true(length(ls(task[["tracker"]])) > 0)
  expect_setequal(
    names(task[["tracker"]]),
    c("classif.ce", "classif.fbeta")
  )
  expect_equal(
    get("classif.ce", envir = task[["tracker"]]),
    2 / 3
  )
  expect_equal(
    get("classif.fbeta", envir = task[["tracker"]]),
    0.5
  )
})


test_that("Les logs de la fonction 'evaluate' fonctionnent correctement avec should_remove_strong_signals", {
  skip("Test skipped while remove strong signals has not been repaired")
  task <- get_test_task(stage = "train")
  task[["tracker"]] <- new.env()
  task[["hist_data"]]$time_til_outcome <- c(rep(1, 5), rep(0, 5))
  with_mock(
    evaluate(task, should_remove_strong_signals = TRUE),
    log_metric = mock_log_metric,
    log_param = mock_log_param
  )
  expect_true(length(ls(task[["tracker"]])) > 0)
  expect_setequal(
    names(task[["tracker"]]),
    c("classif.ce.weaksignals", "classif.fbeta.weaksignals")
  )
  expect_equal(
    get("classif.ce.weaksignals", envir = task[["tracker"]]),
    0
  )
  expect_equal(
    get("classif.fbeta.weaksignals", envir = task[["tracker"]]),
    1
  )
})




test_that("Les logs de la fonction 'evaluate' fonctionnent correctement avec deux tâches", {
  trained_task <- get_test_task(
    stage = "train",
    resampling_strategy = "cv",
    learner = mlr3::LearnerClassifFeatureless$new()
  )
  other_task <- get_test_task(
    stage = "train",
    learner = mlr3::LearnerClassifFeatureless$new()
  )
  trained_task[["tracker"]] <- new.env()
  other_task[["tracker"]] <- new.env()
  with_mock(
    evaluate(trained_task, other_task, should_remove_strong_signals = FALSE),
    log_metric = mock_log_metric,
    log_param = mock_log_param
  )
  expect_true(length(ls(trained_task[["tracker"]])) > 0)
  expect_true(length(ls(other_task[["tracker"]])) > 0)
  expect_setequal(
    names(trained_task[["tracker"]]),
    c("classif.ce", "classif.fbeta")
  )
  expect_setequal(
    names(other_task[["tracker"]]),
    c("classif.ce", "classif.fbeta")
  )

  expect_equal(
    get("classif.ce", envir = trained_task[["tracker"]]),
    0.9
  )
  expect_equal(
    get("classif.fbeta", envir = trained_task[["tracker"]]),
    NaN
  )
  expect_equal(
    get("classif.ce", envir = other_task[["tracker"]]),
    2 / 3
  )
  expect_equal(
    get("classif.fbeta", envir = other_task[["tracker"]]),
    0.5
  )
})
