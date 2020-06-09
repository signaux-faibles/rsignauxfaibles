context("Check model training")

test_train_function  <- function(train_data, outcome, ...) {
  model  <- function(x) return(x)
  return(model)
}

parameters  <- list(
  learn_rate = 0.1,
  max_depth = 4,
  ntrees = 60,
  min_child_weight = 1
)
test_that("train.sf_task works as expected", {
  test_task <- get_test_task()
  test_task[["model_parameters"]] <- parameters
  trained_task <- train(
    task = test_task,
    outcome = "target",
    train_fun = test_train_function
  )
  expect_equal(
    trained_task[["model"]](3),
    3
  )
})

test_that("train.sf_task works with learner as expected", {
  test_task <- get_test_task()
  test_task[["mlr3pipeline"]] <- mlr3pipelines::po("nop")
  test_task[["model_parameters"]] <- list()
  trained_task <- train(
    task = test_task,
    outcome = "target",
    learner = mlr3::lrn("classif.featureless")
  )
  expect_equal(
    trained_task[["mlr3resampled"]]$score(
      mlr3::msr("classif.acc")
    )$classif.acc,
    1 / 3
  )
})

test_that("train.sf_task works with learner and cv as expected ", {
  test_task <- get_test_task(resampling_strategy = "cv")
  test_task[["mlr3pipeline"]] <- mlr3pipelines::po("nop")
  test_task[["model_parameters"]] <- list()
  trained_task <- train(
    task = test_task,
    outcome = "target",
    learner = mlr3::lrn("classif.featureless")
  )
  expect_equal(
    c(classif.acc = 0.1),
    trained_task[["mlr3resampled"]]$aggregate(mlr3::msr("classif.acc")),
  )
})

test_that("processing pipeline is stored in 'mlr3pipeline' property", {
  test_mlr3pipeline_prop <- function(pipe) {
    prep_task <- get_test_task(stage = "prepare", processing_pipeline = pipe)
    expect_true("mlr3pipeline" %in% names(prep_task))
    expect_true(inherits(prep_task[["mlr3pipeline"]], "PipeOp") ||
      inherits(prep_task[["mlr3pipeline"]], "Graph"))
  }
  test_mlr3pipeline_prop(mlr3pipelines::PipeOpNOP$new())
  test_mlr3pipeline_prop(
    mlr3pipelines::as_graph(mlr3pipelines::PipeOpNOP$new())
  )
})


test_that("train.sf_task works with learner as expected", {
  test_task <- get_test_task()
  test_task[["mlr3pipeline"]] <- mlr3pipelines::po("nop")
  test_task[["model_parameters"]] <- list()
  test_task[["mlr3rsmp"]] <- NULL
  trained_task <- train(
    task = test_task,
    outcome = "target",
    learner = mlr3::lrn("classif.featureless")
  )
  expect_equal(
    trained_task[["mlr3model"]]$model$
      classif.featureless$model$tab,
    structure(c(`FALSE` = 5L, `TRUE` = 5L), .Dim = 2L, .Dimnames =
      structure(list(c("FALSE", "TRUE")), .Names = ""), class = "table")
  )
})


test_that("train.cv works as expected", {
  test_task <- get_cv_test_task()
  test_task[["model"]] <- NULL
  test_task[["model_parameters"]] <- parameters
  trained_task <- train(
    task = test_task,
    outcome = "target",
    train_fun = test_train_function
  )
  expect_equal(
    trained_task[["cross_validation"]][[4]][["model"]](3),
    3
  )
})

test_that(
  "Les logs de la fonction 'prepare_data' fonctionnent correctement", {
    task <- get_test_task()
    task[["model_parameters"]] <- parameters
    task[["tracker"]] <- new.env()
    with_mock(
      train(task, outcome = "target", train_fun = test_train_function),
      log_param = mock_log_param,
      log_metric = mock_log_metric
    )
    expect_true(length(ls(task[["tracker"]])) > 0)
    expect_setequal(
      names(task[["tracker"]]),
      c("model_name", "model_target", "model_parameters")
    )
    expect_equal(
      get("model_name", envir = task[["tracker"]]),
      "light gradient boosting"
    )
    expect_equal(
      get("model_target", envir = task[["tracker"]]),
      "18 mois, defaut et defaillance"
    )
    expect_equal(
      get("model_parameters", envir = task[["tracker"]]),
      parameters
      )
  }
)
