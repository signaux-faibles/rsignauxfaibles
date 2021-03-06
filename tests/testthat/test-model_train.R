context("Check model training")

test_train_function <- function(train_data, outcome, ...) {
  model <- function(x) {
    return(x)
  }
  return(model)
}

parameters <- list(
  learn_rate = 0.1,
  max_depth = 4,
  ntrees = 60,
  min_child_weight = 1
)

test_that("train.sf_task works with learner as expected", {
  test_task <- get_test_task()
  test_task[["mlr3pipeline"]] <- mlr3pipelines::po("nop")
  trained_task <- train(
    task = test_task,
    outcome = "target",
    learner = mlr3::lrn("classif.featureless")
  )
  expect_equal(
    trained_task[["mlr3resample_result"]]$score(
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
    trained_task[["mlr3resample_result"]]$aggregate(mlr3::msr("classif.acc")),
  )
})

test_that("train.sf_task returns a task", {
  test_task <- get_test_task(stage = "train")
  expect_is(test_task, "sf_task")
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
  test_task[["mlr3rsmp"]] <- NULL
  trained_task <- train(
    task = test_task,
    outcome = "target",
    learner = mlr3::lrn("classif.featureless")
  )
  expect_equal(
    trained_task[["mlr3model"]]$model$
      classif.featureless$model$tab,
    structure(c(`TRUE` = 5L, `FALSE` = 5L),
      .Dim = 2L, .Dimnames =
        structure(list(c("TRUE", "FALSE")), .Names = ""), class = "table"
    )
  )
})

test_that(
  "Les logs de la fonction 'train_data' fonctionnent correctement",
  {
    task <- get_test_task(
      processing_pipeline = mlr3pipelines::po(
        "scale",
        param_vals = list(center = FALSE)
      )
    )
    task[["tracker"]] <- new.env()

    with_mock(
      train(task, learner = mlr3::LearnerClassifFeatureless$new()),
      log_param = mock_log_param,
      log_metric = mock_log_metric
    )
    expect_true(length(ls(task[["tracker"]])) > 0)
    expect_true(all(
      c(
        "model_target",
        "scale.center",
        "classif.featureless.method",
        "pipeline1"
      ) %in%
        names(task[["tracker"]])
    ))
    expect_equal(
      get("pipeline1", envir = task[["tracker"]]),
      "scale.classif.featureless"
    )
    expect_equal(
      get("classif.featureless.method", envir = task[["tracker"]]),
      "mode"
    )
    expect_equal(
      get("scale.center", envir = task[["tracker"]]),
      "FALSE"
    )
    expect_equal(
      get("model_target", envir = task[["tracker"]]),
      "18 mois, defaut et defaillance"
    )
  }
)

test_that("gam gives the same result applied directly or within mlr3", {
  requireNamespace("data.table")
  task <- mlr3::tsk("pima")
  learner <- get_gam_learner()
  learner$train(task)
  prediction_mlr3 <- data.table::as.data.table(learner$predict(task))$prob.neg

  requireNamespace("mgcv")
  pima_data <- as.data.frame(task$data())
  model <- mgcv::gam(
    diabetes ~ s(age) + s(glucose) + s(insulin) + s(mass) +
      s(pedigree) + s(pregnant) + s(pressure) + s(triceps),
    family = binomial(link = "logit"),
    data = pima_data
    )
  # Prédiction de "neg", car c'est le deuxième niveau du facteur
  prediction_gam <- as.double(predict(model, pima_data, type = "response"))
  expect_equal(prediction_mlr3, prediction_gam)
})
