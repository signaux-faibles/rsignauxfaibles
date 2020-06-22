context("Test preparation functions")

test_task <- get_test_task(stage = "split")

create_prepared_task <- function(
  test_task,
  processing_pipeline = mlr3pipelines::PipeOpNOP$new(),
  target = "target"
) {

  prepared_task <- prepare(
    test_task,
    data_names = c("train_data", "test_data"),
    outcome_field = target,
    training_fields = c("feature"),
    processing_pipeline = processing_pipeline
  )
  return(prepared_task)
}

test_that("no error is thrown with a valid 'processing_pipeline'", {
  expect_error(
    get_test_task(
      stage = "prepare",
      processing_pipeline = mlr3pipelines::PipeOpNOP$new()
    ),
  NA
  )
})

test_that("no error is thrown with a valid 'processing_pipeline' with cv", {
  expect_error(
    get_test_task(
      stage = "prepare",
      resampling_strategy = "cv",
      processing_pipeline = mlr3pipelines::PipeOpNOP$new()
    ),
  NA
  )
})

fake_pipe <- mlr3pipelines::PipeOpScale$new(
  param_vals = list(affect_columns = mlr3pipelines::selector_name("feature"))
)
fake_pipe <- mlr3pipelines::as_graph(fake_pipe)

test_that("'processing_pipeline' is correctly applied", {
  prep_task <- get_test_task(
    stage = "prepare",
    processing_pipeline = fake_pipe
  )
  test_mean_sd <- function(mean_or_sd, expected, test_or_train) {
    expect_equal(
      mean_or_sd(
        get_prepared_data(prep_task, test_or_train)$feature
        ),
      expected,
      tolerance = 10e-3
    )
  }

  # train_data
  test_mean_sd(mean, 0, "train")
  test_mean_sd(sd, 1, "train")
  # test_data
  test_mean_sd(mean, 0.346, "test")
  test_mean_sd(sd, 0.46, "test")
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

test_that("processing pipeline is stored in 'mlr3pipeline' property with cv", {
  test_mlr3pipeline_prop <- function(pipe) {
    prep_task <- get_test_task(
      stage = "prepare",
      resampling_strategy = "cv",
      processing_pipeline = pipe
    )
    expect_true("mlr3pipeline" %in% names(prep_task))
    expect_true(inherits(prep_task[["mlr3pipeline"]], "PipeOp") ||
      inherits(prep_task[["mlr3pipeline"]], "Graph"))
  }
  test_mlr3pipeline_prop(mlr3pipelines::PipeOpNOP$new())
  test_mlr3pipeline_prop(
    mlr3pipelines::as_graph(mlr3pipelines::PipeOpNOP$new())
  )
})

test_that("prepare changes the requested features", {
  test_features <- function(pipe, features, mlr_features = features) {
    prepared_task <- get_test_task(
      stage = "prepare",
      processing_pipeline = pipe,
      training_fields = features
    )
    expect_equal(prepared_task[["training_fields"]], features)
    expect_equal(prepared_task[["mlr3task"]]$col_roles$feature, mlr_features)
    # new features are added when training
  }
  test_features(mlr3pipelines::PipeOpNOP$new(), "feature")
  mutate_po <- mlr3pipelines::PipeOpMutate$new()
  mutate_po$param_set$values$mutation <- list(new_feature = ~ feature ^ 2)
  test_features(
    mutate_po,
    features = "feature"
  )
})

test_that("prepare changes the outcome field if requested", {
  prepared_task <- create_prepared_task(test_task)
  testthat::expect_equal(
    prepared_task[["outcome_field"]],
    "target"
  )
  testthat::expect_equal(
    prepared_task[["mlr3task"]]$col_roles$target,
    "target"
  )
  prepared_task2 <- create_prepared_task(
    test_task,
    target =  "periode"
  )
  testthat::expect_equal(
    prepared_task2[["outcome_field"]],
    "periode"
  )
  testthat::expect_equal(
    prepared_task2[["mlr3task"]]$col_roles$target,
    "periode"
  )
})

create_fte_test_task <- function(processing_pipeline = NULL) {
  test_task <- get_test_task(stage = "load")
  new_data <- test_task[["hist_data"]] %>%
    cbind(
      ab = c("a", "b", "a", "a", "a", "b", "b", "a", "b", "a"),
      cd = c("d", "d", "c", "c", "d", "d", "c", "c", "c", "d"),
      outcome = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE) # nolint
    )

  prepared_task <- get_test_task(
    fake_data = new_data,
    fake_target = "outcome",
    stage = "prepare",
    processing_pipeline = processing_pipeline,
    training_fields = c("feature", "ab", "cd")
  )
  return(prepared_task)
}

test_that("fte state works as expected", {
  testthat::skip_on_ci()
  prep_task <- create_fte_test_task(create_fte_pipeline(c("ab", "cd")))

  expect_known_hash(
    get_prepared_data(prep_task, "test"),
    "40734c2555"
    )
})

test_that("preparing twice with different training fields takes effect", {
  prep_task <- create_fte_test_task(mlr3pipelines::PipeOpNOP$new())

  check_names <- function(names){
    expect_setequal(
      names(get_prepared_data(prep_task, "test")),
      names
    )
  }
  check_names(c("outcome", "feature", "ab", "cd"))
  prep_task <- prepare(
    prep_task,
    processing_pipeline = mlr3pipelines::PipeOpNOP$new(),
    training_fields = "feature"
    )
  check_names(c("outcome", "feature"))
  prep_task <- prepare(
    prep_task,
    processing_pipeline = mlr3pipelines::PipeOpNOP$new(),
    training_fields = c("feature", "ab", "cd")
    )
  check_names(c("outcome", "feature", "ab", "cd"))
})
