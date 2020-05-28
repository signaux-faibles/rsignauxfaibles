context("Test preparation functions")

test_task <- get_test_task(stage = "split")

fake_preparation_map_function <- function(data_to_prepare, options) {
  if (!is.null(options[["offset"]])) {
    return(options[["offset"]])
  }
  return(1)
}

fake_prepare_function  <- function(data_to_prepare, options) {
  if (!is.null(options[["cancel_offset"]])) {
    return(data_to_prepare)
  }

prepared_data <- data_to_prepare %>%
    dplyr::mutate(feature = feature + options[["PREPARATION_MAP"]])
  return(prepared_data)
}

matrix_or_identity <- function(x, options) {
  if (!is.null(options[["as_matrix"]])) {
    return(as.matrix(x))
  }
  return(x)
}

create_prepared_task <- function(test_task,
  preparation_map_options = list(),
  prepare_options = list(),
  shape_frame_options = list(),
  target = NULL,
  processing_pipeline = NULL
  ) {

  prepared_task <- prepare(
    test_task,
    data_names = c("train_data", "test_data"),
    outcome_field = target,
    preparation_map_function = fake_preparation_map_function,
    prepare_function = fake_prepare_function,
    shape_frame_function = matrix_or_identity,
    preparation_map_options = preparation_map_options,
    prepare_options = prepare_options,
    shape_frame_options = shape_frame_options,
    training_fields = c("feature"),
    processing_pipeline = processing_pipeline
  )
  return(prepared_task)
}


TestPipeOp <- R6::R6Class("TestPipeOp", #nolint
  inherit = mlr3pipelines::PipeOpTaskPreprocSimple,
  public = list(
    initialize = function(id = "test", ...) {
      super$initialize(id, feature_types = "numeric", ...)
    }
    ),
  private = list(
    .get_state_dt = function(dt, levels, target) {
      list(offset = 1)
    },
    .transform_dt = function(dt, levels) {
        dt[,
          (names(dt)) := purrr::map(.SD, ~ . + self$state$offset)
        ]
    }
  )
)

fake_pipeline <- TestPipeOp$new(
  param_vals = list(affect_columns = mlr3pipelines::selector_name("feature"))
)
fake_pipeline <- mlr3pipelines::as_graph(fake_pipeline)

test_that("Prepare task works with mlr3pipeline as expected", {
  prepared_task <- create_prepared_task(
    test_task,
    processing_pipeline = fake_pipeline
  )
  expect_true(all(
      c("prepared_train_data", "prepared_test_data") %in%
        names(prepared_task)
      ))
  expect_equal(
    prepared_task[["prepared_train_data"]]$feature,
    prepared_task[["train_data"]]$feature + 1
  )
  expect_equal(
    prepared_task[["prepared_test_data"]]$feature,
    prepared_task[["test_data"]]$feature + 1
  )
  })

test_that("prepare filters the requested features", {
  prepared_task <- create_prepared_task(test_task)
  testthat::expect_equal(
    prepared_task[["training_fields"]],
    "feature"
    )
  testthat::expect_equal(
    prepared_task[["mlr3task"]]$col_roles$feature,
    "feature"
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

test_that("Prepare task works with options as expected", {
  offset2_task <- create_prepared_task(
    test_task,
    preparation_map_options = list(offset = 2)
  )

  no_offset_task <- create_prepared_task(
    test_task,
    prepare_options = list(cancel_offset = TRUE)
  )

  matrix_task <- create_prepared_task(
    test_task,
    shape_frame_options = list(as_matrix = TRUE)
  )

  expect_equal(offset2_task[["preparation_map"]], 2)
  expect_equal(
    no_offset_task[["prepared_train_data"]]$feature,
    no_offset_task[["train_data"]]$feature
  )
  expect_true(inherits(matrix_task[["prepared_test_data"]], "matrix"))
  })

create_fte_test_task <- function(processing_pipeline = NULL) {
  test_task <- get_test_task(stage = "load")
  new_data <- test_task[["hist_data"]] %>%
    cbind(
      ab = c("a", "b", "a", "a", "a", "b", "b", "a", "b", "a"),
      cd = c("d", "d", "c", "c", "d", "d", "c", "c", "c", "d"),
      outcome = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE)
      )

  prepared_task <- get_test_task(
    fake_data = new_data,
    fake_target = "outcome",
    stage = "split"
  )
  prepared_task <- prepare(
    prepared_task,
    data_names = c("train_data", "test_data"),
    preparation_map_options = list(
      outcome_field = NULL,
      target_encode_fields = c("ab", "cd")
      ),
    prepare_options = list(
      outcome_field = "outcome",
      target_encode_fields = c("ab", "cd")
      ),
    training_fields = c("feature", "target_encode_ab",
      "target_encode_cd"),
    processing_pipeline = processing_pipeline
  )
  return(prepared_task)
}

test_that("map creation works as expected", {
  testthat::skip_on_ci()
  prep_task <- create_fte_test_task()
  expect_known_hash(
    prep_task[["prepared_test_data"]],
    "787aefe2fa"
  )
})

test_that("fte state works as expected", {
  testthat::skip_on_ci()
  prep_task <- create_fte_test_task(
    create_fte_pipeline(c("ab", "cd"))
  )
  expect_known_hash(
    prep_task[["prepared_test_data"]],
    "3c90fa4ae4"
    )
})

test_that(
  "Les logs de la fonction 'prepare_data' fonctionnent correctement", {
    test_task <- get_test_task(stage = "split")
    test_task[["tracker"]] <- new.env()
    with_mock(
      create_prepared_task(test_task),
      log_param = mock_log_param,
      log_metric = mock_log_metric
    )
    expect_true(length(ls(test_task[["tracker"]])) > 0)
    expect_setequal(
      names(test_task[["tracker"]]),
      c("preprocessing_strategy")
    )
    expect_equal(
      get("preprocessing_strategy", envir = test_task[["tracker"]]),
      "Target encoding with fte"
    )
  }
)
