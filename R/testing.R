#' Get a fake task
#'
#' @param fake_data Data that is stored as "hist_data". If none is provided,
#'   some data will be automatically set.
#' @param stage `character(1)` \cr Either "load", "split" or "prepare". The
#'   output task will be at the end of the specified stage.
#'
#' @export
get_test_task <- function(
  fake_data = NULL,
  fake_target = "target",
  training_fields = "feature",
  stage = "prepare",
  resampling_strategy = "holdout",
  processing_pipeline = NULL,
  learner = NULL,
  measures = NULL
) {

  assertthat::assert_that(
    stage %in% c("load", "split", "prepare", "train"),
    msg = "Stage should be either 'load', 'split' or 'prepare'"
    )

  if (is.null(fake_data)) {
    fake_data <- data.frame(
      siret = as.character(1:10),
      periode = as.POSIXct(
        seq(
          from = as.Date("2014-01-01"),
          to = as.Date("2014-10-01"),
          length.out = 10
        )
        ),
      target = rep(c(T, F), length.out = 10),
      feature = c(0.48, 0.08, 0.74, 0.27, 0.68, 0.54, 0.54, 0.61, 0.5, 0.69)
    )
  }

  task <- sf_task(
    mongodb_uri = "fake_uri",
    database = "fake_database",
    collection = "fake_collection",
    id = "Fake task",
    target = fake_target,
    verbose = FALSE
  )

  mock_query_database <- function(...) {
    return(fake_data)
  }

  task <-  load_hist_data(
    task,
    batch = "0000",
    fields = names(fake_data),
    database_query_fun = mock_query_database
  )

  task <- load_new_data(
    task,
    periods = as.Date("2014-10-01"),
    batch = "0000",
    fields = names(fake_data),
    database_query_fun = mock_query_database
    )

  if (stage == "load") {
    return(task)
  }

  task  <- split_data(
    task,
    resampling_strategy = resampling_strategy
  )

  if (stage == "split") {
    return(task)
  }

  if (!is.null(processing_pipeline)) {
  task <- prepare(
    task,
    training_fields = training_fields,
    processing_pipeline = processing_pipeline
    )
  } else {
    # TEMP temporary
    fake_preparation_map_function <- function(data_to_prepare, options) {
      return(1)
    }

    fake_prepare_function  <- function(data_to_prepare, options) {
      return(data_to_prepare)
    }

    shape_identity <- function(x, options) {
      return(x)
    }
    task <- prepare(
      task,
      outcome_field = fake_target,
      preparation_map_function = fake_preparation_map_function,
      prepare_function = fake_prepare_function,
      shape_frame_function = shape_identity,
      training_fields = training_fields
    )
    # END TEMP
  }
  if (stage == "prepare") {
    return(task)
  }

  task[["model_parameters"]] <- list()
  task <- train(task, learner = learner)

  return(task)
}


get_cv_test_task <- function() {
  sf_task <- get_test_task()

  cv_task <-  sf_task[c(
    "name",
    "database",
    "collection",
    "mongodb_uri",
    "tracker",
    "hist_data",
    "new_data"
    )]

  class(cv_task) <- c("cv_task", "sf_task")
  cv_task[["cross_validation"]] <- rep(list(get_test_task()), 4)
  cv_task[["cross_validation"]] <- purrr::map2(
    cv_task[["cross_validation"]],
    1:4,
    function(task, holdout) {
      task[["train_data"]] <- task[["train_data"]][-holdout, ]
      return(task)
    })
  return(cv_task)
}

# Pass an environment to client and it will assign new variables in it.
mock_log_param <- function(task, key, value, ...) {
  assertthat::assert_that(inherits(task[["tracker"]], "environment"))
  assign(key, value, envir = task[["tracker"]])
}

# Pass an environment to client and it will assign new variables in it.
mock_log_metric <- function(task, key, value, ...) {
  assertthat::assert_that(inherits(task[["tracker"]], "environment"))
  assign(key, value, envir = task[["tracker"]])
}
