#' Get a fake task
#'
#' @param fake_data Data that is stored as "hist_data". If none is provided,
#'   some data will be automatically set.
#' @param fake_target `character(1)` name of the column that should be use as
#' target for the learning.
#' @param training_fields `character()` name of the feature columns to learn
#' from
#' @param stage `character(1)` \cr Either "load", "split", "prepare", "train"
#' and "evaluate". The output task will be at the end of the specified stage.
#' @param resampling_strategy `character(1)` Either "holdout" or "cv". The
#' resampling strategy of the "train" stage.
#' @param processing_pipeline `mlr3pipelines::PipeOp` A preparation pipeline
#' used for the "prepare" stage.
#' @param learner `mlr3::Learner` A learner used at the "train" stage.
#' @param measures `mlr3::Measure` A list of measures used at the "evaluate"
#' stage.
#'
#' @return An sf_task, at the specified "stage"
#' @export
get_test_task <- function(
                          fake_data = NULL,
                          fake_target = "target",
                          training_fields = "feature",
                          stage = "prepare",
                          resampling_strategy = "holdout",
                          processing_pipeline = mlr3pipelines::PipeOpNOP$new(),
                          learner = mlr3::LearnerClassifFeatureless$new(),
                          measures = mlr3::msr("classif.acc")) {
  previous_threshold <- lgr::lgr$threshold
  lgr::lgr$set_threshold("error")

  admissible_stages <- c(
    "load",
    "split",
    "prepare",
    "train",
    "evaluate"
  )
  assertthat::assert_that(
    stage %in% admissible_stages,
    msg = paste0(
      "Stage should be either ",
      paste0(admissible_stages, collapse = ", ")
    )
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
    target = fake_target
  )

  task <- load_hist_data(
    task,
    batch = "0000",
    fields = names(fake_data),
    database_query_fun = mock_query_database(fake_data)
  )

  task <- load_new_data(
    task,
    periods = as.Date("2014-10-01"),
    batch = "0000",
    fields = names(fake_data),
    database_query_fun = mock_query_database(fake_data)
  )

  if (stage == "load") {
    return(task)
  }

  task <- split_data(
    task,
    resampling_strategy = resampling_strategy
  )

  if (stage == "split") {
    return(task)
  }

  task <- prepare(
    task,
    training_fields = training_fields,
    processing_pipeline = processing_pipeline
  )

  if (stage == "prepare") {
    return(task)
  }

  task <- train(task, learner = learner)

  if (stage == "train") {
    return(task)
  }

  task <- evaluate(task, measures = measures, should_remove_strong_signals = FALSE)

  return(task)

  lgr::lgr$set_threshold(previous_threshold)
}

# Pass an environment to client and it will assign new variables in it.
mock_log_param <- function(task, key, value, ...) {
  assertthat::assert_that(inherits(task[["tracker"]], "environment"))
  assign(key, value, envir = task[["tracker"]])
}

mock_log_metric <- mock_log_param
