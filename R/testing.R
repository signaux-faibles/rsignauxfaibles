get_test_task <- function(seed = 1793) {

  task <- sf_task(
    mongodb_uri = "fake_uri",
    database = "fake_database",
    collection = "fake_collection",
    id = "Fake task",
    target = "target",
    verbose = FALSE
  )
  testthat::with_mock(
    query_database = function(...) {
      fake_df <- data.frame(
        siret = as.character(1:10),
        periode = as.POSIXct(
          seq(
            from = as.Date("2014-01-01"),
            to = as.Date("2014-10-01"),
            length.out = 10
          )
          ),
        target = rep(c(T, F), length.out = 10),
        feature = stats::runif(10)
      )
    },
    task <-  load_hist_data(task, batch = "0000", fields = c("siret", "periode", "target", "feature"))
  )

  task  <- split_data(task)
  task[["new_data"]]  <- task[["hist_data"]]
  task[["prepared_train_data"]]  <- task[["train_data"]]
  task[["prepared_test_data"]]  <- task[["test_data"]]
  task[["prepared_validation_data"]]  <- task[["validation_data"]]
  task[["outcome_field"]] <- "target"

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
