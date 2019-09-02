#' Log des expérimentations Machine Learning
#'
#' Permet de loguer dans une collection mongodb la nature et les différents
#' éléments de la tâche d'apprentissage. Cf le package `MLlogs`.
#'
#' @inheritParams generic_task
#' @inheritParams mongodb_connection
#' @param collection `character(1)`. La collection vers laquelle exporter. Par
#' défaut, "ml_logs".
#'
#' @return returns the `task` unchanged.
#' @export
log_experiment.sf_task <- function(
  task,
  database = task[["database"]],
  collection = "ml_logs",
  tracker = task[["tracker"]],
  ...
  ){

  requireNamespace("MLlogr")

  assertthat::assert_that(!is.null(tracker))

  tracker$database <- database
  tracker$collection <- collection
  tracker$set(
    model_parameters = task[["model_parameters"]],
    model_features =  task[["features"]],
    test_frame = task[["validation_data"]]
    )
  # No train data as it currently not work with cross validation.
  # e82f5df1-7c7f-472e-b3f8-60707d42136d

  tracker$log(...)

  return(invisible(task))
}
