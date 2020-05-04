#' Logs a parameter
#'
#' @inheritParams generic_task
#' @param key `character(1)` The key of the parameter to log.
#' @param value `any(1)` The value of the parameter to log.
#'
#' Logs a parameter if the property "tracker" of `task` is not NULL.
#' Currently uses task[["tracker"]] as a client for mlflow.
#'
#' @export
log_param <- function(task, key, value, ...) {
  if (!is.null(task[["tracker"]]) && requireNamespace("mlflow")) {
    mlflow::mlflow_log_param(key, value, client = task[["tracker"]])
  }
}

#' Logs a metric
#'
#' @inheritParams generic_task
#' @param key `character(1)` The key of the metric to log.
#' @param value `any(1)` The value of the metric to log.
#'
#' Logs a metric if the property "tracker" of `task` is not NULL.
#' Currently uses task[["tracker"]] as a client for mlflow.
#'
#' @export
log_metric <- function(task, key, value, ...) {
  if (!is.null(task[["tracker"]]) && requireNamespace("mlflow")) {
    mlflow::mlflow_log_metric(key, value, client = task[["tracker"]], ...)
  }
}
