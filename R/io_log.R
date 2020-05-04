log_param <- function(task, key, value, ...) {
  if (!is.null(task[["tracker"]]) && requireNamespace("mlflow")) {
    mlflow::mlflow_log_param(key, value, client = task[["tracker"]])
  }
}

log_metric <- function(task, key, value, ...) {
  if (!is.null(task[["tracker"]]) && requireNamespace("mlflow")) {
    mlflow::mlflow_log_metric(key, value, client = task[["tracker"]], ...)
  }
}
