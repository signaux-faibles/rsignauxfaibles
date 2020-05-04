log_param <- function(key, value, client = NULL, ...) {
  mlflow::mlflow_log_param(key, value, client = client)
}

log_metric <- function(key, value, client = NULL, ...) {
  mlflow::mlflow_log_metric(key, value, client = client, ...)
}
