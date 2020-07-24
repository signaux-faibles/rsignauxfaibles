#' Makes a task for
#'
#' @param param
#'
#' @return
#' @export
#'
#' @examples
reset_for_new_run <- function(
  task,
  end_run_status = NULL,
  keep_resampling = TRUE
) {

  new_task <- sf_task(
    mongodb_uri = task$mongodb_uri,
    database = task$database,
    collection = task$collection,
    id = task$id,
    target = task$target,
    tracker = reset_tracker(task$tracker, end_run_status)
    )

  new_task$hist_data <- task$hist_data
  new_task$new_data <- task$new_data
  new_task$mlr3task <- task$mlr3task
  if (keep_resampling) {
    new_task$mlr3rsmp <- task$mlr3rsmp
  }

  return(new_task)
}

reset_tracker <- function(tracker, end_run_status) {
  if (class(tracker) == "mlflow_run") {
     if (!is.null(end_run_status)) {
       mlflow::mlflow_end_run(end_run_status)
     }
     return(mlflow::mlflow_start_run(experiment_id = tracker$experiment_id))
  }
  return(NULL)
}

replace_hist_data <- function(task, data) {
   task <- load_hist_data(
     task,
     batch = NULL,
    fields = names(data),
     database_query_fun = mock_query_database(data)
   )
   return(task)
}

replace_new_data <- function(task, data) {
  task <- load_new_data(
    task,
    periods = as.Date("2020-01-01"),
    batch = NULL,
    fields = names(data),
    database_query_fun = mock_query_database(data)
  )
  return(task)
}

filter_task <- function(sirets) {


}
