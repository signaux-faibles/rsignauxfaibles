context("Text log functions")

test_that(
  "Log functions write to mlflow", {

   # mlflow should be of same version than R "mlflow" library.
   # Use mlflow::install_mlflow() if not.
   testthat::skip_on_ci()
   tdir <- tempdir(check = TRUE)
   task <- get_test_task()
   mlflow::mlflow_set_tracking_uri(file.path("file:/", tdir, "mlruns"))
   run <- mlflow::mlflow_start_run()
   tryCatch({
     task[["tracker"]] <- run
     log_param(task, "a", "text")
     log_metric(task, "b", 6)
   }, finally = {
     mlflow::mlflow_end_run()
   })
   expect_equal(
     mlflow::mlflow_get_run(run$run_id)$params[[1]]$key,
     "a"
     )
   expect_equal(
     mlflow::mlflow_get_run(run$run_id)$params[[1]]$value,
     "text"
     )
   expect_equal(
     mlflow::mlflow_get_run(run$run_id)$metrics[[1]]$key,
     "b"
     )
   expect_equal(
     mlflow::mlflow_get_run(run$run_id)$metrics[[1]]$value,
     6
     )
  }
)
