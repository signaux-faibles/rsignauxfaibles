context("Test that task prediction works as expected")

test_predict_fun <- function(model, new_data) {
  return(rep(1, nrow(new_data)))
}

test_that("predict.sf_task works as expected", {
  test_task <- get_test_task()
  test_task[["model"]] <- list()
  predicted_task <- predict(
    test_task,
    data_names = "test_data",
    predict_fun = test_predict_fun
  )
  pred_data <- predicted_task[["test_data"]]
  expect_equal(pred_data$score, rep(1, nrow(pred_data)))
})

test_that("predict.cv_task works as expected", {
  test_task <- get_cv_test_task()
  test_task[["cross_validation"]] <- purrr::map(
    test_task[["cross_validation"]],
    function(x) {
      x[["model"]] <- list()
      return(x)
    }
  )
  predicted_task <- predict(
    test_task,
    data_names = "test_data",
    predict_fun = test_predict_fun
  )
  pred_data <- predicted_task[["cross_validation"]][[3]][["test_data"]]
  expect_equal(pred_data$score, rep(1, nrow(pred_data)))
})
