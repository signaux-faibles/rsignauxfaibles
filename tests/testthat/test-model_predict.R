context("Test that task prediction works as expected")

test_that("predict works as expected with mlr3", {
   trained_task <- get_test_task(
     stage = "train",
     learner = mlr3::LearnerClassifFeatureless$new()
   )

   trained_task <- predict(trained_task, data_names = "test_data")
   expect_true("prediction_test" %in% names(trained_task))
   expect_is(trained_task$prediction_test, "PredictionClassif")
   expect_equal(
     trained_task$prediction_test %>% data.table::as.data.table() %>% nrow(),
     3
   )
})


test_that("predict works as expected with mlr3, with cv", {
   trained_task <- get_test_task(
     stage = "train",
     resampling_strategy = "cv",
     learner = mlr3::LearnerClassifFeatureless$new()
   )
   trained_task <- predict(trained_task, data_names = "test_data")
   expect_true("prediction_test" %in% names(trained_task))
   expect_is(trained_task$prediction_test, "PredictionClassif")
   expect_equal(
     trained_task$prediction_test %>% data.table::as.data.table() %>% nrow(),
     10
   )
})

test_that("predict works as expected with mlr3", {
   trained_task <- get_test_task(
     stage = "train",
     resampling_strategy = "none",
     learner = mlr3::LearnerClassifFeatureless$new()
   )
   trained_task <- predict(trained_task, data_names = "new_data")
   expect_true("prediction_new" %in% names(trained_task))
   expect_is(trained_task$prediction_new, "PredictionClassif")
})
