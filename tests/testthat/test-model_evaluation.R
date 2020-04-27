# context("Test model evaluation")
#
# test_task  <- get_test_task()
# test_task[["model_performance"]] <- NULL
#
# aux_eval <- function(frame){
#   return(mean( (frame$prediction - frame$target) ^ 2))
# }
# test_eval_function  <- MLsegmentr::eval_function(
#   aux_eval
#   )
#
# test_that("evaluate works as expected on sf_tasks",  {
#   test_task <- evaluate(
#     test_task,
#     eval_function = test_eval_function,
#     remove_strong_signals = FALSE,
#     target_names = "target"
#   )
#   expect_equal(
#     test_task[["model_performance"]]$evaluation,
#     0.3754261,
#     tolerance = 10e-5
#   )
#   # With two tasks to compare
#   test_task[["model_performance"]] <- NULL
#   test_task_2 <- evaluate(
#     test_task,
#     test_task,
#     eval_function = test_eval_function,
#     remove_strong_signals = FALSE,
#     target_names = "target"
#   )
#   expect_equal(
#     test_task_2[["model_performance"]]$evaluation,
#     c(0.3754261, 0.3754261),
#     tolerance = 10e-5
#   )
#   })
#
#
# test_that("join_frames works as expected", {
#   task1  <-  get_test_task()[["train_data"]] %>%
#     rename(model1 = score)
#   task2  <-  get_test_task()[["train_data"]] %>%
#     rename(model2 = score)
#   task3  <-  get_test_task()[["train_data"]] %>%
#     rename(model3 = score)
#   joined <- join_frames(task1, task2, task3)
#   expect_equal(
#     names(joined),
#     c("siret", "periode", "target", "model1", "model2", "model3")
#   )
#   expect_equal(joined$model1, joined$model2)
#   expect_equal(joined$model1, joined$model3)
#   })
#
# test_that("consolidate works as expected", {
#   one_task <- consolidate(
#     test_task = get_test_task(),
#     data_name = "train_data"
#   )
#   two_tasks <- consolidate(
#     test_task1 = get_test_task(),
#     test_task2 = get_test_task(),
#     data_name = "train_data"
#   )
#   one_cv_task <- consolidate(
#     test_task = get_cv_test_task(),
#     data_name = "train_data"
#   )
#   two_cv_tasks <- consolidate(
#     test_task1 = get_cv_test_task(),
#     test_task2 = get_cv_test_task(),
#     data_name = "train_data"
#   )
#   expect_true("Fake.task" %in% names(one_task))
#   expect_true(all(c("Fake.task", "Fake.task.1") %in% names(two_tasks)))
#   expect_true(all(paste0("Fake.task.cv", 1:4) %in% names(one_cv_task)))
#   expect_true(all(paste0("Fake.task.cv", 1:4) %in% names(two_cv_tasks)))
#   expect_true(all(paste0("Fake.task.1.cv", 1:4) %in% names(two_cv_tasks)))
#
#   expect_equal(one_task$Fake.task, two_tasks$Fake.task)
#   expect_equal(two_tasks$Fake.task.1, two_tasks$Fake.task)
#   expect_equal(two_cv_tasks$Fake.task.cv2, two_cv_tasks$Fake.task.1.cv2)
# })
#
# test_that("evaluate works as expected on cv_tasks", {
#   test_task  <- get_cv_test_task()
#   test_task <- evaluate(
#     test_task,
#     eval_function = test_eval_function,
#     remove_strong_signals = FALSE,
#     target_names = "target",
#     data_name = "train_data"
#   )
#   expect_equal(
#     test_task[["model_performance"]]$evaluation,
#     c(0.18873709, 0.18786174, 0.13514012, 0.09863989),
#     tolerance = 10e-5
#   )
#   })
