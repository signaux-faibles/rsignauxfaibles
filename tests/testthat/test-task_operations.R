context("Test task cloning and filtering")

test_that("reset_for_new_run makes a new task", {
  task <- get_test_task(stage = "train")
  new_task <- reset_for_new_run(task)
  expect_true(all(
    c("mlr3task", "mlr3rsmp", "hist_data", "new_data", "tracker") %in%
      names(new_task)
  ))
  expect_true(!"mlr3resample_result" %in% names(new_task))

  new_task2 <- reset_for_new_run(task, keep_resampling = FALSE)
  expect_true(!"mlr3rsmp" %in% names(new_task2))
})

test_that("reset_for_new_run does not copy data", {
  task <- get_test_task(stage = "train")
  task$hist_data <- data.frame(periode = 1, siret = 1:100000, target = c(T, F))
  new_task <- reset_for_new_run(task)
  expect_true(pryr::object_size(c(task, new_task)) < 11000000)
})

test_that("replace_hist_data works as expected", {
  task <- get_test_task(stage = "split")
  mlr3task_copy <- task$mlr3task$clone()
  task$mlr3task$filter(1) # mess-up with task
  repaired_task <- replace_hist_data(task, task$hist_data)
  expect_equal(mlr3task_copy$data(), repaired_task$mlr3task$data())
})

test_that("replace_new_data works as expected", {
  task <- get_test_task(stage = "split")
  expect_error(
    new_task <- replace_new_data(task, task$hist_data),
    NA
  )
})

test_that("filter_task works as expected", {
  task <- get_test_task(stage = "train")
  task <- filter_task(task, c(3, 4))
  expect_equal(as.character(task$hist_data$siret), c("3", "4"))
  expect_equal(
    nrow(data.table::as.data.table(task$mlr3task)),
    2
  )
  task <- filter_task(task, c(3, 4)) # Can be repeated
  expect_equal(as.character(task$hist_data$siret), c("3", "4"))
  expect_equal(
    nrow(data.table::as.data.table(task$mlr3task)),
    2
  )
})
