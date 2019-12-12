context("Test preparation functions")

test_task <- get_test_task()

test_task[["prepared_train_data"]] <- NULL
test_task[["prepared_test_data"]] <- NULL
test_task[["prepared_validation_data"]] <- NULL
test_task[["preparation_map"]] <- NULL

fake_prepare_train_frame  <- function(data_to_prepare, ...){
  offset <- 1
  prepared_data <- data_to_prepare %>%
    dplyr::mutate(target = target + offset)
  return(list(data = prepared_data, preparation_map = offset))
}

fake_prepare_test_frame  <- function(data_to_prepare, preparation_map, ...){
  offset <- preparation_map
  prepared_data <- data_to_prepare %>%
    dplyr::mutate(target = target + offset)
  return(data = prepared_data)
}

test_that("Prepare task works as expected", {
  prepared_task <- prepare(
    test_task,
    prepare_train_function = fake_prepare_train_frame,
    prepare_test_function = fake_prepare_test_frame,
    outcome_field = "target"
  )

  expect_true(all(
      c("prepared_train_data", "prepared_test_data", "prepared_validation_data",
      "preparation_map") %in% names(prepared_task)
      ))
  expect_equal(prepared_task[["preparation_map"]], 1)
  expect_equal(
    prepared_task[["prepared_train_data"]]$target,
    prepared_task[["train_data"]]$target + 1
  )
  expect_equal(
    prepared_task[["prepared_validation_data"]]$target,
    prepared_task[["validation_data"]]$target + 1
  )
  expect_equal(
    prepared_task[["prepared_test_data"]]$target,
    prepared_task[["test_data"]]$target + 1
  )
})

fake_prepare_train_frame_2  <- function(data_to_prepare, training_fields, ...){
  prepared_data <- dplyr::select(data_to_prepare, training_fields)
  return(list(data = prepared_data, preparation_map = list()))
}

fake_prepare_test_frame_2  <- function(data_to_prepare, training_fields, ...){
  prepared_data <- dplyr::select(data_to_prepare, training_fields)
  return(data = prepared_data)
}

test_that("Training_fields works as expected", {
  prepared_task <- prepare(
    test_task,
    prepare_train_function = fake_prepare_train_frame_2,
    prepare_test_function = fake_prepare_test_frame_2,
    training_fields = c("target")
  )
  expect_equal(ncol(prepared_task[["prepared_validation_data"]]), 1)
  expect_equal(ncol(prepared_task[["prepared_train_data"]]), 1)
})


test_that("transformation_map_create and apply work as expected", {
   test_task <- get_test_task()
   test_data  <- test_task[["validation_data"]] %>%
     select(score, target)
   transformation_map <- transformation_map_create(test_data)
   res_data <- transformation_map_apply(transformation_map, test_data)
   expect_equal(res_data[[1]], transformation_map[[1]][["x.t"]])
   expect_equal(res_data[[2]], transformation_map[[2]][["x.t"]])
})
