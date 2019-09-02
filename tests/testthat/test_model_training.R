context("Check H2O model training")

test_train_function  <- function(train_data, outcome, ...){
    model  <- function(x) return(x)
    return(model)
}

test_that("train.sf_task works as expected", {
  test_task <- get_test_task()
  test_task[["model"]] <- NULL
  trained_task <- train(
     task = test_task,
     outcome = "target",
     train_fun = test_train_function
     )
  expect_equal(
    trained_task[["model"]](3),
    3
  )
})


test_that("train.cv works as expected", {
  test_task <- get_cv_test_task()
  test_task[["model"]] <- NULL
  trained_task <- train(
     task = test_task,
     outcome = "target",
     train_fun = test_train_function
     )
  expect_equal(
    trained_task[["cross_validation"]][[4]][["model"]](3),
    3
  )
})
