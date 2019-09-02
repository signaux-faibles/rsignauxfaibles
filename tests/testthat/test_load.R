context("Test loading functions")

test_task <- get_test_task()
test_task[["database"]] <- "unittest_signauxfaibles"
test_task[["collection"]] <- "Features_for_tests"
test_task[["mongodb_uri"]] <- "mongodb://localhost:27017"

test_task[["hist_data"]] <- NULL
test_task[["new_data"]]  <- NULL


test_that("load_hist_data works as expected", {
  expect_error(
    test_task <- load_hist_data.sf_task(
      test_task,
      batch = "1901_interim"
    ),
    NA
    )

  expect_true("hist_data" %in% names(test_task))
  expect_equal(nrow(test_task[["hist_data"]]), 440)
})


test_that("load_new_data works as expected", {
  expect_error(
    test_task <- load_new_data.sf_task(
      test_task,
      batch = "1901_interim",
      periods = as.Date("2019-01-01")
    ),
    NA
    )
  expect_true("new_data" %in% names(test_task))
  expect_equal(nrow(test_task[["new_data"]]), 31)
})
