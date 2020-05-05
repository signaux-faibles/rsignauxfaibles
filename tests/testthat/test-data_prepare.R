 context("Test preparation functions")

test_task <- get_test_task()


test_task[["prepared_train_data"]] <- NULL
test_task[["prepared_test_data"]] <- NULL
test_task[["prepared_validation_data"]] <- NULL
test_task[["preparation_map"]] <- NULL

fake_preparation_map_function <- function(data_to_prepare, options) {
  if (!is.null(options[["offset"]])) {
    return(options[["offset"]])
  }
  return(1)
}

fake_prepare_function  <- function(data_to_prepare, options) {
  if (!is.null(options[["cancel_offset"]])) {
    return(data_to_prepare)
  }

  prepared_data <- data_to_prepare %>%
    dplyr::mutate(target = target + options[["PREPARATION_MAP"]])
  return(prepared_data)
}

matrix_or_identity <- function(x, options) {
  if (!is.null(options[["as_matrix"]])) {
    return(as.matrix(x))
  }
  return(x)
}

create_prepared_task <- function(test_task,
  preparation_map_options = list(),
  prepare_options = list(),
  shape_frame_options = list()
  ) {
  prepared_task <- prepare(
    test_task,
    preparation_map_function = fake_preparation_map_function,
    prepare_function = fake_prepare_function,
    shape_frame_function = matrix_or_identity,
    preparation_map_options = preparation_map_options,
    prepare_options = prepare_options,
    shape_frame_options = shape_frame_options,
    training_fields = c("target", "score")
  )
  return(prepared_task)
}

test_that("Prepare task works as expected", {
  prepared_task <- create_prepared_task(test_task)
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

test_that("Prepare task works with options as expected", {
  offset2_task <- create_prepared_task(
    test_task,
    preparation_map_options = list(offset = 2)
  )

  no_offset_task <- create_prepared_task(
    test_task,
    prepare_options = list(cancel_offset = TRUE)
  )

  matrix_task <- create_prepared_task(
    test_task,
    shape_frame_options = list(as_matrix = TRUE)
  )

  expect_equal(offset2_task[["preparation_map"]], 2)
  expect_equal(
    no_offset_task[["prepared_train_data"]]$target,
    no_offset_task[["train_data"]]$target
  )
  expect_true(inherits(matrix_task[["prepared_test_data"]], "matrix"))
  })

create_fte_test_task <- function() {
  set.seed(1)
  test_task <- get_test_task()

  test_task[["prepared_train_data"]] <- NULL
  test_task[["prepared_test_data"]] <- NULL
  test_task[["prepared_validation_data"]] <- NULL
  test_task[["preparation_map"]] <- NULL
  test_task[["validation_data"]] <- cbind(test_task[["validation_data"]],
    ab = sample(c("a", "b"), nrow(test_task[["validation_data"]]), replace =
      TRUE),
    cd = sample(c("c", "d"), nrow(test_task[["validation_data"]]), replace =
      TRUE),
    outcome = sample(
      c(TRUE, FALSE),
      nrow(test_task[["validation_data"]]),
      replace = TRUE
    )
  )
  set.seed(234)
  test_task[["train_data"]] <- cbind(test_task[["train_data"]],
    ab = sample(c("a", "b"), nrow(test_task[["train_data"]]), replace = TRUE),
    cd = sample(c("c", "d"), nrow(test_task[["train_data"]]), replace =
      TRUE),
    outcome = sample(c(TRUE, FALSE), nrow(test_task[["train_data"]]), replace
      = TRUE)
  )
  return(test_task)
}

create_prepared_fte_test_task <- function() {

  fte_test_task <- create_fte_test_task()

  prepared_task <- prepare(
    fte_test_task,
    data_names = c("train_data", "validation_data"),
    preparation_map_options = list(
      outcome_field = "outcome",
      target_encode_fields = c("ab", "cd")
      ),
    prepare_options = list(
      outcome_field = "outcome",
      target_encode_fields = c("ab", "cd")
      ),
    training_fields = c("target", "score", "target_encode_ab",
      "target_encode_cd")
  )
  return(prepared_task)
}

test_that("map creation works as expected", {
  prep_task <- create_prepared_fte_test_task()
  expect_known_value(
    prep_task[["prepared_validation_data"]],
    file = "./map_golden.log",
    update = FALSE
  )
  })

test_that(
  "Les logs de la fonction 'prepare_data' fonctionnent correctement", {
    task <- get_test_task()
    task[["tracker"]] <- new.env()
    with_mock(
      create_prepared_task(task),
      log_param = mock_log_param,
      log_metric = mock_log_metric
    )
    expect_true(length(ls(task[["tracker"]])) > 0)
    expect_setequal(
      names(task[["tracker"]]),
      c("preprocessing_strategy")
    )
    expect_equal(
      get("preprocessing_strategy", envir = task[["tracker"]]),
      "Target encoding with fte"
    )
    task[["tracker"]] <- NULL
  }
)
