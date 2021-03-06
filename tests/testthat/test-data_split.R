context("Check the test/train split function")

split_test_frame <- expand.grid(
  siret = unlist(
    tidyr::unite(
      expand.grid(siren = 100000001:100000500, et_id = 20001:20004),
      col = "siret",
      sep = ""
    )
  ),
  periode = seq.Date(
    from = as.Date("2014-01-01"),
    to = as.Date("2014-12-01"), "month"
  ),
  stringsAsFactors = FALSE
) %>%
  dplyr::mutate(
    siren = substr(siret, 1, 9),
    outcome = rep(c(TRUE, FALSE), length.out = n())
  )

test_task <- get_test_task(
  fake_data = split_test_frame,
  fake_target = "outcome",
  stage = "load"
)

test_that(
  "split_data does not throw an error with 'holdout' resampling strategy",
  {
    expect_error(
      split_data(
        test_task,
        ratio = 1 / 3,
        resampling_strategy = "holdout"
      ),
      NA
    )
  }
)

test_that("works with 'cv' resampling strategy,", {
  expect_error(split_data(test_task, resampling_strategy = "cv"), NA)
})

test_that("mlr3 resampling obj is stored in 'mlr3rsmp' property", {
  test_mlr3rsmp <- function(rsmp_strat) {
    splitted <- split_data(test_task, resampling_strategy = rsmp_strat)
    expect_true("mlr3rsmp" %in% names(splitted))
    expect_true(inherits(splitted[["mlr3rsmp"]], "Resampling"))
  }
  rsmp_strat_cases <- list("holdout", "cv")
  purrr::walk(rsmp_strat_cases, test_mlr3rsmp)
})

test_that("invalid resampling_strategy does not create a 'mlr3rsmp' property", {
  test_no_mlr3rsmp <- function(rsmp_strat) {
    splitted <- split_data(test_task, resampling_strategy = rsmp_strat)
    test_task_train <- test_task
    test_task_train[["train_data"]] <- test_task[["hist_data"]]
    expect_false("mlr3rsmp" %in% names(splitted))
    expect_equal(test_task_train, splitted)
  }
  rsmp_strat_cases <- list("none", NULL)
  purrr::walk(rsmp_strat_cases, test_no_mlr3rsmp)
})

test_that("'cv' respects 'nfold' parameter", {
  splitted <- split_data(test_task, nfolds = 5, resampling_strategy = "cv")
  expect_equal(splitted$mlr3rsmp$param_set$values$folds, 5)
})

test_that(
  "split_data est reproductible et crée des champs train_data et test_data",
  {
    splitted_task <- split_data(
      test_task,
      ratio = 2 / 3,
      resampling_strategy = "holdout"
    )
    expect_true(all(c("train_data", "test_data") %in% names(splitted_task)))
    expect_known_hash(splitted_task[["train_data"]], "0eb867dcee")
    expect_known_hash(splitted_task[["test_data"]], "1112962206")
  }
)

test_that("Les échantillons ont les bonnes proportions", {
  expect_ratio <- function(
                           task,
                           expected_ratio,
                           subframe_name,
                           whole_frame_name) {
    whole_frame <- task[[whole_frame_name]]
    subframe <- task[[subframe_name]]
    n_siren <- n_distinct(whole_frame$siren)
    sub_siren <- n_distinct(subframe$siren)
    ratio <- sub_siren / n_siren
    expect_lt(abs(ratio - expected_ratio), 0.01)
    return(ratio)
  }
  expect_train_test_ratio <- function(expected_ratio) {
    splitted_task <- split_data(
      test_task,
      ratio = expected_ratio,
      resampling_strategy = "holdout"
    )
    ratio_train <- expect_ratio(
      splitted_task,
      expected_ratio,
      "train_data",
      "hist_data"
    )
    ratio_test <- expect_ratio(
      splitted_task,
      1 - expected_ratio,
      "test_data",
      "hist_data"
    )
    expect_true(ratio_train + ratio_test == 1)
  }

  expect_train_test_ratio(2 / 3)
  expect_train_test_ratio(1 / 10)
  expect_train_test_ratio(1)
  expect_error(expect_train_test_ratio(0))
})

test_that("Il n'y a pas de fuite de données entre échantillons", {
  splitted_task <- split_data(
    test_task,
    ratio = 1 / 2,
    resampling_strategy = "holdout"
  )
  expect_length(
    intersect(
      levels(splitted_task[["train_data"]]),
      levels(splitted_task[["test_data"]])
    ),
    0
  )
})

test_that(
  "Chaque (établissement x période) appartient au moins à un échantillon",
  {
    splitted_task <- split_data(
      test_task,
      ratio = 2 / 3,
      resampling_strategy = "holdout"
    )
    should_be_empty <- splitted_task[["hist_data"]] %>%
      filter(
        !(siret %in% splitted_task[["train_data"]]$siret &
          periode %in% splitted_task[["train_data"]]$periode)
      ) %>%
      filter(
        !(siret %in% splitted_task[["test_data"]]$siret &
          periode %in% splitted_task[["test_data"]]$periode)
      )
    testthat::expect_equal(nrow(should_be_empty), 0)
  }
)

test_that(
  "Les logs de la fonction 'split_data' fonctionnent correctement",
  {
    task <- get_test_task()
    task[["tracker"]] <- new.env()
    ratio <- 2 / 3
    with_mock(
      split_data(task, ratio = ratio, resampling_strategy = "holdout"),
      log_param = mock_log_param,
      log_metric = mock_log_metric
    )
    expect_true(length(ls(task[["tracker"]])) > 0)
    expect_setequal(
      names(task[["tracker"]]),
      c("resampling_strategy", "train_test_ratio")
    )
    expect_equal(
      get("resampling_strategy", envir = task[["tracker"]]),
      "holdout"
    )
    expect_equal(
      get("train_test_ratio", envir = task[["tracker"]]),
      ratio
    )
    task[["tracker"]] <- NULL
  }
)
