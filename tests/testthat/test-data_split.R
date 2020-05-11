context("Check the test/train split function")

my_test_frame <- expand.grid(
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
  ) %>%
tibble::as_tibble()

test_task <- get_test_task(my_test_frame, "outcome", stage = "load")


test_that("split_data fonctionne et crée des champs train_data et test_data", {
  splitted_task  <- split_data(test_task, ratio = 2/3, resampling_strategy = "holdout")
  expect_true(all(c("train_data", "test_data") %in% names(splitted_task)))
})

test_that("Les échantillons ont les bonnes proportions", {

  expect_ratio <- function(task, expected_ratio, subframe_name , whole_frame_name) {
    whole_frame <- task[[whole_frame_name]]
    subframe <- task[[subframe_name]]
    n_siren <- n_distinct(whole_frame$siren)
    sub_siren <- n_distinct(subframe$siren)
    ratio <- sub_siren / n_siren
    expect_lt(abs(ratio - expected_ratio), 0.01)
    return(ratio)
  }
  expect_train_test_ratio <- function(expected_ratio) {
    splitted_task  <- split_data(test_task, ratio = expected_ratio, resampling_strategy = "holdout")
    ratio_train <- expect_ratio(splitted_task, expected_ratio, "train_data", "hist_data")
    ratio_test <- expect_ratio(splitted_task, 1 - expected_ratio, "test_data", "hist_data")
    expect_true(ratio_train + ratio_test == 1)
  }

  expect_train_test_ratio(2/3)
  expect_train_test_ratio(1/10)
  expect_train_test_ratio(1)
  expect_error(expect_train_test_ratio(0))
})

test_that("Il n'y a pas de fuite de données entre échantillons", {
  splitted_task  <- split_data(test_task, ratio = 1/2, resampling_strategy = "holdout")
  expect_length(
    intersect(
      levels(splitted_task[["train_data"]]),
      levels(splitted_task[["test_data"]])
      ),
    0
  )
})

test_that(
  "Les échantillons ne dépendent pas de l'ordre des données d'entrée et
  restent identiques d'une fois sur l'autre", {
  folder <- here::here(
    "tests",
    "testthat",
    "test_split_consistency_known_output"
  )

  if (!dir.exists(folder)) skip("known values only on local repository")

  scrambled_test_frame  <- sample_n(
    my_test_frame,
    size = nrow(my_test_frame),
    replace = FALSE
  )

  expect_equal(
    split_data(
      get_test_task(my_test_frame, "outcome", stage = "load"),
      ratio = 2 / 3,
      resampling_strategy = "holdout"
      ),
    split_data(
      get_test_task(scrambled_test_frame, "outcome", stage = "load"),
      ratio  = 2 / 3,
      resampling_strategy  = "holdout"
    )
  )

  expect_known_hash(
    split_data(
      get_test_task(my_test_frame, "outcome", stage = "load"),
      ratio = 2 / 3,
      resampling_strategy = "holdout"
    ),
  "a00c49d18cfc6bbd0b3e7f5aaf83a"
  )
})

# test_that(
#   "Chaque entreprise appartient au moins à un échantillon", {
#     expect_true(all(unique(my_test_frame$siret) %in% combined$siret))
#   }
# )


# test_that(
#   "split_snapshot_rdm_month gère un nom unique", {
#     split <- split_snapshot_rdm_month(
#       my_test_frame,
#       fracs = c(0.25, 0.25, 0.25, 0.25),
#       names = c("cv")
#     )
#     actual_names <- names(split)
#     expected_names  <- c("cv_1", "cv_2", "cv_3", "cv_4")

#     expect_equal(actual_names, expected_names)
#   }
# )


# test_that(
#   "La fusion de deux catégories ne change pas la reproductibilité de la
#   troisième", {
#   split_1  <- split_snapshot_rdm_month(
#     my_test_frame,
#     fracs = c(0.25, 0.25, 0.25, 0.25),
#     names = c("no1", "no2", "no3", "test")
#   )
#   split_2  <- split_snapshot_rdm_month(
#     my_test_frame,
#     fracs = c(0.75, 0.25),
#     names = c("no1", "test")
#   )

#   expect_equal(split_1[["test"]], split_2[["test"]])
# }
# )

# test_that(
#   "Les logs de la fonction 'split_data' fonctionnent correctement", {
#     task <- get_test_task()
#     task[["tracker"]] <- new.env()
#     with_mock(
#       split_data(task, c(0.75, 0.20, 0.05)),
#       log_param = mock_log_param,
#       log_metric = mock_log_metric
#     )
#     expect_true(length(ls(task[["tracker"]])) > 0)
#     expect_setequal(
#       names(task[["tracker"]]),
#       c("resampling_strategy", "train_val_test_shares")
#     )
#     expect_equal(
#       get("resampling_strategy", envir = task[["tracker"]]),
#       "holdout"
#     )
#     expect_equal(
#       get("train_val_test_shares", envir = task[["tracker"]]),
#       c(train = 0.75, validation = 0.2, test = 0.05)
#     )
#     task[["tracker"]] <- NULL
#   }
# )

# TODO: remove here package from dependencies.
