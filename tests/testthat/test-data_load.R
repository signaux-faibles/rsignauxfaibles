context("Check database connection and data import and cleaning")

test_that("Les requêtes sont bien formées", {
  test_grid <- expand.grid(
    batch = "1812",
    date_inf = c("2014-02-01", NA),
    date_sup = c("2015-03-01", NA),
    min_effectif = c(10, NA),
    fields = c("siren", NA),
    siren = c("0123456789", NA),
    code_ape = c("A", NA),
    subsample = c(10, NULL),
    stringsAsFactors = FALSE
    )

  aux_null <- function(x) ifelse(is.na(x), return(NULL), return(x))
  aux_test_function <- function(batch, date_inf, date_sup, min_effectif,
    fields, siren, code_ape, subsample) {
    req <- factor_query(
      batch = batch,
      date_inf = aux_null(date_inf),
      date_sup = aux_null(date_sup),
      min_effectif = aux_null(min_effectif),
      fields = aux_null(fields),
      siren = aux_null(siren),
      code_ape = aux_null(code_ape),
      subsample = subsample
      )
    expect_true(jsonlite::validate(req),
      info = req
      )
  }

  mapply(
    aux_test_function,
    test_grid$batch,
    test_grid$date_inf,
    test_grid$date_sup,
    test_grid$min_effectif,
    test_grid$fields,
    test_grid$siren,
    test_grid$code_ape,
    test_grid$subsample
    )
})


#
# Test replace_missing_data
#

test_replace_missing_data <- function(
                                      description,
                                      df,
                                      fields,
                                      replace_missing,
                                      expected) {
  testthat::test_that("replace_missing_data " %>% paste0(description), {
    expect_equal(
      replace_missing_data(df, fields, replace_missing),
      expected
    )
  })
}

replace_missing_data_table <- tibble::tribble(
  ~description,
  ~df,
  ~fields,
  ~replace_missing,
  ~expected,

  "should add empty columns if nrow = 0",
  data.frame(),
  "a",
  list(),
  data.frame(a = logical(0)),

  "should replace NA in existing columns with numeric replacement",
  data.frame(a = c(NA)),
  "a",
  list(a = 0),
  data.frame(a = c(0)),

  "should replace NA in existing columns with logical replacement",
  data.frame(a = c(NA)),
  "a",
  list(a = TRUE),
  data.frame(a = c(TRUE)),

  "should replace NA in existing columns with character replacement",
  data.frame(a = c(NA)),
  "a",
  list(a = "a"),
  data.frame(a = c("a"), stringsAsFactors = FALSE),

  "should create non-existing columns with replacement, if in fields",
  data.frame(),
  "a",
  list(a = 0),
  data.frame(a = numeric(0)),

  "should not create columns if not in fields",
  data.frame(),
  c(),
  list(a = "a"),
  data.frame(),
  )

purrr::pwalk(
  .l = replace_missing_data_table,
  .f = test_replace_missing_data
)

#
# End-to-end: import_data
#

import_test_data <- function(batch, fields) {
  test_db <- "unittest_signauxfaibles"
  test_col <- "Features_for_tests"
  empty_data_import <- import_data(
    test_db,
    test_col,
    mongodb_uri = "mongodb://localhost:27017",
    batch = batch,
    min_effectif = 10,
    date_inf = "2014-01-01",
    date_sup = "2014-02-01",
    fields = fields,
    verbose = FALSE
    )
}
test_that("une requête vide renvoie un dataframe vide", {
  empty_data <- import_test_data("wrong_batch", fields = c("siret", "periode"))
  expect_equal(dim(empty_data), c(0, 2))
})

test_that("On arrive à récupérer les éléments de la base", {
  fields <- c("siret", "periode")
  test_object <- import_test_data("test_batch_1", fields = fields)
  expect_equal(names(test_object), fields)
  expect_equal(test_object$siret, "01234567891011")
  expect_equal(test_object$periode, as.Date("2014-01-01"))
})



# TODO: wrong database or collection should throw an error, not returning empty dataframe.
