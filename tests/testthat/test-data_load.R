context("Check database connection and data import and cleaning")

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

## Testing query stages

test_that("build_sort_stage builds a valid sort stage", {
   expect_equal(
     jsonlite::toJSON(build_sort_stage(), auto_unbox = TRUE) %>% toString(),
     '{"$sort":{"value.random_order":-1}}'
     )
})

test_that("build_limit_stage builds a valid limit stage", {
  expect_equal(
    jsonlite::toJSON(build_limit_stage(10), auto_unbox = TRUE) %>% toString(),
    '{"$limit":10}'
  )
})

test_that("build_replace_root_stage builds a valid replaceRoot stage", {
  expect_equal(
    jsonlite::toJSON(build_replace_root_stage(), auto_unbox = TRUE) %>%
      toString(),
    '{"$replaceRoot":{"newRoot":"$value"}}'
  )
})

test_that("build_projection_stage builds a valid projection stage", {
  expect_null(
    build_projection_stage(NULL)
  )
  expect_equal(
    jsonlite::toJSON(build_projection_stage("a"), auto_unbox = TRUE) %>%
      toString(),
    '{"$project":{"a":1}}'
  )
  expect_equal(
    jsonlite::toJSON(build_projection_stage(c("a", "b")), auto_unbox = TRUE) %>%
      toString(),
    '{"$project":{"a":1,"b":1}}'
  )
  expect_equal(
    jsonlite::toJSON(build_projection_stage(c("_id")), auto_unbox = TRUE) %>%
      toString(),
    '{"$project":{"_id":1}}'
  )
})

test_that("build_standard_match_stage builds a valid match stage", {
  expect_equal(
    jsonlite::toJSON(
      build_standard_match_stage(
        "2001",
        as.Date("2001-01-01"),
        as.Date("2021-01-01"),
        10
      ),
      auto_unbox = TRUE
    ) %>%
      toString(),
    paste0(
      '{"$match":{"$and":[',
      '{"_id.batch":"2001"},',
      '{"_id.periode":{"$gte":{"$date":"2001-01-01T00:00:00Z"}}},',
      '{"_id.periode":{"$lt":{"$date":"2021-01-01T00:00:00Z"}}},',
      '{"value.effectif":{"$gte":10}}',
      "]}}"
      )
  )
  expect_equal(
    jsonlite::toJSON(
      build_standard_match_stage(
        "2001",
        as.Date("2001-01-01"),
        as.Date("2021-01-01"),
        NULL
      ),
      auto_unbox = TRUE
    ) %>%
      toString(),
    paste0(
      '{"$match":{"$and":[',
      '{"_id.batch":"2001"},',
      '{"_id.periode":{"$gte":{"$date":"2001-01-01T00:00:00Z"}}},',
      '{"_id.periode":{"$lt":{"$date":"2021-01-01T00:00:00Z"}}},',
      '{"value.effectif":{"$gte":1}}',
      "]}}"
      )
  )
})


test_that("build_standard_match_stage builds a valid match stage", {
  expect_equal(
    jsonlite::toJSON(
      build_sector_match_stage(
        "2001",
        as.Date("2001-01-01"),
        as.Date("2021-01-01"),
        code_ape = c("1234B")
      ),
      auto_unbox = TRUE
    ) %>%
      toString(),
    paste0(
      '{"$match":{"$and":[',
      '{"_id.batch":"2001"},',
      '{"_id.periode":{"$gte":{"$date":"2001-01-01T00:00:00Z"}}},',
      '{"_id.periode":{"$lt":{"$date":"2021-01-01T00:00:00Z"}}},',
      '{"value.code_ape":{"$in":["1234B"]}}',
      "]}}"
      )
  )
})

test_that(
  "assemble_stages_to_query returns a json array of objects, in same order", {
   expect_equal(
      assemble_stages_to_query(list(b = 0), list(a = 1)) %>% toString(),
      '[{"b":0},{"a":1}]'
     )
})

test_that("assemble_stages_to_query ignores NULLs", {
   expect_equal(
      assemble_stages_to_query(NULL, list(a = 1)) %>% toString(),
      '[{"a":1}]'
     )
   expect_equal(
      assemble_stages_to_query(list(b = NULL), list(a = 1)) %>% toString(),
      '[{"a":1}]'
     )
})

test_that("build_standard_query builds a valid query", {
  expect_equal(
    build_standard_query(
      batch = "1802",
      date_inf = as.Date("2001-01-01"),
      date_sup = as.Date("2020-01-01"),
      min_effectif = 10,
      subsample = 10,
      fields = c("a")
    ) %>% toString(),
    paste0(
      '[{"$match":{"$and":[',
      '{"_id.batch":"1802"},',
      '{"_id.periode":{"$gte":{"$date":"2001-01-01T00:00:00Z"}}},',
      '{"_id.periode":{"$lt":{"$date":"2020-01-01T00:00:00Z"}}},',
      '{"value.effectif":{"$gte":10}}',
      "]}},",
      '{"$sort":{"value.random_order":-1}},',
      '{"$limit":10},',
      '{"$replaceRoot":{"newRoot":"$value"}},',
      '{"$project":{"a":1}}]'
    )
  )
})

test_that("build_siret_match_stage builds a valid match stage", {
  # One siret, unusual date_sup
  expect_equal(
    jsonlite::toJSON(
      build_siret_match_stage(
        batch = "2001",
        date_inf = as.Date("2014-01-01"),
        date_sup = as.Date("2014-01-02"),
        sirets = c("01234567891011")
      ),
      auto_unbox = TRUE
    ) %>%
      toString(),
    paste0(
      '{"$match":',
       '{"_id":{"$in":[',
       '{"batch":"2001",',
       '"siret":"01234567891011",',
       '"periode":{"$date":"2014-01-01T00:00:00Z"}}',
       "]}}}"
      )
  )
  # Several sirets, several periods
  expect_equal(
    jsonlite::toJSON(
      build_siret_match_stage(
        batch = "2001",
        date_inf = as.Date("2014-01-01"),
        date_sup = as.Date("2014-03-01"),
        sirets = c("01234567891011", "11109876543210")
      ),
      auto_unbox = TRUE
    ) %>%
      toString(),
    paste0(
      '{"$match":',
       '{"_id":{"$in":[',
       '{"batch":"2001",',
       '"siret":"01234567891011",',
       '"periode":{"$date":"2014-01-01T00:00:00Z"}},',
       '{"batch":"2001",',
       '"siret":"01234567891011",',
       '"periode":{"$date":"2014-02-01T00:00:00Z"}},',
       '{"batch":"2001",',
       '"siret":"11109876543210",',
       '"periode":{"$date":"2014-01-01T00:00:00Z"}},',
       '{"batch":"2001",',
       '"siret":"11109876543210",',
       '"periode":{"$date":"2014-02-01T00:00:00Z"}}',
       "]}}}"
      )
  )
})

#
# import_data
#

test_that(
  "La propriété 'mlr3task' est créée", {
    expect_error(test_task <- get_test_task(stage = "load"), NA)
    expect_true("mlr3task" %in% names(test_task))
    expect_true(inherits(test_task$mlr3task, "TaskClassif"))
})

import_test_data <- function(
                             batch,
                             fields,
                             sirets,
                             code_ape) {
  test_db <- "unittest_signauxfaibles"
  test_col <- "Features_for_tests"
  if (!is.null(sirets)) {
    subsample <- NULL
  } else {
    subsample <- 10
  }
  data_import <- import_data(
    test_db,
    test_col,
    mongodb_uri = "mongodb://localhost:27017",
    batch = batch,
    min_effectif = 10,

    date_inf = as.Date("2014-01-01"),
    date_sup = as.Date("2014-02-01"),
    subsample = subsample,
    fields = fields,
    sirets = sirets,
    code_ape = code_ape
  )
  return(data_import)
}

test_that(
  "une requête vide renvoie un dataframe vide avec une requête standard", {
    testthat::skip_on_ci()
    empty_data <- import_test_data(
      "wrong_batch",
      fields = c("siret", "periode"),
      sirets = NULL,
      code_ape = NULL
    )
    expect_equal(dim(empty_data), c(0, 2))
  })

test_that(
  "On récupére les éléments de la base avec une requête standard", {

  testthat::skip_on_ci()
  fields <- c("siret", "periode")
  test_object <- import_test_data(
    "test_batch_1",
    fields = fields,
    sirets = NULL,
    code_ape = NULL
  )
  expect_equal(names(test_object), fields)
  expect_equal(as.character(test_object$siret), "01234567891011")
  expect_equal(as.Date(test_object$periode), as.Date("2014-01-01"))
})

test_that(
  "une requête vide renvoie un dataframe vide avec une requête par siret", {
  testthat::skip_on_ci()
  empty_data <- import_test_data(
    "test_batch_1",
    fields = c("siret", "periode"),
    sirets = c("1110987654321"),
    code_ape = NULL
  )
  expect_equal(dim(empty_data), c(0, 2))
})

test_that(
  "On récupére les éléments de la base avec une requête par siret", {
  testthat::skip_on_ci()
  fields <- c("siret", "periode")
  test_object <- import_test_data(
    "test_batch_1",
    fields = fields,
    sirets = c("01234567891011"),
    code_ape = NULL
  )
  expect_equal(names(test_object), fields)
  expect_equal(as.character(test_object$siret), "01234567891011")
  expect_equal(as.Date(test_object$periode), as.Date("2014-01-01"))
})

test_that(
  "On récupére les éléments de la base avec une requête par code ape", {
  testthat::skip_on_ci()
  fields <- c("siret", "periode")
  test_object <- import_test_data(
    "test_batch_1",
    fields = fields,
    sirets = NULL,
    code_ape = "1234B"
  )
  expect_equal(names(test_object), fields)
  expect_equal(as.character(test_object$siret), "01234567891011")
  expect_equal(as.Date(test_object$periode), as.Date("2014-01-01"))
})

test_that(
  "get_fields has not inadvertedly changed", {
    # Mostly for coverage.
    expect_length(get_fields(TRUE), 267)
  })
