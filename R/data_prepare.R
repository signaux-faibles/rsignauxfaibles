#' Préparation des échantillons pour l'entraînement et l'évaluation
#'
#' Prépare les échantillons souhaités (listés dans `data_names`) pour
#' l'entraînement ou la prédiction de l'algorithme. Cf `[prepare_data]` pour
#' la nature de cette préparation.
#'
#' @inheritParams generic_task
#' @param data_names `character()` \cr Vecteur de noms des données à préparer.
#'   Doivent-être des noms de champs valides de `task`.
#' @param prepare_train_function :: (data_to_prepare :: data.frame) ->
#'   list(data :: data.frame, preparation_map :: any) \cr
#'   A function that prepares the training data and gives a preparation_map to
#'   be applied for further data preparation. Defaults to
#'   `prepare_train_frame_xgboost`.
#'   /!\ Beware, this function is not yet generic !
#' @param prepare_test_function :: (data_to_prepare :: data.frame,
#' preparation_map :: any) -> data.frame \cr
#'   A function that uses the preparation map to prepare further data (test,
#'   validation data etc.). Defaults to `prepare_test_frame_xgboost`.
#'   /!\ Beware, this function is not yet generic !
#' @param preprocessing_strategy character() \cr
#'   Description en langage naturel de la stratégie de préparation des données
#'   pour le logging de l'expérience.
#' @param training_fields `character()` \cr Les champs qui doivent être
#'   conservées pour l'entraînement, après toutes les phases de préparation,
#'   notamment le target encoding.
#' @param ... Unused.
#'
#' @return `[sf_task]` \cr
#'   L'objet \code{task} donné en entrée auquel les champs de données
#'   préparées ont été ajoutées, avec une convention de nommage d'apposition
#'   du préfixe "prepared_" aux noms des données préparées (par exemple:
#'   "prepared_train_data" correspond aux données de "train_data" préparées).
#'
#' @export
prepare.sf_task <- function( #nolint
  task,
  data_names = c(
    "train_data",
    "test_data",
    "new_data"
    ),
  training_fields = get_fields(training = TRUE),
  outcome_field = NULL,
  processing_pipeline = get_default_pipeline(),
  preprocessing_strategy = NULL,
  ...
  ) {

  set_verbose_level(task)
  data_names <- subset_data_names_in_task(data_names, task)

  ## Core ##
  task[["training_fields"]] <- training_fields

  if (is.null(outcome_field)) {
    task[["outcome_field"]] <- task[["target"]]
  } else {
    task[["outcome_field"]] <- outcome_field
    task[["mlr3task"]]$col_roles$target <- outcome_field
  }

  task[["mlr3pipeline"]] <- processing_pipeline
  gpo <-  mlr3pipelines::as_graph(task[["mlr3pipeline"]])

  task[["mlr3task"]]$col_roles$feature <- intersect(
    training_fields,
    task[["mlr3task"]]$col_roles$feature
  ) # New features are automatically added when training pipeline

  if (is.null(preprocessing_strategy)) {
    ## Default value
    preprocessing_strategy <- "Target encoding with fte"
  }
  log_param(task, "preprocessing_strategy", preprocessing_strategy)

  return(task)
}

#' Keeps only the data_names that are indeed present in the task, warns if some
#' are missing.
subset_data_names_in_task <- function(data_names, task) {
  data_name_is_missing <- ! (data_names %in% names(task))
  if (any(data_name_is_missing)) {
    missing_data_names <- data_names[data_name_is_missing]
    logger::log_warn("There is no {missing_data_names} in current task")
  }
  return(data_names[!data_name_is_missing])
}

create_fte_pipeline <- function(
   target_encode_fields
  ) {
  poe <- mlr3pipelines::po("encodeimpact",
    param_vals = list(
      affect_columns = mlr3pipelines::selector_name(target_encode_fields)
      )
   )
}

#' Construct default pipeline
#'
#' @export
get_default_pipeline <- function() {
  pipeline <- create_fte_pipeline(
    c("code_ape_niveau2", "code_ape_niveau3")
    ) %>>%
    po(
      "encode",
      method = "treatment",
      affect_columns = selector_type("factor")
    )
  return(pipeline)
}

#' Apply preparation pipeline and inspect prepared data
#'
#' @export
get_prepared_data <- function(
  task,
  train_or_test
  ) {
  assertthat::assert_that(train_or_test %in% c("train", "test"))
  assertthat::assert_that(
    "mlr3pipeline" %in% names(task),
    msg = "A pipeline is needed to get prepared data (property: mlr3pipeline)"
  )
  train_id <- task[["mlr3rsmp"]]$train_set(1)
  gpo <- mlr3pipelines::as_graph(task[["mlr3pipeline"]])
  gpo$train(task[["mlr3task"]]$clone()$filter(train_id))
  pred <- gpo$predict(task[["mlr3task"]])[[1]]
  if (train_or_test == "test") {
    test_id <- task[["mlr3rsmp"]]$test_set(1)
    return(pred$data(test_id) %>% as.data.frame())
  } else {
    return(pred$data(train_id) %>% as.data.frame())
  }
}
