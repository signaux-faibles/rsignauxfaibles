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
  outcome_field = "outcome",
  # OLD API
  preparation_map_function = create_fte_map,
  preparation_map_options = list(
    target_encode_fields = c("code_ape_niveau2", "code_ape_niveau3")
    ),
  prepare_function = apply_fte_map,
  prepare_options = list(
    target_encode_fields = c("code_ape_niveau2", "code_ape_niveau3")
    ),
  shape_frame_function = shape_for_xgboost,
  shape_frame_options = list(),
  # NEW API
  processing_pipeline = NULL,
  # END
  preprocessing_strategy = NULL,
  ...
  ) {

  set_verbose_level(task)
  data_names <- subset_data_names_in_task(data_names, task)


  ## Core ##
  task[["training_fields"]] <- training_fields
  task[["outcome_field"]] <- outcome_field


  task  <- purrr::reduce(
    data_names,
    ~ prepare_one_data_name(
      task = .x,
      data_name = .y,
      preparation_map_function,
      preparation_map_options,
      prepare_function,
      prepare_options,
      shape_frame_function,
      shape_frame_options
      ),
    .init = task,
  )

  # TODO: warn when invalid features as argument
  # TODO: accept new variables created during preparation as features
  task[["mlr3task"]]$col_roles$feature <- intersect(
    training_fields,
    task[["mlr3task"]]$col_roles$feature
  )

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


#' Prepares one type of data (train, test, validation, new, other ...)
prepare_one_data_name <- function(
  task,
  data_name,
  preparation_map_function,
  preparation_map_options,
  prepare_function,
  prepare_options,
  shape_frame_function,
  shape_frame_options
  ) {

  training_fields <- task[["training_fields"]]

  log_preparation_info(data_name)

  is_train_data <- (data_name == "train_data")
  if (is_train_data) {
    task[["preparation_map"]]  <- preparation_map_function(
      task[[data_name]],
      c(
        preparation_map_options,
        list(
          TRAINING_FIELDS = task[["training_fields"]],
          OUTCOME_FIELD = task[["outcome_field"]]
        )
      )
    )
  }

  # Maybe set prepare_train_options to default here
  prepared_data <- prepare_function(
    task[[data_name]],
    c(
      prepare_options,
      list(
        PREPARATION_MAP = task[["preparation_map"]],
        IS_TRAIN_DATA = is_train_data,
        TRAINING_FIELDS = task[["training_fields"]],
        OUTCOME_FIELD = task[["outcome_field"]]
      )
    )
  )

  filtered_data <- prepared_data %>%
    dplyr::select(dplyr::one_of(training_fields))

  shaped_data <- shape_frame_function(
    filtered_data,
    shape_frame_options
  )

  task[[paste0("prepared_", data_name)]]  <- shaped_data
  return(invisible(task))
}

#' Logs minimal information on preparation
log_preparation_info <- function(data_name) {
  if (data_name == "train_data") {
    logger::log_info("Preparing {data_name} for training")
  } else {
    logger::log_info("Preparing {data_name} for predicting")
  }
  return()
}

#' Asserts that preparation map is present in data
assert_preparation_map <- function(task) {
  assertthat::assert_that(
    "preparation_map" %in% names(task),
    msg = 'No preparation map has been found. Have you prepared the
    "train_data" first ?'
  )
}


#' Création d'une carte de préparation
#'
#'
#'
#'
#'
#'
#' @export
create_fte_map <- function(
  data_to_prepare,
  options
  ) {
  assertthat::assert_that(all(
      c("target_encode_fields") %in%
        names(options)
      ))
  outcome_field <- options[["OUTCOME_FIELD"]]
  target_encode_fields <- options[["target_encode_fields"]]

  preparation_map <- fte::target_encode_create(
    data_to_encode = data_to_prepare,
    group_variables = target_encode_fields,
    outcome_variable = outcome_field
  )

  return(preparation_map)
}


#' Préparation des données pour l'entraînement
#'
#' Prépare les données pour l'entraînement, avec du target encoding et la
#' transformation en matrice pour alimenter xgboost.
#'
#' La carte de target encoding est retourné en paramètre de sortie afin de
#' pouvoir l'utiliser sur d'autres échantillons de données.
#'
#' @inheritParams prepare.sf_task
#' @param data_to_prepare `data.frame()` \cr Données à préparer
#' @param shape_function `function(data.frame => ?)` \cr
#'   Fonction qui met en forme les données pour l'apprentissage.
#'
#' @return `list(2)` \cr
#'   Une liste avec deux champs:
#'   * "data": `H2OFrame`; les données préparées
#'   * "preparation_map": `list(H2OFrame)`; la carte de target encoding
#' @export
apply_fte_map <- function(
  data_to_prepare,
  options
  ) {

  assertthat::assert_that(all(
      c("target_encode_fields") %in%
        names(options)
      ))
  outcome_field <- options[["OUTCOME_FIELD"]]
  target_encode_fields <- options[["target_encode_fields"]]
  should_target_encode <- length(target_encode_fields) > 0
  if (! should_target_encode) {
    return(data_to_prepare)
  }

  if (options[["IS_TRAIN_DATA"]]) {
    holdout_type <- "leave_one_out"
    prior_sample_size <- 30
    noise_level <- 0.02
  } else {
    holdout_type <- "none"
    prior_sample_size <- 30
    noise_level <- 0
  }

  prepared_data <- fte::target_encode_apply(
    data = data_to_prepare,
    group_variables =  target_encode_fields,
    outcome_variable = outcome_field,
    preparation_map = options[["PREPARATION_MAP"]],
    holdout_type = holdout_type,
    prior_sample_size = prior_sample_size,
    noise_level = noise_level
  )

  return(prepared_data)
}

#' Returns a matrix for using xgboost
#'
#' @param data_to_shape `data.frame()`
#' @param options `list`(any)
#'
#' @return A matrix
#' @export
shape_for_xgboost <- function(
  data_to_shape,
  options
  ) {
  assert_only_valid_columns(data_to_shape)
  return(as.matrix(data_to_shape))
}

#' Check if any column is character or factor
assert_only_valid_columns <- function(data_to_shape) {
  column_classes <- purrr::map(data_to_shape, class)
  class_is_correct <- column_classes != "factor" & column_classes != "character"
  wrong_column_names <- names(data_to_shape)[!class_is_correct]
  assertthat::assert_that(
    all(class_is_correct),
    msg = paste0("Column ", wrong_column_names, " is of unsupported type
      factor or character")
    )
    return()
}


#' Préparation des données pour l'entraînement d'un modèle linéaire
#'
#' Prépare les données pour l'entraînement, avec du target encoding et la
#' transformation en matrice pour alimenter xgboost.
#'
#' La carte de target encoding est retourné en paramètre de sortie afin de
#' pouvoir l'utiliser sur d'autres échantillons de données.
#'
#' @inheritParams prepare.sf_task
#' @param data_to_prepare `data.frame()` \cr Données à préparer
#' @param target_encode_fields `character()` \cr Nom des champs sur lesquels
#' effectuer du target encoding.
#'
#' @return `list(2)` \cr
#'   Une liste avec deux champs:
#'   * "data": `H2OFrame`; les données préparées
#'   * "preparation_map": `list(H2OFrame)`; la carte de target encoding
#' @export
prepare_train_frame_linear <- function(
  data_to_prepare,
  training_fields,
  outcome_field,
  target_encode_fields
  ) {

  holdout_type <- "leave_one_out"
  prior_sample_size <- 30
  noise_level <- 0.02

  target_encode_map <- fte::target_encode_create(
    data_to_encode = data_to_prepare,
    group_variables = target_encode_fields,
    outcome_variable = outcome_field
  )

  prepared_data <- fte::target_encode_apply(
    data = data_to_prepare,
    group_variables =  target_encode_fields,
    outcome_variable = outcome,
    preparation_map = target_encode_map,
    holdout_type = holdout_type,
    prior_sample_size = prior_sample_size,
    noise_level = noise_level
  )

  prepared_data <- prepared_data %>%
    dplyr::select(dplyr::one_of(training_fields))

  transformation_map <- transformation_map_create(
    data_to_transform = prepared_data
  )

  prepared_data <- transformation_map_apply(
    transformation_map,
    data_to_transform = prepared_data
  )


  prepared_data <- as.matrix(prepared_data)

  complete_cases <- stats::complete.cases(prepared_data)
  prepared_data <- prepared_data[complete_cases, ]
  outcome  <- data_to_prepare[[outcome_field]][complete_cases]

  res <- list(
    data = prepared_data,
    outcome = outcome,
    preparation_map = list(
      target_encode_map = target_encode_map,
      transformation_map = transformation_map
    )
  )
  return(res)
}

#' Préparation des données pour la prédiction d'un modèle linéaire
#'
#' Inclut la conversion en H2O, et le target encoding.
#' La map pour le target encoding peut être sauvegardé, afin de permettre de
#' l'appliquer à de nouvelles données.
#' Le target encoding se fait avec du bruit pour éviter le surapprentissage.
#'
#' @inheritParams prepare.sf_task
#' @inheritParams prepare_train_frame_linear
#' @param preparation_map `?` \cr Carte de calcul du target encoding.
#'
#' @return `list(2)` \cr
#'   Une liste avec deux champs:
#'   * "data": `H2OFrame`; les données préparées
#'   * "preparation_map": `list(H2OFrame)`; la carte de target encoding
#' @export
prepare_test_frame_linear <- function(
  data_to_prepare,
  preparation_map,
  training_fields,
  outcome_field,
  target_encode_fields
  ) {

  holdout_type <- "none"
  prior_sample_size <- 30
  noise_level <- 0

  prepared_data <- fte::target_encode_apply(
    data = data_to_prepare,
    group_variables = target_encode_fields,
    outcome_variable = outcome_field,
    preparation_map = preparation_map[["target_encode_map"]],
    holdout_type = holdout_type,
    prior_sample_size = prior_sample_size,
    noise_level = noise_level
  )

  prepared_data <- prepared_data %>%
    dplyr::select(dplyr::one_of(training_fields))

  prepared_data <- transformation_map_apply(
    preparation_map[["transformation_map"]],
    prepared_data
  )

  prepared_data <- as.matrix(prepared_data)

  complete_cases <- stats::complete.cases(prepared_data)
  prepared_data <- prepared_data[complete_cases, ]
  outcome  <- data_to_prepare[[outcome_field]][complete_cases]

  res <- list(
    data =  prepared_data,
    outcome = outcome
  )
  return(res)
}

#' Normalise les données
#'
#' Sélectionne des transformations pour que les données approchent de la
#' normalité.
#'
#' @param data_to_transform `data.frame()` \cr Données à transformer.
#'
transformation_map_create <- function(data_to_transform) {
  # Not sure it works
  if (requireNamespace("bestNormalize")) {
    map <- purrr::map(
      data_to_transform,
      ~ bestNormalize::yeojohnson(
        as.numeric(.),
        standardize = FALSE
      )
    )
    return(map)
  }
}

#' Applies Box-cox transformation map to features
#'
#' @param transformation_map As outputed by transformation_map_create
#' @param data_to_transform data to be transformed
#'
transformation_map_apply <- function(transformation_map, data_to_transform) {
  # Not sure it works
  if (requireNamespace("bestNormalize")) {
    res <- purrr::map2_dfc(
      transformation_map,
      names(transformation_map),
      ~ predict(.x, data_to_transform[[.y]])
    )
    return(res)
  }
}

#' Prepares a cross-validated task
#'
#' Prepares all subtasks (one for each fold) from cv_task. Benefits from the
#' same defaults as for sf_tasks.
#'
#' @inheritParams prepare.sf_task
#' @return The task given as input where each fold-task has been prepared.
#' @export
prepare.cv_task <- function( #nolint
  task,
  data_names = c("train_data",
    "test_data",
    "new_data"
    ),
  preparation_map_function = create_fte_map,
  preparation_map_options = list(
    outcome_field = outcome_field,
    target_encode_fields = c("code_ape_niveau2", "code_ape_niveau3")
    ),
  prepare_function = apply_fte_map,
  prepare_options = list(
    outcome_field = outcome_field,
    target_encode_fields = c("code_ape_niveau2", "code_ape_niveau3")
    ),
  shape_frame_function = shape_for_xgboost,
  shape_frame_options = list(),
  outcome_field = "outcome",
  preprocessing_strategy = NULL,
  training_fields =  get_fields(training = TRUE),
  ...
  ) {

  requireNamespace("purrr")

  task[["cross_validation"]] <- purrr::map(
    .x = task[["cross_validation"]],
    .f = prepare.sf_task,
    data_names = data_names,
    preparation_map_function = create_fte_map,
    preparation_map_options = list(
      outcome_field = outcome_field,
      target_encode_fields = c("code_ape_niveau2", "code_ape_niveau3")
      ),
    prepare_function = apply_fte_map,
    prepare_options = list(
      outcome_field = outcome_field,
      target_encode_fields = c("code_ape_niveau2", "code_ape_niveau3")
      ),
    shape_frame_function = shape_for_xgboost,
    shape_frame_options = list(),
    outcome_field = outcome_field,
    tracker = tracker,
    preprocessing_strategy = preprocessing_strategy,
    training_fields = training_fields
  )
  return(task)
}
