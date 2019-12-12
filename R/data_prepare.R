#' Préparation des échantillons pour l'entraînement et l'évaluation
#'
#' Prépare les échantillons souhaités (listés dans `data_names`) pour
#' l'entraînement ou la prédiction de l'algorithme. Cf `[prepare_data]` pour
#' la nature de cette préparation.
#'
#' @inheritParams generic_task
#' @param data_names `character()` \cr Vecteur de noms des données à préparer.
#'   Doivent-être des noms de champs valides de `task`.
#' @param prepare_train_function :: (data_to_prepare :: data.frame) -> list(data :: data.frame,
#' preparation_map :: any) \cr
#'   A function that prepares the training data and gives a preparation_map to be applied for
#'   further data preparation.
#'   Can be equal to "xgboost" or "linear" for quick reference to the right
#'   preparation function.
#' @param prepare_test_function :: (data_to_prepare :: data.frame,
#' preparation_map :: any) -> data.frame \cr
#'   A function that uses the preparation map to prepare further data (test,
#'   validation data etc.)
#'   Can be equal to "xgboost" or "linear" for quick reference to the right
#'   preparation function.
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
prepare.sf_task <- function(
  task,
  data_names = c(
    "train_data",
    "validation_data",
    "test_data",
    "new_data"
    ),
  prepare_train_function = "xgboost",
  prepare_test_function = "xgboost",
  outcome_field = "outcome",
  tracker = NULL,
  preprocessing_strategy = NULL,
  training_fields = get_fields(training = TRUE),
  ...
  ){

  set_verbose_level(task)
  ## Assertions ##
  admissible_character_types <- c("xgboost", "linear")
  assertthat::assert_that(
    ( length(prepare_train_function) == 1 &&
      prepare_train_function %in% admissible_character_types ) ||
    !is.character(prepare_train_function)
  )
  assertthat::assert_that(
    ( length(prepare_test_function) == 1 &&
      prepare_test_function %in% admissible_character_types ) ||
    !is.character(prepare_test_function)
  )


  ## Defaults ##
  if (is.null(tracker)){
    tracker  <- task[["tracker"]]
  }
  if (is.null(preprocessing_strategy)){
    preprocessing_strategy <- "Target encoding with fte"
  }

  ## Function selection ##
  if (is.character(prepare_train_function)){
    prepare_train_function <- select_prepare_function(
      prepare_train_function,
      "train"
    )
  }
  if (is.character(prepare_test_function)){
    prepare_test_function  <- select_prepare_function(
      prepare_test_function,
      "test"
    )
  }

  ## Core ##
  task[["features"]] <- training_fields

  aux_prepare_train_or_test <- function(task, name){
    if (name %in% names(task)){
      if (name == "train_data"){
        logger::log_info("Preparing {name} for training")
        out <- prepare_train_function(
          data = task[[name]],
          training_fields = training_fields,
          outcome_field = outcome_field
        )
        task[[paste0("prepared_", name)]]  <- out[["data"]]
        task[[paste0("outcome_", name)]]  <- out[["outcome"]]
        task[["preparation_map"]]  <- out[["preparation_map"]]
      } else {
        logger::log_info("Preparing {name} for predicting")
        assertthat::assert_that(
          "preparation_map" %in% names(task),
          msg = 'No preparation map has been found. Have you loaded a task, or
          prepared the "train_data" first ?'
        )
        out <- prepare_test_function(
          data = task[[name]],
          preparation_map = task[["preparation_map"]],
          training_fields = training_fields,
          outcome_field = outcome_field
        )

        task[[paste0("prepared_", name)]]  <- out[["data"]]
        task[[paste0("outcome_", name)]]  <- out[["outcome"]]
      }
    } else {
      logger::log_warn("There is no {name} in current task")
    }
    return(invisible(task))
  }

  task  <- purrr::reduce(
    data_names,
    ~ aux_prepare_train_or_test(task = .x, name = .y),
    .init = task
  )

  if (!is.null(tracker)){
    tracker$set(preprocessing_strategy = preprocessing_strategy)
  }
  return(task)
}

#' Prepares a cross-validated task
#'
#' Prepares all subtasks (one for each fold) from cv_task. Benefits from the
#' same defaults as for sf_tasks.
#'
#' @inheritParams prepare.sf_task
#' @return The task given as input where each fold-task has been prepared.
#' @export
prepare.cv_task <- function(
  task,
  data_names = c("train_data",
    "validation_data",
    "test_data",
    "new_data"
    ),
  prepare_train_function = "xgboost",
  prepare_test_function = "xgboost",
  outcome_field = "outcome",
  tracker = NULL,
  preprocessing_strategy = NULL,
  training_fields =  get_fields(training = TRUE),
  ...
  ){

  requireNamespace("purrr")

  task[["cross_validation"]] <- purrr::map(
    .x = task[["cross_validation"]],
    .f = prepare.sf_task,
    data_names = data_names,
    prepare_train_function = prepare_train_function,
    prepare_test_function = prepare_test_function,
    outcome_field = outcome_field,
    tracker = tracker,
    preprocessing_strategy = preprocessing_strategy,
    training_fields = training_fields
  )
  return(task)
}


#' Selectionne la fonction de préparation adéquate selon son intitulé
#'
#' @param prepare_type `character(1)` \cr Intitulé de la fonction préparation.
#' @param test_or_train `"test" || "train"` \cr Fonction utilisée pour
#' préparer un échantillon d'entraînement ou de test ?
#'
#' @return Une fonction de préparation
#'
select_prepare_function <- function(prepare_type, test_or_train){
  assertthat::assert_that(test_or_train %in% c("test", "train"))

  if (test_or_train == "train") {
    prepare_function <- switch(
      prepare_type,
      xgboost = prepare_train_frame_xgboost,
      linear = prepare_train_frame_linear
    )
  } else if (test_or_train == "test") {
    prepare_function <- switch(
      prepare_type,
      xgboost = prepare_test_frame_xgboost,
      linear = prepare_test_frame_linear
    )
  }

  return(prepare_function)
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
prepare_train_frame_xgboost <- function(
 data_to_prepare,
 training_fields,
 outcome_field
  ){

  holdout_type <- "leave_one_out"
  prior_sample_size <- 30
  noise_level <- 0.02

  preparation_map <- fte::target_encode_create(
    data_to_encode = data_to_prepare,
    group_variables = list(
      c("code_naf"),
      c("code_ape_niveau2"),
      c("code_ape_niveau3")
      ),
    outcome_variable = outcome_field
  )

  prepared_data <- fte::target_encode_apply(
    data = data_to_prepare,
    group_variables = list(
      c("code_naf"),
      c("code_ape_niveau2"),
      c("code_ape_niveau3")
      ),
    outcome_variable = outcome,
    preparation_map = preparation_map,
    holdout_type = holdout_type,
    prior_sample_size = prior_sample_size,
    noise_level = noise_level
  )

  prepared_data <- prepared_data %>%
    dplyr::select(dplyr::one_of(training_fields))

  prepared_data <- as.matrix(prepared_data)

  res <- list(
    data = prepared_data,
    outcome = data_to_prepare[[outcome_field]],
    preparation_map = preparation_map
  )
  return(res)
}

#' Préparation des données pour la prédiction
#'
#' Inclut la conversion en H2O, et le target encoding.
#' La map pour le target encoding peut être sauvegardé, afin de permettre de
#' l'appliquer à de nouvelles données.
#' Le target encoding se fait avec du bruit pour éviter le surapprentissage.
#'
#' @inheritParams prepare.sf_task
#' @inheritParams generic_prepare_train_frame
#' @param preparation_map `?` \cr Carte de calcul du target encoding.
#'
#' @return `list(2)` \cr
#'   Une liste avec deux champs:
#'   * "data": `H2OFrame`; les données préparées
#'   * "preparation_map": `list(H2OFrame)`; la carte de target encoding
prepare_test_frame_xgboost <- function(
  data_to_prepare,
  preparation_map,
  training_fields,
  outcome_field
  ){

  holdout_type <- "none"
  prior_sample_size <- 30
  noise_level <- 0

  prepared_data <- fte::target_encode_apply(
    data = data_to_prepare,
    group_variables = list(
      c("code_naf"),
      c("code_ape_niveau2"),
      c("code_ape_niveau3")
      ),
    outcome_variable = outcome_field,
    preparation_map = preparation_map,
    holdout_type = holdout_type,
    prior_sample_size = prior_sample_size,
    noise_level = noise_level
  )

  prepared_data <- prepared_data %>%
    dplyr::select(dplyr::one_of(training_fields))

  prepared_data <- as.matrix(prepared_data)
  res  <- list(
    data = prepared_data,
    outcome = data_to_prepare[[outcome_field]]
    )
  return(res)
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
prepare_train_frame_linear <- function(
  data_to_prepare,
  training_fields,
  outcome_field
  ){

  holdout_type <- "leave_one_out"
  prior_sample_size <- 30
  noise_level <- 0.02

   target_encode_map <- fte::target_encode_create(
     data_to_encode = data_to_prepare,
     group_variables = list(
       c("code_naf"),
       c("code_ape_niveau2"),
       c("code_ape_niveau3")
       ),
     outcome_variable = outcome_field
   )

   prepared_data <- fte::target_encode_apply(
     data = data_to_prepare,
     group_variables = list(
       c("code_naf"),
       c("code_ape_niveau2"),
       c("code_ape_niveau3")
       ),
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

#' Préparation des données pour la prédiction
#'
#' Inclut la conversion en H2O, et le target encoding.
#' La map pour le target encoding peut être sauvegardé, afin de permettre de
#' l'appliquer à de nouvelles données.
#' Le target encoding se fait avec du bruit pour éviter le surapprentissage.
#'
#' @inheritParams prepare.sf_task
#' @inheritParams generic_prepare_train_frame
#' @param preparation_map `?` \cr Carte de calcul du target encoding.
#'
#' @return `list(2)` \cr
#'   Une liste avec deux champs:
#'   * "data": `H2OFrame`; les données préparées
#'   * "preparation_map": `list(H2OFrame)`; la carte de target encoding
prepare_test_frame_linear <- function(
  data_to_prepare,
  preparation_map,
  training_fields,
  outcome_field
  ){

  holdout_type <- "none"
  prior_sample_size <- 30
  noise_level <- 0

  prepared_data <- fte::target_encode_apply(
    data = data_to_prepare,
    group_variables = list(
      c("code_naf"),
      c("code_ape_niveau2"),
      c("code_ape_niveau3")
      ),
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
transformation_map_create <- function(data_to_transform){
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
transformation_map_apply <- function(transformation_map, data_to_transform){
  if (requireNamespace("bestNormalize")) {
    res <- purrr::map2_dfc(
      transformation_map,
      names(transformation_map),
      ~ predict(.x, data_to_transform[[.y]])
    )
   return(res)
  }
}

