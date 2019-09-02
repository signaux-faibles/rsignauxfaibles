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
#'   further data preparation
#' @param prepare_test_function :: (data_to_prepare :: data.frame,
#' preparation_map :: any) -> data.frame \cr
#'   A function that uses the preparation map to prepare further data (test,
#'   validation data etc.)
#' @param preprocessing_strategy character() \cr
#'   Description en langage naturel de la stratégie de préparation des données
#'   pour le logging de l'expérience.
#' @param training_fields `character()` \cr Les champs qui doivent être
#'   conservées pour l'entraînement, après toutes les phases de préparation,
#'   notamment le target encoding.
#'
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
  prepare_train_function = NULL,
  prepare_test_function = NULL,
  tracker = NULL,
  preprocessing_strategy = NULL,
  training_fields =  get_fields(training = TRUE),
  ...
  ){

  # Defaults values
  if (is.null(prepare_train_function))
    prepare_train_function <- prepare_train_frame
  if (is.null(prepare_test_function))
    prepare_test_function <- prepare_test_frame
  if (is.null(tracker))
    tracker  <- task[["tracker"]]
  if (is.null(preprocessing_strategy))
    preprocessing_strategy <- "Target encoding with fte"

  set_verbose_level(task)
  task[["features"]] <- training_fields

  aux_prepare_train_or_test <- function(task, name){
    if (name %in% names(task)){
      if (name == "train_data"){
        logger::log_info("Preparing {name} for training")
        out <- prepare_train_function(
          data = task[[name]],
          training_fields = training_fields
        )
        task[[paste0("prepared_", name)]]  <- out[["data"]]
        task[["preparation_map"]]  <- out[["preparation_map"]]
      } else {
        logger::log_info("Preparing {name} for predicting")
        assertthat::assert_that(
          "preparation_map" %in% names(task),
          msg = 'No preparation map has been found. Have you loaded a task, or
          prepared the "train_data" first ?'
        )

        task[[paste0("prepared_", name)]]  <- prepare_test_function(
          data = task[[name]],
          preparation_map = task[["preparation_map"]],
          training_fields = training_fields
        )
      }
    } else {
      log_warn("There is no {name} in current task")
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
  prepare_train_function = NULL,
  prepare_test_function = NULL,
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
    tracker = tracker,
    preprocessing_strategy = preprocessing_strategy,
    training_fields = training_fields
  )
  return(task)
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
#'
#' @return `list(2)` \cr
#'   Une liste avec deux champs:
#'   * "data": `H2OFrame`; les données préparées
#'   * "preparation_map": `list(H2OFrame)`; la carte de target encoding
prepare_train_frame <- function(
  data_to_prepare,
  training_fields
  ){

  holdout_type <- "leave_one_out"
  prior_sample_size <- 30
  noise_level <- 0.02
  outcome <- "outcome"

  preparation_map <- fte::target_encode_create(
    data_to_encode = data_to_prepare,
    group_variables = list(
      c("code_naf"),
      c("code_ape_niveau2"),
      c("code_ape_niveau3")
      ),
    outcome_variable = outcome
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

  prepared_data <- prepare_data_shape(
    data_to_prepare = prepared_data,
    training_fields = training_fields
  )

  res <- list(
    data = prepared_data,
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
#' @param data_to_prepare `data.frame()` \cr Données à préparer
#' @param preparation_map `list(H2OFrame)`\cr Carte de calcul du target encoding. Utile
#'   uniquement si `test_or_train` = "test", sinon elle est recalculée.
#'
#' @return `list(2)` \cr
#'   Une liste avec deux champs:
#'   * "data": `H2OFrame`; les données préparées
#'   * "preparation_map": `list(H2OFrame)`; la carte de target encoding
prepare_test_frame <- function(
  data_to_prepare,
  preparation_map,
  training_fields
  ){

  holdout_type <- "none"
  prior_sample_size <- 30
  noise_level <- 0
  outcome <- "outcome"

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

  prepared_data <- prepare_data_shape(
    data_to_prepare = prepared_data,
    training_fields = training_fields
  )

  return(prepared_data)
}

#' Convertit la forme des données pour l'adapter aux exigences de l'algorithme
#'
#' 1- Filtre les champs pour l'entraînement.
#' 2- Convertit en matrice.
#'
#' @inheritParams prepare.sf_task
#' @param data_to_prepare :: `data.frame` \cr Données à préparer
#'
prepare_data_shape  <- function(data_to_prepare, training_fields){
  prepared_data <- data_to_prepare %>%
    dplyr::select(dplyr::one_of(training_fields))
  prepared_data <- as.matrix(prepared_data)
  return(prepared_data)
}
