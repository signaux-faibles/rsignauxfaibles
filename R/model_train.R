#' Entraînement de l'algorithme
#'
#' Entraîne un algorithme sur les données `task[["train_data"]]`. Cf
#' `[train_light_gradient_boosting]` pour plus de détails sur le modèle
#' entraîné.
#'
#' @inheritParams generic_task
#' @param parameters `list(any())` \cr Paramètres du modèle. Cf
#' `[train_light_gradient_boosting]`. Si `NULL`, alors des paramètres par
#' défaut sont sélectionnés.
#' @param seed `integer()`\cr Graîne pour que les calculs aléatoires soient
#' reproductibles.
#' @param train_fun `training_data` \cr Training_function
#' @param outcome `character()` \cr Nom de la colonne dans laquelle on trouve
#' l'objectif d'apprentissage.
#'
#'
#' @return `[sf_task]` \cr L'objet `task` donné en entré, auquel a été ajouté
#' (ou écrasé) le champs "model", dans lequel est stocké un modèle compatible
#' avec la fonction `[prepare.sf_task]`.
#'
#' @export
train.sf_task <- function(
  task,
  outcome = "outcome",
  parameters = NULL,
  seed = 123,
  tracker = task[["tracker"]],
  train_fun = train_xgboost,
  ...
  ){

  assertthat::assert_that(
    "prepared_train_data" %in% names(task),
    msg = "task does not contain prepared train data."
    )

  if (is.null(parameters) &&
    (!"model_parameters" %in% names(task))){
    parameters  <- list(
      learn_rate = 0.1,
      max_depth = 4,
      ntrees = 60,
      min_child_weight = 1
      )

    task[["model_parameters"]] <- parameters
  } else if (is.null(parameters)){
    parameters <- task[["model_parameters"]]
  }

  assertthat::assert_that(
    is.list(parameters)
    )

  assertthat::assert_that(
    all(c("learn_rate", "max_depth", "ntrees", "min_child_weight") %in%
      names(parameters)),
    msg = paste("Following parameters are missing: ",
      dplyr::setdiff(c("learn_rate", "max_depth", "ntrees", "min_child_weight"),
      names(parameters)),
      sep = ", ")
    )

  set_verbose_level(task)

  logger::log_info("Model is being trained.")

  model <- train_fun(
    train_data = task[["prepared_train_data"]],
    outcome = task[["train_data"]][, outcome],
    learn_rate = parameters[["learn_rate"]],
    max_depth = parameters[["max_depth"]],
    ntrees = parameters[["ntrees"]],
    min_child_weight = parameters[["min_child_weight"]]
    )

  logger::log_info("Model trained_successfully")
  task[["model"]] <- model

  if (!is.null(tracker)){
    #TODO: Why does it print "model"?
    # e51f22ca-ed0d-434b-be1e-d57547092a22
     tracker$set(
       model_name  = "light gradient boosting",
       model  = "model",
       model_target  = "18 mois, defaut et defaillance"
       )
  }

  return(invisible(task))
}

#' Train a model on a cross-validated task
#'
#' Train models for each subtask, i.e. for each cross-validation fold.
#'
#' @inheritParams train.sf_task
#'
#' @return The input task, where each cv-subtask has a trained model
#' @export
train.cv_task  <- function(
  task,
  outcome = "outcome",
  parameters = NULL,
  seed = 123,
  tracker = task[["tracker"]],
  train_fun = train_xgboost,
  ...
  ){

  requireNamespace("purrr")
  if (is.null(parameters) && "model_parameters" %in% names(task)){
    parameters <- task[["model_parameters"]]
  }

  task[["cross_validation"]]  <- purrr::map(
    task[["cross_validation"]],
    train.sf_task,
    outcome = outcome,
    parameters = parameters,
    tracker = tracker,
    train_fun = train_fun,
    seed = seed
    )

  return(task)
}

#' Train an xgboost model
#'
#' Trains a light gradient boosting model on training data.
#'
#' @param train_data `data.frame` \cr données d'entraînement, sous la forme d'un H2OFrame
#' @param validation_data `H2OFrame` \cr Données d'évaluation, sous la forme d'un H2OFrame
#' @param outcome `character(1)` \cr Nom de la variable qui sert de cible
#'   d'apprentissage
#' @param learn_rate `numeric(1)` \cr
#' @param max_depth `integer(1)` \cr  Specify the maximum tree depth. Higher
#'   values will make the model more complex and can lead to overfitting.
#'   Setting this value to 0 specifies no limit.
#' @param ntrees `integer(1)` \cr Specify the number of trees to build.
#' @param min_child_weight `integer(1)` \cr Specify the minimum number of
#' observations for a leaf.
#' @param seed `integer(1)` \cr Graine aléatoire pour que les opérations
#'   aléatoires soient reproductibles.
#'
#' @return `H2OBinomialModel` \cr
#' @export
train_xgboost <- function(
  train_data,
  outcome,
  validation_data = NULL,
  learn_rate = 0.1,
  max_depth = 4,
  ntrees = 60,
  min_child_weight = 1,
  seed = 123
  ) {

  #
  # Train the model
  #
  set.seed(seed)
  model <- xgboost::xgboost(
    data = train_data,
    label = outcome,
    params = list(
      eta = learn_rate,
      max_depth = 4,
      min_child_weight = min_child_weight,
      objective = "binary:logistic"
      ),
    nrounds = ntrees
  )
  return(model)
}
