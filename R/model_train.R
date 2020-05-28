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
#' @param train_fun `training_data` \cr Training_function, or "xgboost" or
#' "linear"
#' @param outcome `character()` \cr Nom de la colonne dans laquelle on trouve
#' l'objectif d'apprentissage.
#'
#'
#' @return `[sf_task]` \cr L'objet `task` donné en entré, auquel a été ajouté
#' (ou écrasé) le champs "model", dans lequel est stocké un modèle compatible
#' avec la fonction `[prepare.sf_task]`.
#'
#' @export
train.sf_task <- function( #nolint
  task,
  ## Should not be needed here
  outcome = "outcome",
  parameters = NULL,
  seed = 0,
  ## Neither should this
  train_fun = "xgboost",
  learner = NULL,
  ...
  ) {

  admissible_train_types <- c("xgboost", "linear")
  if (is.character(train_fun)) {
    assertthat::assert_that(
      length(train_fun) == 1 &&
        train_fun %in% admissible_train_types
    )
    train_fun <- switch(
      train_fun,
      xgboost = train_xgboost,
      linear = train_linear
    )
  }

  assertthat::assert_that(
    "prepared_train_data" %in% names(task),
    msg = "task does not contain prepared train data."
  )

  if (is.null(parameters)) {
    parameters <- task[["model_parameters"]]
  }

  assertthat::assert_that(
    is.list(parameters)
  )

  set_verbose_level(task)

  logger::log_info("Model is being trained.")

  if (is.null(learner)) {

    assertthat::assert_that(
      all(c("learn_rate", "max_depth", "ntrees", "min_child_weight") %in%
        names(parameters)),
      msg = paste("Following parameters are missing: ",
        dplyr::setdiff(c("learn_rate", "max_depth", "ntrees", "min_child_weight"),
          names(parameters)),
        sep = ", ")
    )

    outcome <- task[["train_data"]][[task[["outcome_field"]]]]
    model <- train_fun(
      train_data = task[["prepared_train_data"]],
      outcome = outcome,
      learn_rate = parameters[["learn_rate"]],
      max_depth = parameters[["max_depth"]],
      ntrees = parameters[["ntrees"]],
      min_child_weight = parameters[["min_child_weight"]],
      seed = seed
    )

    logger::log_info("Model trained_successfully")
    task[["model"]] <- model

    log_param(task, "model_name",  "light gradient boosting")
    log_param(task, "model_parameters", parameters)
    log_param(task, "model_target",  "18 mois, defaut et defaillance")

  } else {
    possible_parameters <- learner$param_set %>% data.table::as.data.table() %>% .[, id] #nolint
    assertthat::assert_that(
      all(names(parameters) %in% possible_parameters),
      msg = paste("Following parameters are missing: ",
        names(parameters)[!names(parameters) %in% possible_parameters],
        sep = ", ")
    )

    task[["mlr3graphlearner"]] <- mlr3pipelines::GraphLearner$new(task[["mlr3pipeline"]] %>>% learner) #nolint
    task[["mlr3resampled"]] <- mlr3::resample(
      task = task[["mlr3task"]],
      learner = task[["mlr3graphlearner"]],
      resampling = task[["mlr3rsmp"]]
    )
    task[["model"]] <- task[["mlr3resampled"]]
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
  seed = 0,
  train_fun = "xgboost",
  ...
  ) {

  requireNamespace("purrr")
  if (is.null(parameters) && "model_parameters" %in% names(task)) {
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
#' @param train_data `data.frame` \cr données d'entraînement, sous la forme
#'   d'un H2OFrame
#' @param test_data `H2OFrame` \cr Données d'évaluation, sous la forme
#'   d'un H2OFrame
#' @param outcome `character(1)` \cr Nom de la variable qui sert de cible
#'   d'apprentissage
#' @param parameters `list()` \cr list of parameters with fields "learn_rate",
#' "max_depth", "ntrees", "min_child_weight".
#' @param seed `integer(1)` \cr Graine aléatoire pour que les opérations
#'   aléatoires soient reproductibles.
#'
#' @return `H2OBinomialModel` \cr
#' @export
train_xgboost <- function(
  train_data,
  outcome,
  test_data = NULL,
  learn_rate = 0.1,
  max_depth = 4,
  ntrees = 60,
  min_child_weight = 1,
  seed
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
      max_depth = max_depth,
      min_child_weight = min_child_weight,
      objective = "binary:logistic"
      ),
    nrounds = ntrees
  )
  return(model)
}

#' Train a linear model
#'
#' Trains a linear regularized model.
#'
#' @param train_data `data.frame` \cr données d'entraînement, sous la forme
#'   d'un H2OFrame
#' @param test_data `data.frame` \cr Données d'évaluation, sous la forme
#'   d'un H2OFrame
#' @param outcome `character(1)` \cr Nom de la variable qui sert de cible
#'   d'apprentissage
#' @param parameters `list()` \cr Paramètres de la régression logistique.
#' @param seed `integer(1)` \cr Graine aléatoire pour que les opérations
#'   aléatoires soient reproductibles.
#'
#' @export
train_linear <- function(
  train_data,
  outcome,
  test_data = NULL,
  parameters,
  seed = 123
  ) {

  #
  # Train the model
  #
  model <- glmnet::glmnet(
    x = train_data,
    y = outcome,
    family = "binomial"
  )
  return(model)
}
