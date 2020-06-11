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
  seed = 0,
  learner = get_default_learner(),
  store_models = FALSE,
  ...
  ) {

  require(mlr3learners)

  set_verbose_level(task)

  logger::log_info("Model is being trained.")

  graph_learner <- mlr3pipelines::GraphLearner$new(
    task[["mlr3pipeline"]] %>>% learner
  )
  graph_learner$predict_type <- "prob"
  task[["mlr3graph_learner"]] <- graph_learner

  if ("mlr3rsmp" %in% names(task)) {
    task[["mlr3resample_result"]] <- mlr3::resample(
      task = task[["mlr3task"]],
      learner = task[["mlr3graph_learner"]],
      resampling = task[["mlr3rsmp"]],
      store_models = store_models
    )
  } else {
    task[["mlr3model"]] <- graph_learner$train(task[["mlr3task"]])
  }
  logger::log_info("Model trained_successfully")
  # TODO deal with logging
  log_param(task, "model_name",  "light gradient boosting")
  # log_param(task, "model_parameters", parameters)
  log_param(task, "model_target",  "18 mois, defaut et defaillance")
  return(invisible(task))
}

get_default_learner <- function() {
  learner <- mlr3::lrn("classif.xgboost")
  learner$predict_type <- "prob"
  return(learner)
}
