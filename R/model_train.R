#' Entraînement de l'algorithme
#'
#' Entraîne un algorithme sur les données `task[["train_data"]]`. Cf
#' `[train_light_gradient_boosting]` pour plus de détails sur le modèle
#' entraîné.
#'
#' @inheritParams generic_task
#' @param seed `integer()`\cr Graîne pour que les calculs aléatoires soient
#' reproductibles.
#' @param learner `mlr3::ClassifLearner` \cr
#'   Un modèle de classification mlr3 (les paramètres sont directement
#'   spécifiés dans le modèle).
#' @param store_models `logical(1)` \cr
#'   Faut-il sauvegardé tous les modèles entraînés pour référence ultérieure ?
#'
#' @return `[sf_task]` \cr L'objet `task` donné en entré, auquel a été ajouté
#' (ou écrasé) le champs "mlr3resample_result" si les données ont été
#' échantillonnées, "mlr3model" sinon, dans lequel est stocké un modèle
#' compatible avec la fonction `[predict.sf_task]`.
#'
#' @export
train.sf_task <- function( # nolint
                          task,
                          seed = 0,
                          learner = get_default_learner(),
                          store_models = TRUE,
                          ...) {

  # TODO: Log before training, to fail early if tracker should have been
  # changed

  requireNamespace("mlr3learners")
  requireNamespace("mlr3pipelines")

  lgr::lgr$info("Model is being trained.")
  full_pipeline <- task[["mlr3pipeline"]] %>>% learner


  graph_learner <- mlr3pipelines::GraphLearner$new(full_pipeline)
  graph_learner$predict_type <- "prob"
  graph_learner$predict_sets <- c("test", "train")
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
  lgr::lgr$info("Model trained_successfully")

  pipeops <- mlr3pipelines::as_graph(task[["mlr3graph_learner"]])$pipeops
  purrr::walk2(
    seq_along(pipeops),
    names(pipeops),
    ~ log_param(task, paste0("pipeline", .x), .y)
  )
  purrr::walk2(
    names(task[["mlr3graph_learner"]]$param_set$values),
    convert_to_character(task[["mlr3graph_learner"]]$param_set$values),
    ~ log_param(task, .x, .y)
  )
  log_param(task, "model_target", "18 mois, defaut et defaillance")
  return(invisible(task))
}

convert_to_character <- function(x) {
  if (typeof(x) == "closure") {
    return(paste0(as.character(quote(x))))
  } else {
    return(as.character(x))
  }
}

#' Get default mlr3 learner
#'
#' Returns a default learner to train.
#'
#' @export
get_default_learner <- function() {
  return(get_gam_learner())
}


#' Get a xgboost learner
#'
#' @return `mlr3::Learner`
get_xgboost_learner <- function() {
  require(mlr3learners)
  learner <- mlr3::lrn("classif.xgboost")
  learner$predict_type <- "prob"
  learner$param_set$values$max_depth <- 7
  learner$param_set$values$min_child_weight <- 20
  learner$param_set$values$ntreelimit <- 240
  learner$param_set$values$eta <- 0.01
  return(learner)
}

#' Get a generalized additive model learner
#'
#' @return `mlr3::Learner`
get_gam_learner <- function() {
  require(mlr3extralearners)
  # installed with
  # install_github("signaux-faibles/mlr3extralearners", ref = "feat/gam_learner")
  learner <- mlr3extralearners::LearnerClassifGam$new()
  return(learner)
}
