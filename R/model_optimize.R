#' Optimize the hyperparameters of a model
#'
#' Optimise les hyperparamètres du modèle. Nécessite des données
#' d'entraînement et de validation préparées.
#'
#' @inheritParams generic_task
#' @param terminator `bbotk::Terminator` Condition d'arrêt de l'optimisation
#' des hyperparamètres
#' @param param_set `paradox::ParamSet` Les paramètres à optimiser
#' @param tuner `mlr3tuning::Tuner` La méthode d'optimisation des
#' hyperparamètres
#' @param measure `mlr3::Measure` La métrique à optimiser
#'
#'
#' @describeIn optimize_hyperparameters
#'
#' @return La `task` donnée en entrée, auquel a été ajouté un champ
#' "model_parameters" avec les paramètres optimaux calculées.
#' @export
#'
optimize_hyperparameters.sf_task <- function( # nolint
                                             task,
                                             terminator = bbotk::trm("evals", n_evals = 10),
                                             param_set = get_default_param_set(),
                                             tuner = mlr3tuning::tnr("random_search"),
                                             measure = get_default_measure()[[1]],
                                             ...) {
  requireNamespace("paradox")
  requireNamespace("mlr3tuning")

  resampling <- mlr3::rsmp("holdout")
  at <- mlr3tuning::AutoTuner$new(
    learner = task[["mlr3graph_learner"]],
    resampling = resampling,
    measure = measure,
    search_space = param_set,
    terminator = terminator,
    tuner = tuner
  )
  task[["mlr3auto_tuner"]] <- at$train(task$mlr3task)
  return(task)
}

#' Default xgboost parameters to be tuned
#'
#' This gives a default set of parameters to be tuned for the xgboost model.
#' To be used with `optimize_hyperparameters`.
#'
#' @return a paradox::ParamSet
#' @export
get_default_param_set <- function() {
  paradox::ParamSet$new(
    list(
      paradox::ParamDbl$new("classif.xgboost.eta", lower = 0.001, upper = 0.03),
      paradox::ParamDbl$new(
        "classif.xgboost.min_child_weight",
        lower = 0,
        upper = 100
      ),
      paradox::ParamDbl$new("classif.xgboost.gamma", lower = 0, upper = 0.03),
      paradox::ParamInt$new("classif.xgboost.max_depth", lower = 3, upper = 10)
    )
  )
}
