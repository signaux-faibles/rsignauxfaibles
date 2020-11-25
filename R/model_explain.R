#' Local explanation of a single prediction
#'
#' @inheritParams generic_task
#' @param explanation_method `function` Fonction qui prend en paramètre:
#' - "task": `sf_task` la tâche d'apprentissage
#' - "data_to_explain": `data.frame | data.table`  données à expliquer
#' @param data_to_explain `data.frame`
#' @param ... Paramètres additionnels passés à `explanation_method`
#' @export
explain.sf_task <- function( #nolint
                            task,
                            data_to_explain,
                            explanation_method = get_default_explanation_method(),
                            ...) {
  explanation_method <- get_default_explanation_method()

  return(explanation_method(task, data_to_explain, ...))
}

#' Get default explanation method
#'
#' @inheritParams explain.sf_task
#'
#' @return `function`
#' @export
get_default_explanation_method <- function(task, data_to_explain) {
   return(explain_gam)
}


#' Get explanations for a gam model
#'
#' @inheritParams explain.sf_task
#' @param data_to_explain `data.frame`
#'
#' @return
#' @export
explain_gam <- function(task, data_to_explain) {
  prepared_data <- get_prepared_data(task, data_to_explain)
  gam_model <- get_gam_from_task(task)
  explanation <- predict(gam_model, prepared_data, type = "terms")
  return(explanation)
}


#' Fetches the trained gam model inside a task
#'
#' @inheritParams generic_task
#'
#' @return `mgcv::gam` gam model
get_gam_from_task <- function(task) {
  if ("mlr3resample_result" %in% names(task)) {
    graphlearner <- task$mlr3resample_result$learners[[1]]
    gam_model <- graphlearner$model$classif.gam$model
  } else if ("mlr3model" %in% names(task)) {
    gam_model <- task$mlr3model$model$classig.gam$model
  } else {
    stop('model should have created "mlr3resample_result" or "mlr3model" property in task during training')
  }
  return(gam_model)
}
