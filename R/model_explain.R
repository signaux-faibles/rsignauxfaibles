#' Local explanation of a single prediction
#'
#' @inheritParams generic_task
#' @param explanation_method `function` Fonction qui prend en paramètre:
#' - "task": `sf_task` la tâche d'apprentissage
#' - "data_to_explain": `data.frame | data.table`  données à expliquer
#' @param ... Paramètres additionnels passés à `explanation_method`
#' @export
explain.sf_task <- function( #nolint
                            task,
                            data_to_explain,
                            explanation_method = get_default_explanation_method(),
                            ...) {
  explanation_method <- get_default_explanation_method()

  return(explanation_method(task$mlr3graph_learner, data_to_explain))
}



