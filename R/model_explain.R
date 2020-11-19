#' Local explanation of a single prediction
#'
#' @inheritParams generic_task
#' @param explanation_method `function` Fonction qui prend en paramètre:
#' - "model": `any` le modèle
#' - "data_to_explain": `data.frame | data.table`  données à expliquer
#' @param ... Paramètres additionnels passés à `explanation_method`
#' @export
explain.sf_task <- function(explanation_method, ...) {

}

