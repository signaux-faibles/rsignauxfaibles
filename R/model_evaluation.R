#' Évaluation du modèle
#'
#' Evalue les prédictions d'un ou plusieurs tasks.
#' Stocke le résultat dans le premier task.
#'
#' @param ... `tasks` \cr Tasks to be evaluated.
#' @param eval_function `MLsegmentr::eval_function()` \cr Objet s3
#' d'évaluation.
#' @param data_name `character(1)` \cr Sur quelles données évaluer ?
#' @param plot `logical(1)` \cr Faut-il tracer la figure avec la fonction de
#'   l'`eval_function` (si disponible)
#' @param prediction_names `character(1)` Name of columns containing
#'   predictions. Default value should be correct, advanced setting to use
#'   with care.
#' @param target_names :: character() \cr Nom de la colonne qui contient
#'   l'objectif d'apprentissage.
#' @param segment_names :: character() \cr Nom de la colonne qui permet de
#'   segmenter l'évaluation.
#' @param remove_strong_signals :: `logical(1)`\cr Faut-il retirer des
#' échantillons de test ou de validation les entrerprises qui présentent des
#' signaux forts, c'est-à-dire 3 mois de défaut, ou une procédure collective
#' en cours ? Nécessite que les données contenues dans
#' \code{task[["hist_data"]]} possèdent le champs "time_til_outcome".
#'
#' @return `task` donnée en entrée à laquelle s'est ajoutée (ou a été
#' remplacé) un champs "model_performance", avec le résultat de la fonction
#' d'évaluation
#' @export
evaluate <- function(
  ...,
  measures =  get_default_measure(),
  data_name = "test_data",
  should_remove_strong_signals = TRUE
  ) {

  tasks <- list(...)
  purrr::walk(tasks, check_resample_results)
  assertthat::assert_that(length(tasks) >= 1)
  assertthat::assert_that(
    length(data_name) == 1,
    msg = paste0("Evaluation can only be made on a single data type",
      "(new, test) at once",
      sep = " "
      )
  )

  resample_results <- purrr::map(tasks, "mlr3resample_result")
  if (should_remove_strong_signals) {
    logger::log_info("Les 'signaux forts' sont retires des donnees
      d'evaluation (test, validation)")
      resample_results <- purrr::map(resample_results, remove_strong_signals)
  }
  benchmark <- do.call(c, resample_results)
  evaluation <- benchmark$aggregate(measures = measures)

  for (i in seq_len(nrow(evaluation))) {
    purrr::walk(
      7:length(evaluation),
      ~ log_metric(tasks[[i]], names(evaluation)[.], evaluation[i][[.]])
    )
  }
  return(evaluation)
}

get_default_measure <- function() {
  return(msrs(c("classif.ce", "classif.fbeta")))
}

remove_strong_signals <- function(
  resample_result
  ) {
  filtered_resample_results <- resample_result

 return(filtered_resample_results)
 # TODO: implement
  # assertthat::assert_that("time_til_outcome" %in% names(evaluation_data))
  # evaluation_data  <- evaluation_data %>%
  #   dplyr::filter(is.na(time_til_outcome) | time_til_outcome > 0)
}

check_resample_results <- function(
  task
  ) {
  assertthat::assert_that(
    "mlr3resample_result" %in% names(task),
    msg = paste0(
      "to be evaluated, a task must have a resampling strategy",
      "and the model must be trained, thus having a mlr3resample_result",
      "property",
      sep = " "
    )
  )
  assertthat::assert_that(
    inherits(task[["mlr3resample_result"]], "ResampleResult"),
    msg = "mlr3resample_result property should inherit from `ResampleResult`"
    )
}
