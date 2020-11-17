#' Évaluation du modèle
#'
#' Evalue les prédictions d'un ou plusieurs tasks, et retourne la performance
#' calculée sur l'échantillon (ou les échantillons dans le cadre de la
#' validatio croisée) de test.
#'
#' @param ... `tasks` \cr Tasks to be evaluated.
#' @param measures `mlr3::Measure` Vecteur de mesures utilisées pour évaluer
#' la perforlance.
#' @param should_remove_strong_signals :: `logical(1)`\cr Faut-il retirer des
#' échantillons de test les entrerprises qui présentent des
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
                     measures = get_default_measure(),
                     should_remove_strong_signals = TRUE) {
  tasks <- list(...)
  purrr::walk(tasks, check_resample_results)
  assertthat::assert_that(length(tasks) >= 1)
  resample_results <- purrr::map(tasks, "mlr3resample_result")

  if (should_remove_strong_signals) {
    stop("This functionality needs to be repaired")
    lgr::lgr$info(
      "Les 'signaux forts' sont retires des donnees d'evaluation (test, validation)"
    )
    resample_results <- purrr::map(tasks, remove_strong_signals)
  }
  benchmark <- do.call(c, resample_results)
  # Automatically converted to BenchmarkResult
  evaluation <- benchmark$aggregate(measures = measures)

  for (i in seq_len(nrow(evaluation))) {
    purrr::walk(
      7:length(evaluation),
      ~ log_metric(
        tasks[[i]],
        paste0(
          names(evaluation)[.],
          ifelse(should_remove_strong_signals, ".weaksignals", "")
        ),
        evaluation[i][[.]]
      )
    )
  }
  return(evaluation)
}

#' Get defaults mlr3 measures.
#'
#' @export
get_default_measure <- function() {
  return(mlr3::msrs(c("classif.fbeta", "classif.ce")))
}

remove_strong_signals <- function(task) {
  filtered_resample_results <- task$mlr3resample_result$clone()
  assertthat::assert_that("time_til_outcome" %in% names(task[["hist_data"]]))
  weak_rows <- task[["hist_data"]] %>%
    mutate(row_id = 1:n()) %>%
    dplyr::filter(is.na(time_til_outcome) | time_til_outcome > 0) %>%
    .$row_id

  filtered_resample_results$data$prediction <- purrr::map(
    filtered_resample_results$data$prediction,
    ~ list(test = filter_mlr3_prediction(.$test, weak_rows))
  )

  return(filtered_resample_results)
}

check_resample_results <- function(task) {
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

filter_mlr3_prediction <- function(prediction, rows) {
  prediction_dt <- data.table::as.data.table(prediction)
  real_rows <- rows[rows %in% prediction$row_ids]
  filter <- prediction_dt$row_id %in% real_rows
  prediction_filtered_dt <- prediction_dt[filter, ]
  prediction_filtered <- mlr3::PredictionClassif$new(
    row_ids = real_rows,
    truth = prediction_filtered_dt$truth,
    response = prediction_filtered_dt$response
  )
  return(prediction_filtered)
}
