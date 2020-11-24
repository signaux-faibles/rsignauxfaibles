#' Copie et nettoie une tâche pour un nouvel entraînement
#'
#' Retourne une tâche d'apprentissage où les éléments du précédent
#' apprentissage sont nettoyées.
#' Les données ne sont pas copiées tant qu'elles ne sont pas modifiées.
#' Attention, une incohérence entre "hist_data" et "mlr3task" ne sera pas
#' corrigée.
#'
#' @param task `sf_task`
#' @param end_run_status `c("FINISHED", "FAILED", "KILLED")` Si spécifié,
#' termine le précédent run de mlflow avec le statut donné. Sinon, ne termine
#' pas le précédent run.
#' @param keep_resampling `logical(1)` Faut-il garder l'échantillonnage de la
#' tâche mère ?
#'
#' @export
copy_for_new_run <- function(
                              task,
                              end_run_status = NULL,
                              keep_resampling = TRUE) {
  new_task <- sf_task(
    mongodb_uri = task$mongodb_uri,
    database = task$database,
    collection = task$collection,
    id = task$id,
    target = task$target,
    tracker = reset_tracker(task$tracker, end_run_status)
  )

  new_task$hist_data <- task$hist_data
  new_task$new_data <- task$new_data
  new_task$mlr3task <- task$mlr3task$clone()
  if (keep_resampling) {
    new_task$mlr3rsmp <- task$mlr3rsmp$clone(deep = TRUE)
  }

  return(new_task)
}

reset_tracker <- function(tracker, end_run_status) {
  if (class(tracker) == "mlflow_run") {
    if (!is.null(end_run_status)) {
      mlflow::mlflow_end_run(end_run_status)
    }
    return(mlflow::mlflow_start_run(experiment_id = tracker$experiment_id))
  }
  return(NULL)
}

#' Remplace les données historiques par d'autres données
#'
#' Reconstitue la tâche d'apprentissage task$mlr3task en conséquence.
#'
#' @inheritParams generic_task
#' @param data `data.frame()` Les données, doivent avoir des colonnes "siret",
#' "periode" et task[["target"]]
#'
#' @return La sf_task d'origine
#' @export
replace_hist_data <- function(task, data) {
  task <- load_hist_data(
    task,
    batch = NULL,
    fields = names(data),
    database_query_fun = mock_query_database(data)
  )
  return(task)
}

#' Remplace les données new_data par d'autres données
#'
#' @inheritParams generic_task
#' @param data `data.frame()` Les données, doivent avoir des colonnes "siret",
#' "periode" et task[["target"]]
#'
#' @describeIn replace_hist_data
#'
#' @return La sf_task d'origine
#' @export
replace_new_data <- function(task, data) {
  task <- load_new_data(
    task,
    periods = as.Date("2020-01-01"),
    batch = NULL,
    fields = names(data),
    database_query_fun = mock_query_database(data)
  )
  return(task)
}

#' Filtre la tâche d'apprentissage aux sirets désirés
#'
#' Attention, cette action réinitialise la tâche (échantillonnage,
#' entraînement).
#'
#' @inheritParams generic_task
#' @param sirets `character()` Sirets à conserver dans la tâche
#' d'apprentissage.
#'
#' @return Une copie de la tâche initiale, réinitialisée, et filtrée sur les
#' sirets demandés.
#'
#' @export
filter_task <- function(task, sirets) {
  # Filter mlr3task
  ids <- task$hist_data %>%
    filter(siret %in% sirets) %>%
    .$ids

  task[["hist_data"]] <- task[["hist_data"]] %>%
    filter(siret %in% sirets)

  # Filter hist_data
  task[["mlr3task"]]$filter(ids)
  reset_task <- copy_for_new_run(task, keep_resampling = FALSE)
  return(reset_task)
}
