# Generic functions

#' Documentation tâche générique
#'
#' @param task `[sf_task]` \cr Objet s3 de type sf_task
#' @param tracker `[mllogr::Tracker]` \cr Logger of ML experiment
#' @param ... Not used
#' @name generic_task
NULL

#' Initialiser une tâche d'apprentissage
#'
#' Un objet s3 de type sf_task est défini, dans lequel seront défini et
#' stockés les tâches intermédiaires et les résultats de l'apprentissage.
#'
#' Si `tracker` est `NULL`, alors un nouveau Tracker est créé
#'
#' @param verbose `logical(1)` \cr
#'   Active ou désactive le log des actions ultérieures.
#' @inheritParams mongodb_connection
#' @inheritParams generic_task
#' @param experiment_name `character()` \cr
#'   Quel est l'objet de cette tâche ?
#' @param experiment_description `character()` \cr
#'   Descriptions supplémentaires sur l'expérimentation en cours.
#'
#' @return `[rsignauxfaibles::sf_task]` \cr
#'   Un objet sf_task avec un attribut de type `logical` "verbose", qui
#'   définit le niveau de log, ainsi qu'un attribut "to_log" de type `list`
#'   dans lequel seront stockés des informations spécifiques pour le log.
#'
#' @export
sf_task <- function(
  verbose,
  database = "test_signauxfaibles",
  collection = "Features",
  mongodb_uri,
  experiment_name,
  experiment_description,
  tracker = NULL
  ){

  res <- list(
    database = database,
    collection = collection,
    mongodb_uri = mongodb_uri
  )
  class(res) <- "sf_task"
  attr(res, "verbose") <- verbose
  if (is.null(tracker) && requireNamespace("MLlogr")){

    res[["tracker"]] <- MLlogr::Tracker$new(
      #TODO: Mieux gérer la base de logging
      # f4737679-916d-4aeb-84ba-958046f4ca31
      database = database,
      collection = collection,
      control = list(id_columns = c("siret", "periode"))
    )
    res[["name"]] <- experiment_name
    res[["tracker"]]$set(
      experiment_name = experiment_name,
      experiment_description = experiment_description
    )
  }
  return(res)
}

#' Active ou désactive le logging
#'
#' Prend en compte l'attribut "verbose" de l'objet task pour fixer le bon
#' niveau de logging
#' @param task `[sf_task]` \cr Objet s3 de type sf_task
#' @return TRUE
set_verbose_level <- function(task){
  requireNamespace("logger")
  if (attr(task, "verbose")){
    logger::log_threshold(logger::TRACE)
  } else {
    logger::log_threshold(logger::WARN)
  }
  return(TRUE)
}

#' Print sf_task
#'
#' @param x `sf_task` \cr
#' @param ... Useless
#'
#' @return invisible(x)
#' @export
print.sf_task <- function(x, ...){
  requireNamespace("purrr")
  cat("-- FIELDS --\n")
  aux_fun <- function(name, x){
    if (!is.character(x) || length(x) > 1){
      cat(paste0("  * ", name, " (", paste0(class(x), collapse = ", "), ")\n"))
    } else {
      cat(paste0("  * ", name, " : ", x, "\n"))
    }
  }
  purrr::walk2(names(x), x, aux_fun)

  cat("-- INFO --\n")
  purrr::walk2(
    names(x[["tracker"]]$values),
    x[["tracker"]]$values,
    aux_fun
    )
  return(invisible(x))
}



#' Vérification de champs
#'
#' Vérifie si les champs qui vont être écrits sont déjà existant, et le cas
#' échéant vont être écrasés.
#' @param task `[sf_task]` \cr Objet s3 de type sf_task
#' @param field_names `character()` \cr Nom des champs à vérifier.
#' @return Nom des champs écrasés, `character(0)` sinon.
check_overwrites <- function(task, field_names){
  set_verbose_level(task)
  overwrite <- intersect(field_names, names(task))
  if (length(overwrite) > 1) {
    logger::log_info(
      'Les champs {paste(overwrite, collapse = ",")} sont ecrases avec
      les nouvelles valeurs.'
    )
  }
  return(overwrite)
}

#' Chargement de données historiques
#'
#' @param task `[sf_task]` \cr Objet s3 de type sf_task
#' @param ... Additional parameters depending on class of task
#' @export
load_hist_data <- function(task, ...) {
  UseMethod("load_hist_data", task)
}

#' Loads new data
#' @inheritParams load_hist_data
#' @export
load_new_data <- function(task, ...) {
  UseMethod("load_new_data", task)
}

#' Splits data
#' @inheritParams load_hist_data
#' @export
split_data <- function(task, ...) {
  UseMethod("split_data", task)
}

#' Prepare data
#' @inheritParams load_hist_data
#' @export
prepare <- function(task, ...) {
  UseMethod("prepare", task)
}

#' Optimize hyperparameters
#' @inheritParams load_hist_data
#' @export
optimize_hyperparameters <- function(task, ...) {
  UseMethod("optimize_hyperparameters", task)
}

#' Trains model on data
#' @inheritParams load_hist_data
#' @export
train <- function(task, ...) {
  UseMethod("train", task)
}

#' Loads task
#' @inheritParams load_hist_data
#' @export
load <- function(task, ...) {
  UseMethod("load", task)
}

load.default <- function(task, ...) {
  base::load(task, ...)
}

#' Saves task
#' @inheritParams load_hist_data
#' @export
save <- function(task, ...) {
  UseMethod("save", task)
}

save.default <- function(task, ...) {
  base::save(task, ...)
}

#' Exports data
#'
#' @inheritParams load_hist_data
#' @export
export <- function(task, ...) {
  UseMethod("export", task)
}

#' Log a machine learning experiment
#'
#' @inheritParams load_hist_data
#' @export
log_experiment <- function(task, ...) {
  UseMethod("log_experiment", task)
}

#' Explains model results
#' @inheritParams load_hist_data
#' @export
explain <- function(task, ...) {
  UseMethod("explain", task)
}
