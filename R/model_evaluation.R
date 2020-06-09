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
  measures =  NULL,
  data_name = "test_data",
  should_remove_strong_signals = TRUE
  ) {

  tasks <- list(...)
  purrr::walk(tasks, check_resample_results)
  assertthat::assert_that(length(tasks) >= 1)
  assertthat::assert_that(
    length(data_name) == 1,
    msg = "Evaluation can only be made on a single data type (new, test) at once"
  )

  resample_results <- purrr::map(tasks, "mlr3resampled")
  if (should_remove_strong_signals) {
    logger::log_info("Les 'signaux forts' sont retires des donnees
      d'evaluation (test, validation)")
      resample_results <- purrr::map(resample_results, remove_strong_signals)
  }
  benchmark <- do.call(c, resample_results)
  return(benchmark$aggregate(measures = measures))
}

remove_strong_signals <- function(
  resample_result
  ) {
  filtered_resample_results <- resample_result

 return(filtered_resample_results)
  # assertthat::assert_that("time_til_outcome" %in% names(evaluation_data))
  # evaluation_data  <- evaluation_data %>%
  #   dplyr::filter(is.na(time_til_outcome) | time_til_outcome > 0)
}

check_resample_results <- function(
  task
  ) {
  assertthat::assert_that(
    "mlr3resampled" %in% names(task),
    msg = paste0(
      "to be evaluated, a task must have a resampling strategy",
      "and the model must be trained, thus having a mlr3resampled property"
    )
  )
  assertthat::assert_that(
    inherits(task[["mlr3resampled"]], "ResampleResult"),
    msg = "mlr3resampled property should inherit from `ResampleResult`"
    )
}


  #   requireNamespace("MLsegmentr")
  #   assesser <- MLsegmentr::Assesser$new(
  #     evaluation_data
  #   )
  #   assesser$set_predictions(prediction_names)
  #   assesser$set_targets(target_names)
  #   if (!is.null(segment_names)) {
  #     assesser$set_segments(segment_names)
  #   }
  #   assesser$evaluation_funs <- eval_function

  #   aux <- assesser$assess_model(plot = plot)

  #   task <- tasks[[1]]

  #   perf  <- aux[["performance_frame"]]
  #   task[["plot"]] <- aux[["plot"]]

  #   if (default_eval_fun) {
  #     task[["model_performance"]] <- perf %>%
  #       filter(evaluation_name != "prcurve")
  #   } else {
  #     task[["model_performance"]] <- perf
  #   }
  # } else {
  #   assertthat::assert_that("mlr3prediction_test" %in% names(task))
  #   task[["model_performance"]] <- task[["mlr3prediction_test"]]$score(measures)
  #   # TODO: filter strong signals
  # }

  # log_metric(task, "model_performance", task[["model_performance"]])

  # return(task)
  # ## Log model performance.
# }

#' Consolidate several tasks into a frame for evaluation
#'
#' @param ... `sf_tasks | cv_tasks` \cr Tasks to consolidate
#' @param data_name `character()` \cr Name of the type of data to consolidate
#' ("train_data", "test_data")
#'
# consolidate <- function(..., data_name) {
#   postfix <- ".cv"
#   tasks <- list(...)
#   assertthat::assert_that(length(tasks) >= 1)
#   if (length(tasks) == 1) {
#     task <- tasks[[1]]
#     name <- make.names(task[["name"]])
#     if (inherits(task, "cv_task")) {
#       # Recursively consolidate cross_validated tasks.
#       names <- paste0(
#         name,
#         postfix,
#         seq_along(task[["cross_validation"]])
#       )
#       task[["cross_validation"]] <- purrr::map2(
#         task[["cross_validation"]],
#         names,
#         function(my_task, my_name) {
#           my_task[["name"]] <- my_name
#           return(my_task)
#         }
#       )
#       consolidated_frame <- purrr::invoke(
#         consolidate,
#         task[["cross_validation"]],
#         data_name = data_name
#       )
#     } else if (inherits(task, "sf_task")) {
#       consolidated_frame <- task[[data_name]] %>%
#         rename(!!name := score)
#     } else {
#       stop("Class not handled by consolidate function")
#     }
#   } else {
#     # Rename tasks to have valid names
#     names <- make.names(purrr::map(tasks, "name"), unique = TRUE)
#     tasks <- purrr::map2(
#       tasks,
#       names,
#       function(my_task, my_name) {
#         my_task[["name"]] <- my_name
#         return(my_task)
#       }
#     )
#     # Computed consolidated frames for each task
#     consolidated_tasks <- purrr::map(
#       tasks,
#       consolidate,
#       data_name = data_name
#     )
#     # Consolidate at last
#     consolidated_frame <- purrr::invoke(
#       join_frames,
#       consolidated_tasks
#     )
#   }
#   return(consolidated_frame)
# }


#' Join several frames
#'
#'
#' Joins the frames by all fields that are present in all the frames.
#'
#' @param ... :: `data.frame` \cr Frames to join
#'
#' @return `data.frame` \cr Joined frame
# join_frames <- function(
#   ...
#   ) {
#   list_data <- list(...)
#   assertthat::assert_that(length(list_data) >= 1)

#   shared_names <- purrr::reduce(
#     list_data,
#     ~ intersect(.x, names(.y)),
#     .init = names(list_data[[1]])
#   )

#   assertthat::assert_that(length(shared_names) > 1)

#   combined_data  <- purrr::reduce(
#     list_data,
#     dplyr::full_join,
#     by = shared_names
#   )
#   return(combined_data)
# }
