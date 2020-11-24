#' Préparation des échantillons pour l'entraînement et l'évaluation
#'
#' Prépare les données pour l'entraînement ou la prédiction de l'algorithme.
#'
#' @inheritParams generic_task
#' @param processing_pipeline `mlr3pipelines::PipeOp` \cr
#'   Pipeline de préparation des données
#' @param outcome_field `character(1)` \cr
#'   Champ de la variable à prédire.
#' @param training_fields `character()` \cr
#'   Les champs qui doivent être conservées pour l'entraînement.
#' @param ... Unused.
#'
#' @return `[sf_task]` \cr
#'   L'objet \code{task} donné en entrée auquel le champ "mlr3pipeline" a été
#'   ajouté. Cette pipeline ne sera appliquée qu'au moment de l'entraînement,
#'   ou de l'utilisation de `get_prepared_data()`.
#'
#' @export
prepare.sf_task <- function( # nolint
                            task,
                            training_fields = get_fields(training = TRUE),
                            outcome_field = NULL,
                            processing_pipeline = get_default_pipeline(),
                            ...) {
  task[["training_fields"]] <- training_fields

  if (is.null(outcome_field)) {
    task[["outcome_field"]] <- task[["target"]]
  } else {
    task[["outcome_field"]] <- outcome_field
    task[["mlr3task"]]$col_roles$target <- outcome_field
  }

  processing_pipeline <- mlr3pipelines::as_graph(processing_pipeline)
  task[["mlr3pipeline"]] <- processing_pipeline

  task[["mlr3task"]]$col_roles$feature <- training_fields

  return(task)
}

#' Creates a PipeOp for impact encoding
#'
#' @param target_encode_fields `character()` Name of fields to "feature target
#' encode (fte)" (or "impact encode")
#'
#' @return `mlr3pipelines::PipeOp`
create_fte_pipeline <- function(target_encode_fields) {
  poe <- mlr3pipelines::po("encodeimpact",
    param_vals = list(
      affect_columns = mlr3pipelines::selector_name(target_encode_fields)
    )
  )
}

#' Construct default pipeline
#'
#' @export
get_default_pipeline <- function() {
  pipeline <- create_fte_pipeline(
    c("code_ape_niveau2", "code_ape_niveau3")
  ) %>>%
    mlr3pipelines::po(
      "encode",
      method = "treatment",
      affect_columns = mlr3pipelines::selector_type("factor")
    )
  return(pipeline)
}

#' Apply preparation pipeline and inspect prepared data
#'
#' Applique la pipeline de préparation sur les données d'entraînement ou de
#' test (que le premier échantillon en cas de validation croisée).
#'
#' Cette fonction est uniquement prévue pour l'inspection du bon
#' fonctionnement de la pipeline de préparation.
#'
#' L'objet task doit avoir une propriété "mlr3pipeline" de type
#' `mlr3pipelines::PipeOp` ou `mlr3pipelines::Graph`
#'
#' @inheritParams generic_task
#' @param data `data.frame` données à préparer
#'
#' @return `data.frame` données passées en entrée après la préparation
#'
#' @export
get_prepared_data <- function(task, data) {
  assertthat::assert_that(
    "mlr3pipeline" %in% names(task),
    msg = "A pipeline is needed to get prepared data (property: mlr3pipeline)"
  )
  if (!"mlr3rsmp" %in% names(task)) {
    train_ids <- task[["mlr3task"]]$row_ids
  } else {
    train_ids <- task[["mlr3rsmp"]]$train_set(1)
  }

  gpo <- task[["mlr3pipeline"]]
  gpo$train(task[["mlr3task"]]$clone()$filter(train_ids))
  data_ids <- task$mlr3task$nrow + seq_len(nrow(data))

  new_data_task <- task[["mlr3task"]]$
    clone()$
    rbind(data)$
    filter(data_ids)
  pred <- gpo$predict(new_data_task)[[1]]
  pred <- as.data.frame(pred$data())

  return(pred)
}
