#' Predict from a trained model
#'
#' Predict on some data.
#' Data should be prepared.
#'
#' @param object `sf_task()` \cr Objet `sf_task` avec un modèle entraîné dans
#'   le champs `model`. Les données doivent avoir été préparées.
#' @param data_names `character()` \cr Nom des données sur lesquelles prédire.
#' @param predict_fun `model, new_data -> predictions` \cr Prediction function
#' @param ... Useless.
#'
#' @return `sf_task()`\cr
#'   Task donnée en entrée, pour laquelle une colonne "score" a été rajoutée
#'   aux données nommées dans `data_names`
#' @export
predict.sf_task <- function(
  object,
  data_names = c(
    "new_data",
    "train_data",
    "test_data"
    ),
  predict_fun = predict_model,
  ...
  ) {

  task  <- object
  set_verbose_level(task)

  assertthat::assert_that(all(c("model") %in% names(task)),
    msg = "Task should have a model to predict on new
    data")

    predict_on_given_data <- function(data_name, task) {

      prepared_data_name <- paste0("prepared_", data_name)
      if (!prepared_data_name %in% names(task)) {
        logger::log_warn("{data_name} is missing or has not been prepared yet")
        return(task)
      }

      logger::log_info("Model is being applied on {prepared_data_name}")

      prediction <- predict_fun(
        model = task[["model"]],
        new_data = task[[prepared_data_name]]
      )

      if (is.data.frame(prediction)) {
        dup_names <- intersect(names(prediction %>%
            dplyr::select(-siret, -periode)),
          names(task[[data_name]]))
        task[[data_name]] <- task[[data_name]] %>%
          dplyr::select(-one_of(dup_names))
        task[[data_name]] <- task[[data_name]] %>%
          left_join(prediction, by = c("siret", "periode"))
      } else {
        task[[data_name]][["score"]]  <- prediction
      }

      logger::log_info("Prediction successfully done.")
      return(task)
    }

    if (any(c("mlr3model", "mlr3resampled") %in% names(task))) {

      if (name == "test_data") {

        assertthat::assert_that("mlr3resampled" %in% names(task))
        task[["prediction_test"]] <- task[["mlr3resampled"]]$prediction()

      } else if (name == "train_data") {

        stop("Currently not supported")

      } else if (name == "new_data") {

        assertthat::assert_that("mlr3model" %in% names(task))
        task[["prediction_new"]] <- task[["mlr3model"]]$predict_newdata(
          task[["new_data"]]
        )

      }
    } else {
      for (name in data_names) {
        task  <- predict_on_given_data(name, task)
      }
    }
    return(task)
}

#' Predict model on new data for a cross-validated task
#'
#' Predict on new data for each cross-validated fold.
#'
#' @inheritParams predict.sf_task
#' @return A task where each cross-validated fold has predictions
#' @export
predict.cv_task <- function(
  object,
  data_names = c(
    "new_data",
    "train_data",
    "test_data"
    ),
  predict_fun = predict_model,
  ...
  ) {

  requireNamespace("purrr")
  task <- object
  task[["cross_validation"]] <- purrr::map(
    task[["cross_validation"]],
    predict.sf_task,
    data_names = data_names,
    predict_fun = predict_fun
  )
  return(task)
}

#' Prédiction du model
#'
#' Produit une prédiction du modèle sur de nouvelles données. Renvoie le
#' résultat converti sous forme de `data.frame()`
#'
#' @param model `H2OModel()` \cr
#'   Un modèle H2O.
#' @param new_data `H2OFrame()` \cr  Données au bon format pour alimenter le
#' modèle.
#'
#' @return `data.frame()` \cr
#'   Un data.frame avec les colonnes "predicted_outcome" (`TRUE` ou `FALSE`),
#'   "score" (entre 0 et 1), "siret" et "periode"
#' @export
predict_model <- function(model, new_data) {
  prediction <- predict(model, new_data)
  return(prediction)
}
