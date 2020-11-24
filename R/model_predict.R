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
                            ...) {
  task <- object
  assertthat::assert_that(
    all(data_names %in% c("new_data", "train_data", "test_data"))
  )

  if (any(c("mlr3model", "mlr3resample_result") %in% names(task))) {
    if ("test_data" %in% data_names) {
      assertthat::assert_that("mlr3resample_result" %in% names(task))
      task[["prediction_test"]] <- task[["mlr3resample_result"]]$prediction()
    } else if ("train_data" %in% data_names) {
      stop("Currently not supported")
    } else if ("new_data" %in% data_names) {
      assertthat::assert_that("mlr3model" %in% names(task))
      task[["prediction_new"]] <- task[["mlr3model"]]$predict_newdata(
        task[["new_data"]]
      )
    }
  }
  return(task)
}

#' Applique des corrections à la prédiction de l'apprentissage automatique
#'
#' Après avoir prédit sur de nouvelles données, certaines corrections a
#' posteriori sont appliquées à la prédiction. Les corrections se présentent
#' comme des termes supplémentaires dans l'espace des log-vraisemblances.
#'
#' @param correction_debt `data.frame` Corrections liées aux entreprises ayant
#' des débits sur les cotisations sociales. Colonnes "siret" et "correction_debt".
#' @param correction_sector `data.frame` Corrections liées aux secteurs
#' d'activité selon qu'ils soient plus ou moins touchés par la crise.
#' Colonnes "secteur" et "correction_sector".
#'
#' @return `sf_task` avec nouveau champ "full_prediction", un data frame avec
#' les colonnes: "siret", "periode", la décomposition en variables latentes,
#' la prédiction de l'apprentissage automatique, les corrections apportées,
#' la prédicition corrigé (tout ça dans l'espace des log-vraisemblance) et la
#' correction corrigée dans l'espace des probabilités
apply_corrections <- function(task, correction_debt, correction_sector) {
  df_join <- task$new_data %>%
    select(siret, code_ape)

  ape_to_secteur <- get_ape_to_secteur()
  df_join <- df_join %>%
    left_join(ape_to_secteur, by = "code_ape")

  df_corrections <- df_join %>%
    left_join(correction_debt, by = "siret") %>%
    left_join(correction_sector, by = "secteur") %>%
    select(correction_debt, correction_sector)

  prediction_log_likelihood <- task$prediction_new %>%
    data.table::as.data.table() %>%
    gtools::logit()

  # TODO: add latent variables here
  task$full_prediction <- data.frame(
    prediction = prediction_log_likelihood,
    correction_debt = df_corrections$correction_debt,
    correction_sector = df_corrections$correction_sector,
    corrected_prediction = prediction + correction_debt + corrected_sector,
    corrected_prediction_prob = gtools::inv.logit(corrected_prediction)
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
