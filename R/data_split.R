
#' Scission des données en échantillon d'entraînement et de test.
#'
#' Scinde les données historiques en échantillon d'entraînement et de test,
#' selon le ratio souhaité. S'assure que deux établissements de la
#' même entreprise ne soient pas à la fois dans deux échantillons différents
#' pour éviter la fuite d'information d'un échantillon vers l'autre.
#'
#' @inheritParams generic_task
#' @param ratio `numeric(1)` \cr Ratio des données utilisées pour
#' l'entraînement. Ignoré si 'resampling_strategy != "holdout"'
#' @param nfolds `numeric(1)` \cr Nombre d'échantillons de validation croisé.
#' Ignoré si 'resampling_strategy != "cv"'.
#' @param resampling_strategy `character(1)` \cr Ou bien "holdout"
#' ou "cv". Toute autre valeur n'occasionne aucun échantillonnage.
#'
#' @describeIn split_data
#'
#' @return `[sf_task]` \cr L'objet \code{task} donné en entrée auquel les
#' champs "train_data", et "test_data" ont été ajoutés (ou écrasés), chacun
#' contenant un data.frame() avec les colonnes de `task[["hist_data"]]` et un
#' sous-ensemble (possiblement vide) de ses lignes.
#'
#' @export
split_data.sf_task <- function( # nolint
                               task,
                               ratio = 2 / 3,
                               nfolds = 5,
                               resampling_strategy = "holdout",
                               ...) {
  allowable_strategies <- mlr3::mlr_resamplings$keys()

  if (is.null(resampling_strategy) ||
    !resampling_strategy %in% allowable_strategies) {
    lgr::lgr$info(paste(
      "Les données ne sont pas échantillonnées car le paramètre",
      "'resampling_strategy' n'est pas valide. Les paramètres valides sont:",
      paste(allowable_strategies, collapse = ", ")
    ))
    task[["train_data"]] <- task[["hist_data"]]
    return(task)
  }

  lgr::lgr$info(paste0(
    "Les donnees historiques sont scindes en ",
    "echantillons d'entrainement et de test"
  ))

  assertthat::assert_that("hist_data" %in% names(task),
    msg = paste0(
      "Il faut d'abord charger des données à",
      "échanillonner avec 'load_hist_data()'"
    )
  )

  assertthat::assert_that(
    ratio > 0 && ratio <= 1,
    msg = "ratio doit être un nombre entre 0 exclu et 1 inclus"
  )
  assertthat::assert_that(
    nfolds >= 2,
    msg = "'nfolds' doit être supérieur à 2"
  )

  mlr3resampling <- mlr3::rsmp(resampling_strategy)
  if (resampling_strategy == "holdout") {
    mlr3resampling$param_set$values[["ratio"]] <- ratio
  } else if (resampling_strategy == "cv") {
    mlr3resampling$param_set$values[["folds"]] <- nfolds
  }

  set.seed(3)
  mlr3resampling$instantiate(task[["mlr3task"]])

  task[["mlr3rsmp"]] <- mlr3resampling

  # Temp: saving train_data, test_data to task
  task[["train_data"]] <- task[["hist_data"]][mlr3resampling$train_set(1), ]
  task[["test_data"]] <- task[["hist_data"]][mlr3resampling$test_set(1), ]

  log_param(task, "resampling_strategy", resampling_strategy)
  log_param(task, "train_test_ratio", ratio)

  return(task)
}
