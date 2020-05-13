
#' Scission des données en échantillon d'entraînement et de test.
#'
#' Scinde les données historiques en échantillon d'entraînement et de test,
#' selon le ratio souhaité. S'assure que deux établissements de la
#' même entreprise ne soient pas à la fois dans deux échantillons différents
#' pour éviter la fuite d'information d'un échantillon vers l'autre.
#'
#' @inheritParams generic_task
#' @param ratio `numeric(1)` \cr Ratio of data
#' used for training.
#' @param resampling_strategy `character(1)` \cr Either "holdout"
#' or "cross_validation"
#'
#' @describeIn split_data
#'
#' @return `[sf_task]` \cr L'objet \code{task} donné en entrée auquel les
#' champs "train_data", et "test_data" ont été ajoutés (ou écrasés), chacun
#' contenant un data.frame() avec les colonnes de `task[["hist_data"]]` et un
#' sous-ensemble (possiblement vide) de ses lignes.
#'
#' @export
split_data.sf_task <- function(
  task,
  ratio = 2 / 3,
  resampling_strategy = "holdout",
  ...
) {

  set_verbose_level(task)

  logger::log_info(paste0("Les donnees historiques sont scindes en ",
      "echantillons d'entrainement et de test"))

  assertthat::assert_that("hist_data" %in% names(task),
    msg = paste0("Il faut d'abord charger des données à",
      "échanillonner avec 'load_hist_data()'")
  )

  assertthat::assert_that(ratio > 0 && ratio <= 1)

  # creating mlr3task
  mlr3_data <- task[["hist_data"]]

  mlr3_data[[task[["target"]]]] <- as.factor(mlr3_data[[task[["target"]]]])

  if (!c("siren") %in% names(mlr3_data)) {
    mlr3_data$siren <- substr(mlr3_data$siret, 1, 9)
  }

  mlr3task <- mlr3::TaskClassif$new(
    id = "signaux-faibles",
    backend = mlr3_data,
    target = task[["target"]]
  )

  mlr3task$col_roles$name <- c("siret")
  mlr3task$col_roles$group <- c("siren")
  mlr3task$col_roles$feature <- setdiff(
    mlr3task$col_roles$feature,
    c("siret", "siren")
  )

  mlr3resampling <- mlr3::rsmp(resampling_strategy)
  if (resampling_strategy == "holdout") {
    mlr3resampling$param_set$values[["ratio"]] <- ratio
  }

  set.seed(3)
  mlr3resampling$instantiate(mlr3task)

  task[["mlr3task"]] <- mlr3task
  task[["mlr3rsmp"]] <- mlr3resampling

  task[["train_data"]] <- task[["hist_data"]][mlr3resampling$train_set(1), ]
  task[["test_data"]] <- task[["hist_data"]][mlr3resampling$test_set(1), ]

  # Temp: saving train_data, test_data to task
  log_param(task, "resampling_strategy", resampling_strategy)
  log_param(task, "train_test_ratio", ratio)

  return(task)
}
