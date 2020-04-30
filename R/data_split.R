#' Scission des données en échantillon d'entraînement, de validation et de
#' test.
#'
#' Scinde les données historiques en échantillon d'entraînement, de validation
#' et de test, selon les proportions souhaitées. S'assure que deux
#' établissements de la même entreprise ne soient pas à la fois dans deux
#' échantillons différents pour éviter la fuite d'information d'un échantillon
#' vers l'autre.
#'
#'  La fraction de l'échantillon de test est calculée par
#'  1 - frac_train - frac_val. (frac_train + frac_val) doit donc être inférieur
#'  à 1. Le seul cas où cette condition n'est pas testée est lorsque frac_train
#'  = 1.
#'
#' @inheritParams generic_task
#' @inheritParams split_snapshot_rdm_month
#' @describeIn split_data
#'
#' @return `[sf_task]` \cr
#'   L'objet \code{task} donné en entrée auquel les champs "train_data",
#'   "validation_data" et "test_data" ont été
#'   ajoutés (ou écrasés), chacun contenant un data.frame() avec les colonnes
#'   de `task[["hist_data"]]` et un sous-ensemble (possiblement vide) de ces
#'   lignes.
#'
#' @export
split_data.sf_task <- function(
                               task,
                               fracs = c(0.6, 0.2, 0.2),
                               names = c("train", "validation", "test"),
                               ...) {
  set_verbose_level(task)

  logger::log_info("Les donnees historiques sont scindes en echantillons
    d'entrainement, de test et de validation")

  assertthat::assert_that("hist_data" %in% names(task),
    msg = "Please load historical data before holding out test data"
  )

  if ((length(fracs) == 1 && fracs == 1) || identical(fracs, c(1, 0, 0))) {
    task[["train_data"]] <- task[["hist_data"]]
  } else {
    res <- split_snapshot_rdm_month(
      data_sample = task[["hist_data"]],
      fracs = fracs,
      names = names
    )

    for (name in names) {
      task[[paste0(name, "_data")]] <- task[["hist_data"]] %>%
        semi_join(res[[name]], by = c("siret", "periode"))
    }
  }


  if (!is.null(task[["tracker"]]) && requireNamespace("mlflow")) {
    names(fracs) <- names
    mlflow::mlflow_log_param("resampling_strategy", "holdout")
    mlflow::mlflow_log_param("train_val_test_shares", fracs)
  }

  return(task)
}

#' Prepare cross-validation
#'
#' Splits the data into `n_folds` for cross validation
#'
#' @inheritParams split_data.sf_task
#' @param n_folds `integer(1)` \cr Number of folds
#' @param frac_test `numeric(1)` \cr Fraction of data which should be kept
#'   aside
#'
#' @return `c(cv_task, sf_task)` \cr
#'  A cross validated task with field "cross_validation", which is a list of
#'  tasks, one for each fold.
#' @export
split_n_folds <- function(
                          task,
                          n_folds = 4,
                          frac_test = 0.2) {
  requireNamespace("purrr")

  assertthat::assert_that(frac_test >= 0 && frac_test < 1)
  frac_cv <- (1 - frac_test) / n_folds
  cv_names <- paste0("cv_", 1:n_folds)
  task <- split_data(
    task,
    fracs = c(rep(frac_cv, n_folds), frac_test),
    names = c(cv_names, "test")
  )
  cv_names <- paste0(cv_names, "_data")
  cv_chunks <- task[cv_names]
  task[cv_names] <- NULL

  create_cv_task <- function(cv_number, cv_chunks) {
    cv_task <- sf_task(
      verbose = TRUE,
      database = task[["database"]],
      collection = task[["collection"]],
      mongodb_uri = task[["mongodb_uri"]],
      tracker = task[["tracker"]]
    )
    cv_task[["validation_data"]] <- cv_chunks[[cv_number]]
    cv_task[["train_data"]] <- dplyr::bind_rows(cv_chunks[-cv_number])

    if (!is.null(task[["tracker"]]) && requireNamespace("mlflow")) {
      mlflow::mlflow_log_param(
        "resampling_strategy",
        paste0(n_folds, "-folds cross validation")
      )
    }

    return(cv_task)
  }

  task[["cross_validation"]] <- purrr::map(
    1:n_folds,
    create_cv_task,
    cv_chunks = cv_chunks
  )

  if (!"cv_task" %in% class(task)) {
    class(task) <- c("cv_task", class(task))
  }

  return(task)
}

#' Découper l'échantillon en échantillon d'entraînement, de test et de validation
#'
#' Scinde les données en échantillon d'entraînement, de validation et de
#' test, selon les proportions souhaitées. S'assure que deux établissements de la même entreprise ne soient pas
#' a la fois dans deux échantillons différents pour éviter la fuite
#' d'information d'un échantillon vers l'autre.
#'
#'  La fraction de l'échantillon de test est calculée par
#'  1 - frac_train - frac_val. (frac_train + frac_val) doit donc être inférieur
#'  a 1.
#'
#' @param data_sample `dataframe()`\cr
#' Données à scinder avec des champs "periode" et "siret".
#' @param fracs `numeric()` \cr Fraction des différents sous-échantillons.
#' Toutes les fractions doivent être dans ]0,1] et leur somme doit être égale
#' à 1.
#' @param names `character()` \cr Vecteur de noms des sous-échantillons, qui doit être de même
#' longueur que les fractions, ou de longueur 1. Si de longueur 1, alors un
#' préfixe "_X", avec X la numérotation des échantillons, est automatiquement ajouté.
#' @param seed `integer(1)` \cr Seed pour assurer la reproductibilité des
#'   opérations aléatoires.
#'
#' @return `list(data.frames(3))` \cr
#' Liste de trois data.frames: \code{train}, \code{validation} and \code{test}
#' avec les couples (siret x periode) des trois échantillons calculés.
#' @export
split_snapshot_rdm_month <- function(
                                     data_sample,
                                     fracs,
                                     names,
                                     seed = 1234) {
  assertthat::assert_that(
    sum(fracs) == 1,
    msg = "Sum of fractions should be equal to 1"
  )
  assertthat::assert_that(
    all(fracs > 0),
    msg = "All fractions should be strictly positivie"
  )
  assertthat::assert_that(
    length(names) == length(fracs) ||
      length(names) == 1,
    msg = "Les noms specifies doivent etre de meme longueur que les fractions,
    ou de longueur 1"
  )


  data_sample <- data_sample %>%
    select(siret, periode) %>%
    arrange(siret, periode) %>%
    mutate(siren = substr(siret, 1, 9))

  sirens <- data_sample %>%
    select(siren) %>%
    distinct()

  set.seed(seed)

  random_vec <- stats::runif(n = nrow(sirens))
  random_cats <- .bincode(random_vec, breaks = c(0, cumsum(fracs)), TRUE, TRUE)
  sirens <- sirens %>%
    mutate(ss = random_cats) %>%
    mutate(ss = factor(ss, levels = 1:length(fracs)))

  data_sample <- data_sample %>%
    left_join(y = sirens, by = "siren") %>%
    select(siret, periode, ss)

  if (length(names) == 1 && length(fracs) > 1) {
    names <- paste0(names, "_", 1:length(fracs))
  }
  result <- stats::setNames(
    base::split(data_sample %>% select(-ss), data_sample %>% select(ss)),
    names
  )

  return(result)
}
