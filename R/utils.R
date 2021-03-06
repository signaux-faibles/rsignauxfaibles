#' Gives alert levels from prediction and F-scores
#'
#' Lower thresholds are strict (a prediction falling on the threshold is
#' binned to the lower alert level)
#'
#' @param prediction Vector, list of predictions between 0 and 1.
#' @param f1 `Double(1)`. F1 threshold
#' @param f2 `Double(1)`. F2 threshold
#'
#' @return A factor vector with alert levels.
#' @export
#'
alert_levels <- function(prediction, f1, f2) {
  assertthat::assert_that(f2 <= f1,
    msg = "F2 threshold cannot be greater than F1 threshold Could you have
    inverted the F scores ?"
  )
  alert <- .bincode(
    x = prediction,
    breaks = c(-1e-4, f2, f1, 1 + 1e-4),
  ) %>%
    factor(
      levels = 1:3,
      labels = c("Pas d'alerte", "Alerte seuil F2", "Alerte seuil F1")
    )
}


#' Nomme un fichier
#'
#' Nomme un fichier selon une convension précise afin de sauvegarder plusieurs
#' fichiers sans écraser les précédents.
#'
#' @param absolute_path `character(1)`\cr Chemin d'accès depuis
#' la racine
#'   du projet.
#' @param file_detail `character(1)` \cr Description en un mot de la nature du
#'   fichier sauvegardé
#' @param file_extension `character(1)` \cr Extension souhaitée pour le nom de
#' fichier
#' @param full_path `logical(1)` \cr Faut-il renvoyer le chemin complet
#' (`TRUE`) ou uniquement le nom de fichier (`FALSE`)
#'
#' @return `character()` \cr
#'   Un nom de fichier qui n'écrase pas les fichiers déjà contenues dans le
#'   dossier spécifié par "absolute_path", de la forme
#'   "AAAA-MM-JJ_vX_{file_detail}.{file_extension}", où X est un chiffre
#'
#' @export
name_file <- function(
                      absolute_path,
                      file_detail,
                      file_extension = "",
                      full_path = FALSE) {
  full_dir_path <- absolute_path

  assertthat::assert_that(dir.exists(full_dir_path),
    msg = "Directory not found. Check relative path"
  )

  file_list <- list.files(full_dir_path)


  n_different <- grepl(
    paste0(
      "^", Sys.Date(), "_v[0-9]*_",
      file_detail, "\\.", file_extension, "$"
    ),
    file_list
  ) %>%
    sum()


  file_name <- paste0(
    Sys.Date(),
    "_v",
    n_different + 1,
    "_",
    file_detail,
    ".",
    file_extension
  )

  if (full_path) {
    full_file_path <- file.path(full_dir_path, file_name)

    return(full_file_path)
  } else {
    return(file_name)
  }
}

mock_query_database <- function(data) {
  return(function(...) {
    return(data)
  })
}
