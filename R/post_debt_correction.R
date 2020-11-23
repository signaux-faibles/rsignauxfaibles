#' Calcule une correction pour les entreprises endettées
#'
#' La correction est dans l'espace des log-vraisemblance (donc après avoir
#' appliqué un logit aux prédictions en probabilité).
#'
#' @inheritParams generic_task
#'
#' @return `data.frame` avec colonnes "siret" et "correction_debt"
#' @export
compute_debt_correction <- function(task) {
  # Correction débits
  assertthat::assert_that("new_data" %in% names(task))
  new_data <- task$new_data
  correction <- new_data %>%
    group_by(code_ape_niveau3) %>%
    mutate(ratio_dette = (montant_part_patronale + montant_part_patronale) / effectif) %>%
    arrange(code_ape_niveau3, desc(ratio_dette)) %>%
    group_by(code_ape_niveau3, ratio_dette > 1) %>%
    mutate(
      # position == 1 pour max dette, puis décroit jusqu'à 0 pour pas de dette
      position = case_when(
        ratio_dette > 1 ~ seq(1, 0, length.out = n()),
        TRUE ~ 0 # Tout les autres cas: pas de dette => pas de pénalité
      ),
      correction_debt = dbeta(position, 3, 1) / 3
      # Courbe croissante, de f(0) = 0 à f(1) = 1
    ) %>%
    ungroup() %>%
    select(siret, correction_debt)

  return(correction)
}
