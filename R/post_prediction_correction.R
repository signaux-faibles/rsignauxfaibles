#' Fonction de récupération des données de conjoncture Banque de France
#'
#' Via l'api webstat.
#'
#' La variable webstat_client_ID est indispensable pour s'identifier auprès de
#' l'api webstat pour faire fonctionner w_series_list. Cf la documentation
#' officielle ou la page gollum "Accès extérieurs depuis labtenant dans R".
#'
#' @return `data.frame` Quelques séries de données de conjoncture de la Banque
#' de France, depuis le premier janvier 2014.
#' @export
fetch_conj_data <- function() {
  assertthat::assert_that(exists("webstat_client_ID"),
    msg = paste("L'accès à l'API webstat demande une identification.\n",
      "Veuillez vous référer à la page gollum",
      '"Accès extérieurs depuis labtenant dans R"')
    )
  require(rwebstat)
  require(dplyr)
  series <- rwebstat::w_series_list("CONJ", language = "fr")
  questions_to_keep <- c(
    "CCTSM000", # situation actuelle de l'état du carnet de commandes
    "CREEM100", # évolution des commandes reçues de l`étranger
    # par rapport au mois précédent
    "CRTEM100", # évolution des commandes reçues par rapport au mois précédent
    "LITEM100", # évolution des livraisons par rapport au mois précédent
    "PRTEM100", # évolution de la production par rapport au mois précédent
    "PRTPM100", # prévisions de production à 1 mois
    "PVTSM000", # situation actuelle des stocks de produits finis
    "TUTSM000" # taux moyen d'utilisation des capacités de production
  )
  series <- series %>%
    rename(
      secteur = ENQCNJ_SECTEUR,
      question = ENQCNJ_QUESTION,
      frequence = FREQ,
      redressement = ADJUSTMENT
    ) %>%
    filter(
      frequence == "M", # Frequence mensuelle
      redressement == "S" # Redressement de la saisonnalité
    ) %>%
    filter(question %in% questions_to_keep)

    data_conj_original <- rwebstat::w_data(
      dataset_name = "CONJ",
      series_name = string_denominator(series$SeriesKey),
      language = "fr"
    )
    data_conj <- data_conj_original %>%
      select(periode = "date", all_of(series$SeriesKey)) %>%
      filter(periode >= "2014-01-01")

    # Transformation en format
    # periodes X secteur X (une colonne par question)
    data_conj <- data_conj %>%
      tidyr::pivot_longer(
        cols = -periode,
        names_to = "serie_name",
        values_to = "serie_value"
        ) %>%
    mutate(
      secteur = stringr::str_sub(serie_name, 17, 21),
      question = stringr::str_sub(serie_name, 23, 30)
      ) %>%
    select(-serie_name) %>%
    tidyr::pivot_wider(names_from = question, values_from = serie_value)
  return(data_conj)
}

#' Fonction utilitaire
#'
#' Prend un vecteur de codes de séries de l'API webstat de la Banque de France
#' et conserve les paramètres communs à toutes les séries. Les paramètres
#' différents sont remplacés par "*", dans un format utilisable par
#' `rwebstat::w_data`
#' @param strings `character()`  Vecteur de codes de séries de l'API webstat
string_denominator <- function(strings) {
  splitted_strings <- strsplit(strings, ".", fixed = TRUE)
  n_substrings <- unique(purrr::map_dbl(splitted_strings, length))
  # All series names must have the same number of substrings
  assertthat::assert_that(length(n_substrings) == 1)
  regexp <- purrr::map_chr(seq_len(n_substrings),
    ~ check_if_same_string(purrr::map_chr(splitted_strings, .))
  )
  regexp <- paste0(regexp, collapse = ".")
  return(regexp)
}

#' Fonction utilitaire
#'
#' Si toutes les chaînes sont identiques, renvoie la valeur
#' identique, sinon renvoie "*".
#'
#' @param strings `character` vecteur de chaînes de caractères
check_if_same_string <- function(strings) {
  if (n_distinct(strings) == 1) {
    return(unique(strings))
  } else {
    return("*")
  }
}

