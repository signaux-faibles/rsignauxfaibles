compute_sectorial_correction <- function(
                                         conjuncture_predictions = get_conjuncture_predictions()) {
  conjuncture_predictions <- conjuncture_predictions %>%
    select(prediction, n_month_period, secteur) %>%
    mutate(prediction = gtools::logit(prediction))

  # How much did the crisis affect the predictions ?
  # Computes the average difference between before mid-2018 and last
  # observation.
  correction <- conjuncture_predictions %>%
    filter(
      n_month_period <= as.Date("2018-06-01") |
        n_month_period == last(n_month_period)
    ) %>%
    mutate(last_observation = (n_month_period == last(n_month_period))) %>%
    group_by(secteur, last_observation) %>%
    summarize(mean_prediction = mean(prediction, na.rm = TRUE)) %>%
    tidyr::pivot_wider(
      # One row for each "secteur"
      # New columns:
      # last_observation_TRUE: last observed prediction
      # last_observation_FALSE: average observed prediction before mid-2018
      names_from = last_observation,
      names_prefix = "last_observed_pred_",
      values_from = mean_prediction
    ) %>%
    mutate(correction_prediction = last_observed_pred_TRUE - last_observed_pred_FALSE) %>%
    mutate(correction_prediction = ifelse(is.na(correction_prediction), 0, correction_prediction)) %>%
    select(secteur, correction_prediction)

  return(correction)
}

#' Train and predict a model on conjuncture data
#'
#' @param df_conj données de conjoncture
#' @param df_agg_failure données d'agrégation sectorielle.
#'
#' @return `data.frame` with columns: prediction (in [0, 1], n_month_period,
#'   secteur)
get_conjuncture_predictions <- function(
                                        task,
                                        df_conj = fetch_conj_data(),
                                        df_agg_failure = fetch_aggregated_sectors(task)) {
  assertthat::assert_that(all(c("CCTSM000", "TUTSM000", "PRTEM100") %in% df_conj))
  nb_months <- 3

  conj_centered <- df_conj %>%
    mutate(
      n_month_period = lubridate::floor_date(
        as.Date(periode),
        paste0(nb_months, "m")
      )
    ) %>%
    group_by(n_month_period, secteur) %>%
    summarize(
      CCTSM000 = mean(CCTSM000), # Carnet de commande
      TUTSM000 = mean(TUTSM000), # Taux utilisation capacité production
      PRTEM100 = mean(PRTEM100), # Evolution de la production
      .groups = "drop"
    ) %>%
    tidyr::pivot_longer(
      c(CCTSM000, TUTSM000, PRTEM100),
      names_to = "indicateur",
      values_to = "valeur"
    ) %>%
    group_by(secteur, indicateur) %>%
    mutate(
      valeur = valeur - mean(valeur, na.rm = T)
    ) %>%
    ungroup()

  seq_date <- seq.Date(
    as.Date("2014-01-01"),
    as.Date("2018-04-01"),
    by = paste0(nb_months, " month")
  )
  assertthat::assert_that(all(seq_date %in% conj_centered$n_month_period))

  conj_centered_scaled <- conj_centered %>%
    group_by(indicateur) %>%
    mutate(
      valeur = valeur / sd(valeur[n_month_period %in% seq_date])
    ) %>%
    ungroup() %>%
    group_by(indicateur, secteur) %>%
    mutate(
      valeur_lag1 = lag(valeur),
      valeur_lag2 = lag(valeur, 2)
    ) %>%
    tidyr::pivot_wider(names_from = "indicateur", values_from = c("valeur", "valeur_lag1", "valeur_lag2"))

  df <- df_agg_failure %>%
    aggregate_by_secteur() %>%
    aggregate_by_n_months(nb_months) %>%
    adjust_seasonnality(nb_months)

  df <- df %>%
    left_join(conj_centered_scaled, by = c("n_month_period", "secteur")) %>%
    filter(!is.na(secteur)) %>%
    mutate(
      count = as.integer(count),
      count_new_outcome = as.integer(count_new_outcome)
    )
  priors <- c(
    brms::set_prior("student_t(10, 0, 1)", class = "b", ub = 0)
  )
  latent_model <- brms::brm(
    count_new_outcome | trials(count) ~ (1 | secteur) +
      valeur_lag1_CCTSM000 +
      valeur_lag1_TUTSM000,
    data = df,
    family = "binomial",
    prior = priors
  )

  # Prediction
  counts <- df %>%
    group_by(secteur) %>%
    summarize(count = as.integer(last(count))) %>%
    select(secteur, count)
  conj <- conj_centered_scaled %>%
    left_join(counts, by = "secteur")
  prediction <- predict(model, conj)
  multiplicative_fact <- 18 / nb_months
  res <- dplyr::bind_cols(conj, as.data.frame(prediction)) %>%
    mutate(
      prediction = multiplicative_fact * Estimate / count,
      Q2.5 = multiplicative_fact * Q2.5 / count,
      Q97.5 = multiplicative_fact * Q97.5 / count
    )
  return(res)
}



#' Requete la base de données pour récupérer les secteurs agrégés
#'
#' La collection "secteurs" contient des informations agrégés par code_ape et
#' periode, pour les entreprises avec un effectif connu supérieur à 10.
#'
#' Le mois de janvier 2015 est retiré des données, car ce mois présente un
#' pic dû au fait qu'on prend en compte les données de défaut URSSAF
#' uniquement à partir de décembre 2014 (12 mois d'observations pour la
#' cotisation moyenne), ce qui créé artificiellement des
#' time_til_outcome == 1 en janvier 2015.
#'
#' /!\ Le premier semestre ne dure en conséquence que 5 mois, penser à
#' redresser en cas d'agrégation.
#'
#' @inheritParams generic_task
#' @param adjust `logical` Faut-il redresser la série ? (anomalies en base,
#' saisonnalité)
#' @param ape_to_naf `data.frame` table de passage du code ape au code naf
#' @param ape_to_secteur `data.frame` table de passage du code ape au secteur
#'
#' @return `data.frame` avec les colonnes "code_ape", "periode", "count" qui
#' correspond au nombre d'observations total, "count_outcome" qui compte le
#' nombre de défaillances/défauts à 18 mois, "count_new_outcome", qui compte
#' le nombre de défaillances/défauts à 1 mois, "secteur", "code_naf"
#'
#' @export
fetch_aggregated_sectors <- function(
                                     task,
                                     ape_to_naf = get_ape_to_naf(),
                                     ape_to_secteur = get_ape_to_secteur()) {
  require(mongolite)
  collection <- "secteurs"
  dbconnection <- mongolite::mongo(
    collection = collection,
    db = task$database,
    url = task$mongodb_uri,
    verbose = FALSE
  )
  secteurs_agreges <- dbconnection$find(
    "{}",
    fields = '{"_id.code_ape": 1, "_id.periode": 1, "count": 1, "count_outcome": 1, "count_new_outcome": 1}'
  )
  secteurs_agreges$code_ape <- secteurs_agreges[, "_id"]$code_ape
  secteurs_agreges$periode <- secteurs_agreges[, "_id"]$periode
  secteurs_agreges[, "_id"] <- NULL

  # Ajout des secteurs
  secteurs_agreges <- secteurs_agreges %>%
    left_join(ape_to_secteur)

  # Ajout des naf
  secteurs_agreges <- secteurs_agreges %>%
    left_join(ape_to_naf)

  secteurs_agreges <- secteurs_agreges %>%
    filter(periode > "2015-01-01", periode < "2019-07-01")

  return(secteurs_agreges)
}


#' Recupère la table de correspondance code_ape <-> secteur
#'
#' Les secteurs sont utilisés dans les enquêtes de conjoncture de la Banque de
#' France
#'
#' @param path :: chemin d'accès au fichier excel. Par défaut:
#' "raw_data/RWEBSTATS_table_passage.xlsx". Le fichier est disponible dans
#' "gollum", sur la page "Ressources externes depuis labtenant dans R".
#' @param sheet :: Feuille à récupérer, par défaut, la sixième
#'
#' @return `data.frame` table de correspondance avec les colonnes "code_ape", "secteur",
#' "nom_secteur".
get_ape_to_secteur <- function(path = file.path("raw_data", "RWEBSTATS_table_passage.xlsx"), sheet = 6) {
  require(readxl)
  require(stringr)
  require(dplyr)
  ape_to_secteur <- readxl::read_xlsx(path, sheet) %>%
    select(code_ape = naf700, secteur = emc, nom_secteur = libemc) %>%
    mutate(secteur = stringr::str_replace_all(str_pad(secteur, 5), " ", "0")) %>%
    mutate(code_ape = stringr::str_replace(code_ape, fixed("."), ""))
  return(ape_to_secteur)
}

#' Récupère la table de correspondance de ape à naf
#'
#' Cette table de correspondance doit par défaut se trouver dans le répertoire
#' "raw_data" et porter le nom "naf5to1.csv".
#'
#' Ce fichier est disponible dans le répertoire "gollum", sur la page
#' "Ressources externes depuis labtenant dans R"
#'
#' @param path :: chemin d'accès
#'
#' @return `data.frame` table de correspondance avec colonnes "code_ape" et
#' "code_naf"
#' @export
get_ape_to_naf <- function(path = file.path("raw_data", "naf5to1.csv")) {
  code_ape_to_naf <- readr::read_csv2(path) %>%
    rename("code_ape" = "n5", "code_naf" = "n1")
  return(code_ape_to_naf)
}

#' Fonction de récupération des données de conjoncture Banque de France
#'
#' Via l'api webstat.
#'
#' La variable webstat_client_ID est indispensable pour s'identifier auprès de
#' l'api webstat pour faire fonctionner w_series_list. Cf la documentation
#' officielle ou la page gollum "Ressources externes depuis labtenant dans R".
#'
#' @return `data.frame` Quelques séries de données de conjoncture de la Banque
#' de France, depuis le premier janvier 2014.
#' @export
fetch_conj_data <- function() {
  assertthat::assert_that(exists("webstat_client_ID"),
    msg = paste(
      "L'accès à l'API webstat demande une identification.\n",
      "Veuillez vous référer à la page gollum",
      '"Ressources externes depuis labtenant dans R"'
    )
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
  regexp <- purrr::map_chr(
    seq_len(n_substrings),
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

#' Agrège les données de défaillances par secteur
#'
#' @param df `data.frame()` Données agrégées.
#'
#' @return `data.frame()`
#' @export
aggregate_by_secteur <- function(df) {
  secteurs_agreges <- df %>%
    group_by(periode, secteur) %>%
    summarize(
      count = sum(count),
      count_new_outcome = sum(count_new_outcome),
      prop_new_outcome = count_new_outcome / count,
      nom_secteur = first(nom_secteur),
      .groups = "drop"
    )
  return(secteurs_agreges)
}

#' Agrège les donnée de défaillances par plages de plusieurs mois
#'
#' @param df `data.frame` Données à agréger, avec colonnes "count",
#' "count_new_outcome", "secteur", "nom_secteur"
#' @param n_month `1, 2, 3, 6` durée de la plage en mois
#'
#' @return `data.frame` Données agrégées, "count" est moyenné,
#' "count_new_outcome" est sommé, "prop_new_outcome" est recalculé.
#' @export
aggregate_by_n_months <- function(df, n_month = 3) {
  assertthat::assert_that(n_month %in% c(1, 2, 3, 6))

  df <- df %>%
    mutate(n_month_period = lubridate::floor_date(
      periode,
      paste0(n_month, "m")
    )) %>%
    add_missing_first_month()

  aggregated_df <- df %>%
    group_by(secteur, n_month_period) %>%
    summarise(
      count = mean(count),
      count_new_outcome = sum(count_new_outcome),
      prop_new_outcome = count_new_outcome / count,
      nom_secteur = first(nom_secteur),
      .groups = "drop"
    )

  return(aggregated_df)
}

#' Fonction utilitaire: corriger l'absence du mois de janvier 2015
#'
#' Le mois de janvier est imputé par la moyenne des autresmois de la période.
#'
#' @param df `data.frame()`
#'
#' @return `data.frame()`
add_missing_first_month <- function(df) {
  df <- df %>%
    group_by(secteur, nom_secteur, n_month_period) %>%
    tidyr::nest() %>%
    mutate(
      data = case_when(
        n_month_period == "2015-01-01" ~ purrr::map(
          data,
          add_missing_first_month_aux
        ),
        TRUE ~ data
      )
    ) %>%
    tidyr::unnest("data")
  return(df)
}


#' Fonction utilitaire: Ajout d'un mois manquant dans le premier trimestre
#' avec des valeurs moyennes
#'
#' Cette fonction est une fonction utilitaire utilisée dans
#' aggregate_by_n_months, et prend un data.frame obtenu après un
#' `group_by(secteur, nom_secteur) %>% nest()` sur un data.frame de
#' défaillances agrégés par secteur. Notamment utilisé dans
#' aggregate_by_secteur
#'
#' @param data_secteur_n_months  `data.frame()` Données sectorielles propres à
#' un secteur. Colonnes attendues: "periode", "count", "count_new_outcome",
#' "prop_new_outcome".
#'
#' @return le même data.frame avec une ligne pour la période 2015-01-01
#' moyennée sur les données de la première plage de "n_months" mois.
#'
#'
add_missing_first_month_aux <- function(data_secteur_n_months) {
  require(dplyr)
  assertthat::assert_that(
    !as.Date("2015-01-01") %in% data_secteur_n_months$periode,
    msg = "Tentative d'imputer une donnée existante en 2015-01-01"
  )
  data_secteur_n_months <- data_secteur_n_months %>%
    dplyr::add_row(
      periode = as.Date("2015-01-01"),
      count = mean(data_secteur_n_months$count),
      count_new_outcome = mean(data_secteur_n_months$count_new_outcome),
      prop_new_outcome = count_new_outcome / count
    ) %>%
    arrange(periode)

  return(data_secteur_n_months)
}

#' Correction de la saisonnalité pour le secteur 000C1
#'
#' Une forte saisonnalité a été observé pour le secteur 000C1.
#' Cette fonction corrige cette saisonnalité.
#'
#' @param df `data.frame()` Données agrégées par périodes.
#' @param n_month `int()` Durée de la plage d'agrégation.
#'
#' @return
adjust_seasonnality <- function(df, n_month) {
  df <- df %>%
    group_by(secteur) %>%
    arrange(n_month_period) %>%
    mutate(n_month_period_int = 1:n()) %>%
    ungroup()

  df_000C1 <- df %>%
    filter(secteur == "000C1")
  trend <- lm(data = df_000C1, prop_new_outcome ~ n_month_period_int)

  n_periods_per_year <- 12 / n_month
  matrix_size <- ceiling(
    length(residuals(trend)) / n_periods_per_year
  ) * n_periods_per_year

  residuals_matrix <- matrix(
    residuals(trend)[1:matrix_size],
    ncol = 12 / n_month,
    byrow = TRUE
  )
  seasonality <- residuals_matrix %>% colMeans(na.rm = TRUE)

  df_redr <- df %>%
    mutate(prop_new_outcome = case_when(
      secteur == "000C1" ~ prop_new_outcome - seasonality,
      TRUE ~ prop_new_outcome
    ))
  return(df_redr)
}
