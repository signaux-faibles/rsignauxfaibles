#' @import dplyr
#' @importFrom lubridate %m+% %m-%
#' @importFrom rlang :=
#' @importFrom stats predict
NULL

#' Documentation des informations de connection à mongodb
#'
#' @param mongodb_uri `character(1)` \cr url to the database in mongodb uri
#' format.
#' @param database `character(1)` \cr Nom de la base de données vers laquelle
#' param exporter. Par défaut, celle stockée dans \code{task}.
#' @param collection `character(1)` \cr Nom de la collection vers laquelle
#'   exporter. Par défaut, celle stockée dans \code{task}.
#' @name mongodb_connection
NULL

#' Chargement de données historiques
#'
#' Charge les données historiques de signaux faibles et les stocke dans un
#' champ "hist_data" de l'objet \code{task}
#'
#' @param batch `character(1)` \cr Batch auquel doit être importées les
#'   données. Les modifications opérées par les batchs ultérieurs sont
#'   ignorées.
#' @inheritParams mongodb_connection
#' @param subsample `integer(1)` \cr Nombre d'objets (c'est-à-dire de couples
#'   siret x periode) à échantillonner.
#' @param fields `character()` \cr Noms des champs à requêter dans la base de
#'   données. Doit contenir "siret" et "periode". Si égal à \code{NULL}, alors
#'   charge tous les champs disponibles.
#' @param date_inf `Date` \cr Date inférieure du chargement des données.
#' @param date_sup `Date` \cr Date supérieure (exclue) du chargement des
#'   données.
#' @param min_effectif `integer(1)` \cr Limite basse du filtrage de l'effectif
#'   (la limite est incluse)
#' @param sirets `character()` \cr Liste de sirets à exporter. Si égale à
#'   \code{NULL}, charge tous les sirens disponibles.
#' @param code_ape `character()` \cr Liste de codes APE à exporter. Si égale à
#'   \code{NULL}, charge tous les codes disponibles.
#'
#' @return `[sf_task]` \cr
#'   L'objet \code{task} donné en entrée auquel le champs "hist_data" a été
#'   ajouté (ou écrasé), contenant un  data.frame() avec les colonnes incluses
#'   dans le paramètre d'entrée \code{fields}, et pour chaque ligne un couple
#'   unique siret x periode.
#'
#' @describeIn load_hist_data
#'
#' @export
load_hist_data.sf_task <- function(
  task,
  batch,
  database = task[["database"]],
  collection = task[["collection"]],
  mongodb_uri = task[["mongodb_uri"]],
  subsample = NULL,
  fields = get_fields(training = FALSE),
  date_inf = as.Date("2015-01-01"),
  date_sup = as.Date("2017-01-01"),
  min_effectif = 10L,
  sirets = NULL,
  code_ape = NULL,
  debug = FALSE,
  ...) {
  set_verbose_level(task)
  logger::log_info("Chargement des données historiques.")

  hist_data <- import_data(
    database,
    collection,
    mongodb_uri = mongodb_uri,
    batch,
    min_effectif = min_effectif,
    date_inf = date_inf,
    date_sup = date_sup,
    fields = fields,
    sirets = sirets,
    code_ape = code_ape,
    subsample = subsample,
    verbose = attr(task, "verbose"),
    debug = debug
  )

  if (nrow(hist_data) > 1) {
    logger::log_info("Les donnees ont ete chargees avec succes.")
  } else {
    logger::log_warn("Aucune donnee n'a ete chargee. Veuillez verifier la
      requete.")
  }
  check_overwrites(task, "hist_data")
  task[["hist_data"]] <- hist_data

  return(task)
}

#' Chargement de nouvelles données
#'
#' @param task `[sf_task]` \cr Objet s3 de type sf_task
#' @param periods `[Date()]` \cr Périodes d'intérêt, auquels charger les
#'   données. Des périodes supplémentairs peuvent être chargées selon la
#'   valeur de rollback_months.
#' @inheritParams import_data
#' @param rollback_months `integer(1)`\cr Nombre de mois précédant le premier
#'   mois de `periods` à charger. Permet d'effectuer des calculs de différences
#'   ou de moyennes glissantes pour les périodes d'intérêt.
#'
#' @describeIn load_new_data
#'
#' @return `[sf_task]` \cr
#'   L'objet \code{task} donné en entrée auquel le champs "new_data" a été
#'   ajouté (ou écrasé), contenant un  data.frame() avec les colonnes incluses
#'   dans le paramètre d'entrée \code{fields}, et pour chaque ligne un couple
#'   unique siret x periode.
#' @export
load_new_data.sf_task <- function(
  task,
  periods,
  batch,
  database = task[["database"]],
  collection = task[["collection"]],
  mongodb_uri = task[["mongodb_uri"]],
  fields = get_fields(training = FALSE),
  min_effectif = 10L,
  rollback_months = 1L,
  debug = FALSE,
  ...) {
  set_verbose_level(task)

  logger::log_info("Loading data from last batch")
  task[["new_data"]] <- import_data(
    database = database,
    collection = collection,
    mongodb_uri = mongodb_uri,
    batch = batch,
    date_inf = min(periods) %m-% months(rollback_months),
    date_sup = max(periods) %m+% months(1),
    min_effectif = min_effectif,
    fields = fields,
    verbose = attr(task, "verbose"),
    debug = debug
  )

  if ("periode" %in% fields &&
    max(task[["new_data"]]$periode) != max(periods)) {
    logger::log_warn("Data is missing at actual period !")
  }
  return(task)
}

#' Connexion à la base de donnée
#'
#' `import_data` permet de requêter des données mongoDB pour en
#' faire un dataframe ou un Spark dataframe. \cr
#' `factor_query` permet de fabriquer la requête d'aggrégation
#' correspondante. \cr
#'
#' @inheritParams mongodb_connection
#' @param batch `character(1)` \cr Batch auquel doit être importées les
#'   données. Les modifications opérées par les batchs ultérieurs sont
#'   ignorées.
#' @param sirets `character()` \cr Liste de sirens à exporter. Si égale à
#'   \code{NULL}, charge tous les sirens disponibles.
#' @param date_inf `Date(1)` \cr Limite inférieure de la période de temps
#' requêtée
#' @param date_sup `Date(1)` \cr Limite supérieure de la période de temps requêtée
#' @param min_effectif `integer(1)` \cr Limite basse du filtrage de l'effectif
#'   (la limite est incluse)
#' @param fields `character()` \cr Noms des champs à requêter dans la base de
#'   données. Doit contenir "siret" et "periode". Si égal à `NULL`, alors
#'   charge tous les champs disponibles.
#' @param code_ape `character()` \cr Liste de code NAF ou APE (niveau 2 à 5) à
#'   exporter. Si égale à \code{NULL}, charge tous les codes disponibles. Il est
#'   permis de mélanger des codes de différents niveaux.
#' @param subsample `integer(1)` \cr Nombre d'objets (c'est-à-dire de couples
#'   siret x periode) à échantillonner.
#' @param verbose `logical(1)` \cr Faut-il afficher dans le terminal des
#'   informations sur l'évolution du chargement des données?
#' @param replace_missing `list()` \cr Liste nommée, dont les noms sont les
#'   noms de variables et les valeurs sont les valeurs de remplacement des NA.
#'   Si égal à NULL, alors des remplacements par défauts
#'
#' @section Remplacement des valeurs manquantes par défaut:
#' replace_missing <- list(
#'   montant_part_patronale         = 0,
#'   montant_part_ouvriere          = 0,
#'   montant_echeancier             = 0,
#'   ratio_dette                    = 0,
#'   ratio_dette_moy12m             = 0,
#'   montant_part_patronale_past_1  = 0,
#'   montant_part_ouvriere_past_1   = 0,
#'   montant_part_patronale_past_2  = 0,
#'   montant_part_ouvriere_past_2   = 0,
#'   montant_part_patronale_past_3  = 0,
#'   montant_part_ouvriere_past_3   = 0,
#'   montant_part_patronale_past_6  = 0,
#'   montant_part_ouvriere_past_6   = 0,
#'   montant_part_patronale_past_12 = 0,
#'   montant_part_ouvriere_past_12  = 0,
#'   apart_heures_consommees        = 0,
#'   apart_heures_autorisees        = 0,
#'   apart_entreprise               = 0,
#'   tag_default                    = FALSE,
#'   tag_failure                    = FALSE,
#'   tag_outcome                    = FALSE
#'   )
#'
#' @return `data.frame()`
#'
#' @export
import_data <- function(
  database,
  collection,
  mongodb_uri,
  batch,
  min_effectif,
  date_inf = NULL,
  date_sup = NULL,
  fields = NULL,
  sirets = NULL,
  code_ape = NULL,
  subsample = NULL,
  verbose = FALSE,
  replace_missing = NULL,
  debug = FALSE) {
  requireNamespace("logger")
  if (verbose) {
    logger::log_threshold(logger::TRACE)
  } else {
    logger::log_threshold(logger::WARN)
  }

  assertthat::assert_that(date_sup > date_inf)
  if (is.null(sirets) && is.null(code_ape)) {
    query <- build_standard_query(
      batch = batch,
      date_inf = date_inf,
      date_sup = date_sup,
      min_effectif = min_effectif,
      subsample = subsample,
      fields = fields
    )
  } else if (!is.null(sirets)) {
    assertthat::assert_that(
      is.null(subsample),
      msg = "L'option subsample n'est pas valide si le paramètre 'sirets' est renseigné."
    )
    assertthat::assert_that(
      is.null(sirets) || is.null(code_ape),
      msg = "Les valeurs 'sirets' et 'code_ape' ne peuvent pas être requêtées en même temps"
    )
    query <- build_siret_query(
      batch = batch,
      date_inf = date_inf,
      date_sup = date_sup,
      sirets = sirets,
      fields = fields
    )
  } else {
    query <- build_sector_query(
      batch = batch,
      date_inf = date_inf,
      date_sup = date_sup,
      code_ape = code_ape,
      fields = fields
    )
  }

  if (debug) {
    cat(query)
  }

  assertthat::assert_that(
    is.null(fields) || all(c("periode", "siret") %in% fields)
  )

  logger::log_info("Connexion a la collection mongodb {collection} ...")
  df <- query_database(query, database, collection, mongodb_uri, verbose)
  logger::log_info("Import fini.")


  df <- replace_missing_data(
    df = df,
    fields = fields,
    replace_missing = replace_missing
  )

  n_eta <- dplyr::n_distinct(df$siret)
  n_ent <- dplyr::n_distinct(df$siret %>% stringr::str_sub(1, 9))
  logger::log_info(
    "Import de {n_eta} etablissements issus de {n_ent} entreprises."
  )

  df <- update_types(
    df = df
  )

  check_valid_data(df)
  logger::log_info(" Fini.")

  return(df)
}

query_database <- function(
  query,
  database,
  collection,
  mongodb_uri,
  verbose) {
  dbconnection <- mongolite::mongo(
    collection = collection,
    db = database,
    url = mongodb_uri,
    verbose = verbose
  )
  logger::log_info("Connexion effectuée avec succès. Début de l'import.")
  df <- dbconnection$aggregate(query)
  return(df)
}

replace_missing_data <- function(
  df,
  fields,
  replace_missing) {
  df <- add_missing_fields(
    df = df,
    fields = fields
  )

  # Default values
  if (is.null(replace_missing)) {
    replace_missing <- list(
      montant_part_patronale = 0,
      montant_part_ouvriere = 0,
      montant_echeancier = 0,
      ratio_dette = 0,
      ratio_dette_moy12m = 0,
      montant_part_patronale_past_1 = 0,
      montant_part_ouvriere_past_1 = 0,
      montant_part_patronale_past_2 = 0,
      montant_part_ouvriere_past_2 = 0,
      montant_part_patronale_past_3 = 0,
      montant_part_ouvriere_past_3 = 0,
      montant_part_patronale_past_6 = 0,
      montant_part_ouvriere_past_6 = 0,
      montant_part_patronale_past_12 = 0,
      montant_part_ouvriere_past_12 = 0,
      apart_heures_consommees = 0,
      apart_heures_autorisees = 0,
      apart_entreprise = 0,
      tag_default = FALSE,
      tag_failure = FALSE,
      tag_outcome = FALSE
    )
  }

  if (any(names(replace_missing) %in% colnames(df))) {
    logger::log_info("Filling missing values with default values.")
  }

  df <- df %>%
    tidyr::replace_na(
      replace = replace_missing,
    )
  return(df)
}


add_missing_fields <- function(
  df,
  fields) {
  missing_fields <- fields[
    !fields %in% names(df)
    ]

  if (length(missing_fields) >= 1) {
    logger::log_info("Champ(s) manquant(s): {missing_fields}")
    logger::log_info("Remplacements par NA.")

    for (missing_field in missing_fields) {
      df <- df %>% dplyr::mutate(!!missing_field := NA)
    }
  }
  return(df)
}


update_types <- function(
  df
  ) {

  # Dates de type Date
  df <- df %>%
    mutate_if(lubridate::is.POSIXct, ~ as.character(as.Date(.)))

  # Régions de type facteurs
  if ("region" %in% names(df)) {
    df <- df %>%
      mutate(region = factor(region))
  }
  return(df)
}

check_valid_data <- function(
  df) {
  assertthat::assert_that(
    all(c("periode", "siret") %in% names(df)),
    msg = "Les données importées ne contiennent pas la clé (siret x période)"
  )
  assertthat::assert_that(
    anyDuplicated(df %>% select(siret, periode)) == 0,
    msg = "La base importee contient des lignes identiques."
  )
}

#' Get sirets of companies detected by SF
#'
#' Under construction: TODO
#' - Possibility to filter by batch, algo, periods
#' - custom threshold (F1, F2, other)
#'
#' @inheritParams mongodb_connection
#' @return vector of unique sirets
#' @export
get_sirets_of_detected <- function(
  database = "test_signauxfaibles",
  collection = "Scores",
  mongodb_uri) {
  dbconnection <- mongolite::mongo(
    collection = collection,
    db = database,
    url = mongodb_uri,
    verbose = FALSE
  )

  res <- dbconnection$find(
    '{ "$or": [ { "alert": "Alerte seuil F1" },
    { "alert": "Alerte seuil F2" } ] }'
  )

  return(unique(res$siret))
}

###################################


query_or_null <- function(value_to_test, query) {
  if (is.null(value_to_test)) {
    return(NULL)
  } else {
    return(query)
  }
}

date_query <- function(date, gte_or_lt) {
  assertthat::assert_that(gte_or_lt %in% c("gte", "lt"))
  query <- query_or_null(
    date,
    list(
      "_id.periode" = list()
    )
  )
  if (!is.null(query)) {
    query[["_id.periode"]][[paste0("$", gte_or_lt)]] <-
      list("$date" = paste0(date, "T00:00:00Z"))
  }
  return(query)
}


build_standard_query <- function(
  batch,
  date_inf,
  date_sup,
  min_effectif,
  subsample,
  fields
  ) {

  match_stage <- build_standard_match_stage(
    batch,
    date_inf,
    date_sup,
    min_effectif
  )
  sort <- build_sort_stage()
  limit <- build_limit_stage(subsample)
  replace_root <- build_replace_root_stage()
  projection <- build_projection_stage(fields)

  query <- assemble_stages_to_query(
    match_stage,
    sort,
    limit,
    replace_root,
    projection
  )

  return(query)
}

build_siret_query <- function(
  batch,
  date_inf,
  date_sup,
  sirets,
  fields
  ) {

  match_stage <- build_siret_match_stage(
    batch,
    date_inf,
    date_sup,
    sirets
  )
  replace_root <- build_replace_root_stage()
  projection <- build_projection_stage(fields)

  query <- assemble_stages_to_query(
    match_stage,
    replace_root,
    projection
  )

  return(query)
}

build_sector_query <- function(
  batch,
  date_inf,
  date_sup,
  code_ape,
  min_effectif,
  fields) {

  match_ape <- build_sector_match_stage(batch, date_inf, date_sup, code_ape)
  replace_root <- build_replace_root_stage()
  projection <- build_projection_stage(fields)
  query <- assemble_stages_to_query(
    match_ape,
    replace_root,
    projection
  )
  return(query)
}

assemble_stages_to_query <- function(...) {
  query <- list(...)  %>%
    .[!purrr::map_lgl(., is.null)] %>%
    jsonlite::toJSON(auto_unbox = TRUE)
  return(query)
}

build_sort_stage <- function() {
  sort_stage <- list("$sort" = list(value.random_order = -1))
  return(sort_stage)
}

build_limit_stage <- function(subsample) {
  limit_stage <- list("$limit" = subsample)
  return(limit_stage)
}

build_replace_root_stage <- function() {
  list("$replaceRoot" = list("newRoot" =  "$value"))
}

build_projection_stage <- function(fields) {
  projection_stage <- list()
  projection_stage[["$project"]] <- setNames(
    rep(list(1), length(fields)),
    fields
  )
  projection_stage <- query_or_null(
    fields,
    projection_stage
  )
  return(projection_stage)
}

build_standard_match_stage <- function(
  batch,
  date_inf,
  date_sup,
  min_effectif
  ) {
  ## Construction de la requete ##
  match_batch <- list("_id.batch"  = batch)
  match_date_inf <- date_query(date_inf, "gte")
  match_date_sup <- date_query(date_sup, "lt")
  if (is.null(min_effectif)) {
    min_effectif <- 1
  }
  match_eff <- list(value.effectif = list("$gte" = min_effectif))

  match_stage <- list(
    "$match" = list(
      "$and" = c(
        list(match_batch),
        list(match_date_inf),
        list(match_date_sup),
        list(match_eff)
      )
    )
  )
  return(match_stage)
}

build_siret_match_stage <- function(
  batch,
  date_inf,
  date_sup,
  sirets
  ) {


  library(lubridate)
  n_periods <- interval(date_inf, date_sup) %/% months(1)
  if (n_periods == 0) {
    discrete_periods <- date_inf
  } else {
    discrete_periods <- date_inf %m+%
      ((seq_len(n_periods) - 1) * months(1))
  }


  make_id_objects <- function(siret) {
    id_objects <- purrr::map(
      discrete_periods,
      ~ list(
        batch = batch,
        siret = siret,
        periode = list("$date" =  paste0(., "T00:00:00Z"))
      )
    )
    return(id_objects)
  }

  all_id_objects <- purrr::map(sirets, make_id_objects) %>%
    purrr::reduce(c)

  match_query <- list(
    "$match" = list(
      "_id" = list(
        "$in" = c(
          I(all_id_objects)
        )
      )
    )
  )
  return(match_query)
}


build_sector_match_stage <- function(
  batch,
  date_inf,
  date_sup,
  code_ape
  ) {

  match_batch <- list("_id.batch"  = batch)
  match_date_inf <- date_query(date_inf, "gte")
  match_date_sup <- date_query(date_sup, "lt")
  match_ape <- list("value.code_ape" = list("$in" = I(code_ape)))

  match_stage <- list(
    "$match" = list(
      "$and" = c(
        list(match_batch),
        list(match_date_inf),
        list(match_date_sup),
        list(match_ape)
      )
    )
  )
  return(match_stage)
}


#' Get a list of field names
#'
#' Fonction utilitaire pour récupérer rapidement une liste de noms de
#' variables (champs mongodb).  Utilitary function to quickly get a field name
#' list. Put training = TRUE to get only fields used for training. Put an
#' input to 0 to remove the related fields, to 1 to keep basic fields, and to
#' 2 to include all fields.
#'
#' @param training `logical()` \cr Si `TRUE`, uniquement des champs utilisés
#' pour l'entraînement sont retournés.
#' @param siren `0 | 1 | 2` \cr Niveau de
#' détail des données Sirene. cf Détails.
#' @param urssaf `0 | 1 | 2` \cr Niveau de détail des données urssaf. cf
#' Détails.
#' @param delai `0 | 1 | 2` \cr Niveau de détail des données delai. cf
#' Détails.
#' @param effectif `0 | 1 | 2` \cr Niveau de détail des données effectif. cf
#' Détails.
#' @param diane `0 | 1 | 2` \cr Niveau de détail des données diane. cf
#' Détails.
#' @param bdf `0 | 1 | 2` \cr Niveau de détail des données bdf. cf Détails.
#' @param apart `0 | 1 | 2` \cr Niveau de détail des données apart. cf
#' Détails.
#' @param procol `0 | 1 | 2` \cr Niveau de détail des données procol. cf
#' Détails.
#'
#' @param interim `0 | 1 | 2` \cr Niveau de détail des données interim. cf
#' Détails.
#' @param target_encode `0 | 1 | 2` \cr Target encoding. Uniquement
#' disponibles si `training`= `TRUE`.
#' @param info `0 | 1 | 2`\cr Année des
#' exercices Diane & Bdf
#'
#' @section Details: Chaque type de données a 3 niveaux. Le niveau 0
#' correspond à aucune données. Le niveau 1 aux données élémentaires, et le
#' niveau 2 toutes les données retravaillées.
#'
#' @return `character()` \cr Vecteur de noms de variables @export
get_fields <- function(
  training,
  siren = 2,
  urssaf = 2,
  delai = 2,
  effectif =2,
  diane = 2,
  bdf = 2,
  apart = 2,
  procol = 2,
  interim = 0,
  target_encode =2,
  info = 0
) {

  fields <- c()
  if (siren >= 1 && !training) {
    fields <- c( fields, "siret",
    "siren", "periode", "code_ape", "code_ape_niveau2", "code_ape_niveau3",
    "code_naf", "libelle_naf", "libelle_ape5", "departement") }
  if (siren >= 1) {
    fields <- c( fields, "age_entreprise", "region")
  }

  if (urssaf >= 1) {
    fields <- c(
      fields,
      "montant_part_patronale",
      "montant_part_ouvriere",
      "montant_echeancier",
      "delai",
      "duree_delai",
      "ratio_dette",
      "ratio_dette_moy12m",
      "cotisation_moy12m"
    )
  }
  if (urssaf >= 2) {
    fields <- c(
      fields,
      "montant_part_patronale_past_1",
      "montant_part_ouvriere_past_1",
      "montant_part_patronale_past_2",
      "montant_part_ouvriere_past_2",
      "montant_part_patronale_past_3",
      "montant_part_ouvriere_past_3",
      "montant_part_patronale_past_6",
      "montant_part_ouvriere_past_6",
      "montant_part_patronale_past_12",
      "montant_part_ouvriere_past_12",
      "ratio_dette_delai",
      "debit_entreprise"
    )
  }

  if (apart >= 1) {
    fields <- c(
      fields,
      "apart_heures_consommees",
      "apart_heures_autorisees"
    )
  }

  if (apart >= 2) {
    fields <- c(
      fields,
      "apart_entreprise"
    )
  }
  if (effectif >= 1) {
    fields <- c(
      fields,
      "effectif",
      "effectif_ent"
    )
  }
  if (effectif >= 2) {
    fields <- c(
      fields,
      "effectif_past_6",
      "effectif_past_12",
      "effectif_past_18",
      "effectif_past_24"
    )
  }

  if (diane >= 1) {
    fields <- c(
      fields,
      "dette_fiscale_et_sociale",
      "effectif_consolide",
      "frais_de_RetD",
      "conces_brev_et_droits_sim",
      "nombre_etab_secondaire",
      "nombre_filiale",
      "taille_compo_groupe",
      "concours_bancaire_courant",
      "equilibre_financier",
      "independance_financiere",
      "endettement",
      "autonomie_financiere",
      "degre_immo_corporelle",
      "financement_actif_circulant",
      "liquidite_generale",
      "liquidite_reduite",
      "rotation_stocks",
      "credit_client",
      "credit_fournisseur",
      "taux_interet_financier",
      "taux_interet_sur_ca",
      "endettement_global",
      "taux_endettement",
      "capacite_remboursement",
      "capacite_autofinancement",
      "couverture_ca_fdr",
      "couverture_ca_besoin_fdr",
      "poids_bfr_exploitation",
      "exportation",
      "efficacite_economique",
      "productivite_potentiel_production",
      "productivite_capital_financier",
      "productivite_capital_investi",
      "taux_d_investissement_productif",
      "rentabilite_economique",
      "performance",
      "rendement_brut_fonds_propres",
      "rentabilite_nette",
      "rendement_capitaux_propres",
      "rendement_ressources_durables",
      "taux_marge_commerciale",
      "taux_valeur_ajoutee",
      "part_salaries",
      "part_etat",
      "part_preteur",
      "part_autofinancement",
      "ca",
      "ca_exportation",
      "achat_marchandises",
      "achat_matieres_premieres",
      "production",
      "marge_commerciale",
      "consommation",
      "autres_achats_charges_externes",
      "valeur_ajoutee",
      "charge_personnel",
      "impots_taxes",
      "subventions_d_exploitation",
      "excedent_brut_d_exploitation",
      "autres_produits_charges_reprises",
      "dotation_amortissement",
      "resultat_expl",
      "operations_commun",
      "produits_financiers",
      "charges_financieres",
      "interets",
      "resultat_avant_impot",
      "produit_exceptionnel",
      "charge_exceptionnelle",
      "participation_salaries",
      "impot_benefice",
      "benefice_ou_perte"
    )
  }

  if (diane >= 2) {
    fields <- c(
      fields,
      # DIANE PAST 1
      "effectif_consolide_past_1",
      "dette_fiscale_et_sociale_past_1",
      "frais_de_RetD_past_1",
      "conces_brev_et_droits_sim_past_1",
      "nombre_etab_secondaire_past_1",
      "nombre_filiale_past_1",
      "taille_compo_groupe_past_1",
      "concours_bancaire_courant_past_1",
      "equilibre_financier_past_1",
      "independance_financiere_past_1",
      "endettement_past_1",
      "autonomie_financiere_past_1",
      "degre_immo_corporelle_past_1",
      "financement_actif_circulant_past_1",
      "liquidite_generale_past_1",
      "liquidite_reduite_past_1",
      "rotation_stocks_past_1",
      "credit_client_past_1",
      "credit_fournisseur_past_1",
      "taux_interet_financier_past_1",
      "taux_interet_sur_ca_past_1",
      "endettement_global_past_1",
      "taux_endettement_past_1",
      "capacite_remboursement_past_1",
      "capacite_autofinancement_past_1",
      "couverture_ca_fdr_past_1",
      "couverture_ca_besoin_fdr_past_1",
      "poids_bfr_exploitation_past_1",
      "exportation_past_1",
      "efficacite_economique_past_1",
      "productivite_potentiel_production_past_1",
      "productivite_capital_financier_past_1",
      "productivite_capital_investi_past_1",
      "taux_d_investissement_productif_past_1",
      "rentabilite_economique_past_1",
      "performance_past_1",
      "rendement_brut_fonds_propres_past_1",
      "rentabilite_nette_past_1",
      "rendement_capitaux_propres_past_1",
      "rendement_ressources_durables_past_1",
      "taux_marge_commerciale_past_1",
      "taux_valeur_ajoutee_past_1",
      "part_salaries_past_1",
      "part_etat_past_1",
      "part_preteur_past_1",
      "part_autofinancement_past_1",
      "ca_past_1",
      "ca_exportation_past_1",
      "achat_marchandises_past_1",
      "achat_matieres_premieres_past_1",
      "production_past_1",
      "marge_commerciale_past_1",
      "consommation_past_1",
      "autres_achats_charges_externes_past_1",
      "valeur_ajoutee_past_1",
      "charge_personnel_past_1",
      "impots_taxes_past_1",
      "subventions_d_exploitation_past_1",
      "excedent_brut_d_exploitation_past_1",
      "autres_produits_charges_reprises_past_1",
      "dotation_amortissement_past_1",
      "resultat_expl_past_1",
      "operations_commun_past_1",
      "produits_financiers_past_1",
      "charges_financieres_past_1",
      "interets_past_1",
      "resultat_avant_impot_past_1",
      "produit_exceptionnel_past_1",
      "charge_exceptionnelle_past_1",
      "participation_salaries_past_1",
      "impot_benefice_past_1",
      "benefice_ou_perte_past_1",
      # DIANE PAST 2
      "effectif_consolide_past_2",
      "dette_fiscale_et_sociale_past_2",
      "frais_de_RetD_past_2",
      "conces_brev_et_droits_sim_past_2",
      "nombre_etab_secondaire_past_2",
      "nombre_filiale_past_2",
      "taille_compo_groupe_past_2",
      "concours_bancaire_courant_past_2",
      "equilibre_financier_past_2",
      "independance_financiere_past_2",
      "endettement_past_2",
      "autonomie_financiere_past_2",
      "degre_immo_corporelle_past_2",
      "financement_actif_circulant_past_2",
      "liquidite_generale_past_2",
      "liquidite_reduite_past_2",
      "rotation_stocks_past_2",
      "credit_client_past_2",
      "credit_fournisseur_past_2",
      "taux_interet_financier_past_2",
      "taux_interet_sur_ca_past_2",
      "endettement_global_past_2",
      "taux_endettement_past_2",
      "capacite_remboursement_past_2",
      "capacite_autofinancement_past_2",
      "couverture_ca_fdr_past_2",
      "couverture_ca_besoin_fdr_past_2",
      "poids_bfr_exploitation_past_2",
      "exportation_past_2",
      "efficacite_economique_past_2",
      "productivite_potentiel_production_past_2",
      "productivite_capital_financier_past_2",
      "productivite_capital_investi_past_2",
      "taux_d_investissement_productif_past_2",
      "rentabilite_economique_past_2",
      "performance_past_2",
      "rendement_brut_fonds_propres_past_2",
      "rentabilite_nette_past_2",
      "rendement_capitaux_propres_past_2",
      "rendement_ressources_durables_past_2",
      "taux_marge_commerciale_past_2",
      "taux_valeur_ajoutee_past_2",
      "part_salaries_past_2",
      "part_etat_past_2",
      "part_preteur_past_2",
      "part_autofinancement_past_2",
      "ca_past_2",
      "ca_exportation_past_2",
      "achat_marchandises_past_2",
      "achat_matieres_premieres_past_2",
      "production_past_2",
      "marge_commerciale_past_2",
      "consommation_past_2",
      "autres_achats_charges_externes_past_2",
      "valeur_ajoutee_past_2",
      "charge_personnel_past_2",
      "impots_taxes_past_2",
      "subventions_d_exploitation_past_2",
      "excedent_brut_d_exploitation_past_2",
      "autres_produits_charges_reprises_past_2",
      "dotation_amortissement_past_2",
      "resultat_expl_past_2",
      "operations_commun_past_2",
      "produits_financiers_past_2",
      "charges_financieres_past_2",
      "interets_past_2",
      "resultat_avant_impot_past_2",
      "produit_exceptionnel_past_2",
      "charge_exceptionnelle_past_2",
      "participation_salaries_past_2",
      "impot_benefice_past_2",
      "benefice_ou_perte_past_2"
    )
  }
  if (bdf >= 1) {
    fields <- c(
      fields,
      "taux_marge",
      "delai_fournisseur",
      "poids_frng",
      "financier_court_terme",
      "frais_financier",
      "dette_fiscale"
    )
  }

  if (bdf >= 2) {
    fields <- c(
      fields,
      # BDF PAST 1
      "taux_marge_past_1",
      "delai_fournisseur_past_1",
      "poids_frng_past_1",
      "financier_court_terme_past_1",
      "frais_financier_past_1",
      "dette_fiscale_past_1",
      # BDF PAST 2
      "taux_marge_past_2",
      "delai_fournisseur_past_2",
      "poids_frng_past_2",
      "financier_court_terme_past_2",
      "frais_financier_past_2",
      "dette_fiscale_past_2"
    )
  }

  if (procol >= 1 && !training) {
    fields <- c(
      fields,
      # ALTARES
      "outcome",
      "time_til_outcome",
      "etat_proc_collective"
    )
  }
  if (procol >= 2 && !training) {
    fields <- c(
      fields,
      "tag_failure",
      "tag_default",
      "tag_debit",
      "time_til_failure",
      "time_til_default"
    )
  }

  if (interim >= 1) {
    fields <- c(
      fields,
      "interim_proportion",
      "interim_ratio_past_6",
      "interim_ratio_past_12",
      "interim_ratio_past_18",
      "interim_ratio_past_24"
    )
  }

  if (target_encode >= 1 && training) {
    fields <- c(
      fields,
      "target_encode_code_ape_niveau2",
      "target_encode_code_ape_niveau3"
    )
  }
  if (info >= 1 && !training) {
    fields <- c(
      fields,
      "region",
      "departement",
      "arrete_bilan_diane",
      "exercice_bdf",
      "exercice_diane"
    )
  }
  return(fields)
}

#' Obtenir une liste allégée de champs à exporter
#'
#' TODO temporaire, retravailler get_fields.
#' task e58fb5d5-4bc1-410b-8287-c78e4fd442d0
#'
#' @export
get_fields_training_light <- function() {
  return(
    c(
      "apart_heures_consommees",
      "effectif_past_24",
      "montant_part_ouvriere_past_12",
      "montant_part_ouvriere_past_3",
      "montant_part_ouvriere_past_2",
      "montant_part_patronale_past_2",
      "montant_part_ouvriere_past_1",
      "montant_part_patronale_past_1",
      "montant_part_patronale",
      "ratio_dette",
      "ratio_dette_moy12m",
      "dette_fiscale_et_sociale_past_2",
      "frais_de_RetD_past_2",
      "independance_financiere_past_2",
      "endettement_past_2",
      "credit_client_past_2",
      "capacite_autofinancement_past_2",
      "exportation_past_2",
      "productivite_capital_investi_past_2",
      "rendement_capitaux_propres_past_2",
      "rendement_ressources_durables_past_2",
      "part_autofinancement_past_2",
      "charge_personnel_past_2",
      "frais_de_RetD_past_1",
      "endettement_past_1",
      "credit_client_past_1",
      "capacite_autofinancement_past_1",
      "exportation_past_1",
      "rentabilite_economique_past_1",
      "part_autofinancement_past_1",
      "valeur_ajoutee_past_1",
      "charge_personnel_past_1",
      "effectif_consolide",
      "frais_de_RetD",
      "concours_bancaire_courant",
      "endettement",
      "autonomie_financiere",
      "degre_immo_corporelle",
      "rotation_stocks",
      "credit_fournisseur",
      "productivite_capital_investi",
      "performance",
      "benefice_ou_perte",
      "taux_marge_past_2",
      "concours_bancaire_courant_past_2",
      "taux_marge_commerciale_past_2",
      "taux_marge_commerciale_past_1",
      "taux_marge_commerciale",
      "TargetEncode_code_ape_niveau2",
      "TargetEncode_code_ape_niveau3"
    )
  )
}

#' Récupère les dernières données disponibles pour un batch
#'
#' Cette fonction permet d'accéder rapidement aux dernières données
#' disponibles.
#'
#' @param last_batch `character(1)` \cr Batch auquel doit être importées les
#'   données. Les modifications opérées par les batchs ultérieurs sont
#'   ignorées.
#' @param periods `[Date()]` \cr Périodes d'intérêt, auquels charger les
#'   données. Des périodes supplémentairs peuvent être chargées selon la
#'   valeur de rollback_months.
#' @inheritParams mongodb_connection
#' @param fields `character()` \cr Noms des champs à requêter dans la base de
#'   données. Doit contenir "siret" et "periode". Si égal à \code{NULL}, alors
#'   charge tous les champs disponibles.
#' @param min_effectif `integer(1)` \cr Limite basse du filtrage de l'effectif
#'   (la limite est incluse)
#' @param rollback_months `integer(1)`\cr Nombre de mois précédant le premier mois de
#'   `periods` à charger. Permet d'effectuer des calculs de différences ou de
#'   moyennes glissantes pour les périodes d'intérêt.
#'
#' @return `data.frame()` \cr
#' Données avec les colonnes décrites dans `fields`, pour les périodes
#' définies par `periods` et `rollback_months`
#' @export
get_last_batch <- function(
  last_batch,
  periods,
  database,
  collection,
  mongodb_uri,
  fields,
  min_effectif,
  rollback_months) {
  current_data <- import_data(
    database,
    collection,
    mongodb_uri,
    last_batch,
    date_inf = min(periods) %m-% months(rollback_months),
    date_sup = max(periods) %m+% months(1),
    min_effectif = min_effectif,
    fields = fields
  )

  if ("periode" %in% fields && max(current_data$periode) != max(periods)) {
    logger::log_warn("Data is missing at actual period !")
  }
  return(current_data)
}
