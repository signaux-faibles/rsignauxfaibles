#' Export de données
#'
#' Exporte les données.
#'
#' @inheritParams generic_task
#' @param export_type `"csv" | "mongodb"` \cr Export en csv, ou en
#'   mongodb ?
#' @param batch description
#' @param f_scores `numeric(2)` \cr F_scores as obtained on test data. Names
#'   must be "F1" and "F2".
#' @param known_sirens_full_path `character()` \cr  Un ou plusieurs chemins de
#'   fichiers absolus contenant des sirens (un siren par ligne). Les
#'   établissements de ces entreprises seront marqués comme connus.
#' @param export_fields `character()` \cr Champs à exporter (only for csv
#'   export)
#' @param csv_export_path `character()` \cr Chemin du répertoire pour
#'   l'export csv.
#' @inheritParams mongodb_connection
#' @param collection_features `character()` \ cr Collection mongodb où
#' récupérer des données supplémentaires
#' @param collection_scores `character()` \cr Collection mongodb stockant les
#'   scores calculés (utilisée en écriture).
#' @param ... additional parameters for export functions.
#' @return `sf_task` \cr L'objet `task` donné en entrée.
#'
#' @export
export.sf_task <- function(
                           task,
                           export_type,
                           batch,
                           f_scores = c(F1 = 0.15, F2 = 0.07),
                           known_sirens_full_path = NULL,
                           export_fields = NULL,
                           csv_export_path = NULL,
                           database = task[["database"]],
                           mongodb_uri = task[["mongodb_uri"]],
                           collection_features = task[["collection"]],
                           collection_scores = "Scores",
                           algo_name = "algo",
                           ...) {
  requireNamespace("purrr")
  if (is.null(export_fields)) {
    export_fields <- c(
      "siret",
      "periode",
      "raison_sociale",
      "departement",
      "region",
      "score",
      "score_diff",
      "connu",
      "date_ccsf",
      "etat_proc_collective",
      "date_proc_collective",
      "interessante_urssaf",
      # "default_urssaf",
      "effectif",
      "libelle_naf",
      "libelle_ape5",
      "code_ape",
      "montant_part_ouvriere",
      "montant_part_patronale",
      "ca",
      "ca_past_1",
      "benefice_ou_perte",
      "benefice_ou_perte_past_1",
      "resultat_expl",
      "resultat_expl_past_1",
      "poids_frng",
      "taux_marge",
      "frais_financier",
      "financier_court_terme",
      "delai_fournisseur",
      "dette_fiscale",
      "apart_heures_consommees",
      "apart_heures_autorisees",
      # "cotisation_moy12m",
      "compte_urssaf",
      "montant_majorations",
      "exercice_bdf",
      "exercice_diane",
      "delai"
    )
  }


  if (!is.null(export_type)) {
    assertthat::assert_that(all(export_type %in% c("csv", "mongodb")))

    lgr::lgr$info("Adding additional fields for export")

    res <- task[["new_data"]] %>%
      format_for_export(
        export_fields = export_fields,
        database = database,
        collection = collection_features,
        mongodb_uri = mongodb_uri,
        last_batch = batch,
        known_sirens_full_path = known_sirens_full_path
      )

    lgr::lgr$info(
      "Data is exported to {paste(export_type, collapse = ' and ')}"
    )
    purrr::walk(
      .x = export_type,
      .f = function(x, ...) {
        if (x == "csv") {
          export_scores_to_csv(
            ...,
            absolute_path = csv_export_path
          )
        } else if (x == "mongodb") {
          export_scores_to_mongodb(
            ...,
            f_scores = f_scores,
            database = database,
            collection = collection_scores,
            mongodb_uri = mongodb_uri
          )
        }
      },
      formatted_data = res,
      batch = batch,
      algo = algo_name
    )
  }
  lgr::lgr$info("Data exported with success to
    {paste(export_type, collapse = ' and ')}")
  return(task)
}

#' Prepare_for_export
#'
#' Si le nombre de périodes disponible dans les données est supérieur à deux,
#' alors la dernière période disponible est absente du data.frame de retour.
#'
#' @param data_to_export `data.frame(1)` \cr Données à exporter. Doit avoir
#'   les champs "siret", "periode", et "score".
#' @param export_fields `character()` \cr Liste des champs à charger pour
#'   enrichir les scores. Ces champs sont chargés directement depuis la base
#'   avec les informations de connection.
#' @inheritParams mongodb_connection
#' @param last_batch `character(1)` \cr Nom du batch à prendre en compte pour
#'   les données.
#' @param known_sirens_full_path `character()` \cr Chemins absolus des
#'   fichiers contenant des listes de sirens connus.
#'
#' @return `data.frame()`\cr
#'   Données formatées avec les champs "siret", "periode", "score",
#'   "score_diff", "connu" ainsi que les champs spécifiés dans
#'   `export_fields`.
#' @export
format_for_export <- function(
                              data_to_export,
                              export_fields,
                              database,
                              collection,
                              mongodb_uri,
                              last_batch,
                              known_sirens_full_path) {
  requireNamespace("lgr")

  prediction <- data_to_export %>%
    select(siret, periode, score)

  all_periods <- prediction %>%
    arrange(periode) %>%
    .$periode %>%
    unique()

  if (length(all_periods) < 2) {
    lgr::lgr$warn(
      "Less than two periods do not allow to compute score variations,
      or to monitor company appearing since last period. You can add more
      periods with the rollback_months parameter from load_new_data function"
    )
  }

  # Compute additional indicators about progression
  pred_data <- prediction %>%
    dplyr::group_by(siret) %>%
    dplyr::arrange(siret, periode) %>%
    dplyr::mutate(
      last_score = dplyr::lag(score)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(score_diff = score - last_score) %>%
    dplyr::select(-c(last_score))

  if (length(all_periods) >= 2) {
    pred_data <- pred_data %>%
      dplyr::filter(periode > min(all_periods), periode <= max(all_periods))
  }

  first_period <- min(all_periods)
  last_period <- max(all_periods)
  lgr::lgr$info("Preparation a l'export ... ")
  lgr::lgr$info("Derniere periode connue: %s", last_period)

  donnees <- import_data(
    database = database,
    collection = collection,
    mongodb_uri = mongodb_uri,
    batch = last_batch,
    date_inf = first_period,
    date_sup = last_period %m+% months(1),
    min_effectif = 10,
    fields = export_fields[!export_fields %in% c(
      "connu", "score_diff", "score"
    )]
  )

  donnees <- pred_data %>%
    mutate(siret = as.character(siret)) %>%
    left_join(donnees %>% mutate(siret = as.character(siret)),
      by =
        c("siret", "periode")
    ) %>%
    dplyr::mutate(CCSF = date_ccsf) %>%
    dplyr::arrange(dplyr::desc(score), siret, dplyr::desc(periode))

  # Report des dernieres infos financieres connues

  # TODO couper nom / chemin du dossier
  if (!is.null(known_sirens_full_path)) {
    donnees <- donnees %>%
      mark_known_sirets(
        full_paths = unique(dirname(known_sirens_full_path))
      ) %>%
      select(export_fields)
  } else {
    donnees <- donnees %>%
      dplyr::mutate(connu = FALSE) %>%
      dplyr::select(export_fields)
  }

  all_names <- names(donnees)
  lgr::lgr$info("Les variables suivantes sont absentes du dataframe:
    {export_fields[!(export_fields %in% all_names)]}")
  export_fields <- export_fields[export_fields %in% all_names]


  # if (is.emp)
  to_export <- donnees %>%
    dplyr::select(one_of(export_fields))
  return(to_export)
}



#' Export des scores dans une collection mongodb
#'
#' Exporte les scores vers une collection mongodb à partir des données formattées par la fonction
#' \code{\link{format_for_export}}.
#'
#' @param formatted_data `data.frame()` \cr Données avec les champs "siret",
#'   "periode", "score" et "score_diff". C'est le cas des données formatées par
#'   \code{\link{format_for_export}}.
#' @param algo `character(1)` \cr Nom de l'algo qui figurera dans les objets
#'   exportés
#' @param batch `character(1)` \cr Nom du batch qui figurera dans les objets
#'   exportés
#' @param f_scores `character(2)` \cr Vecteur de scores F1 et F2. Doit être
#'   nommé avec comme noms "F1" et "F2"
#' @param database `character(1)` \cr Nom de la base de données vers laquelle
#'   param exporter.
#' @param collection `character(1)' \cr Nom de la collection vers laquelle
#'   exporter.
#' @inheritParams mongodb_connection
#'
#' @return Retourne TRUE. \cr Les objets insérés dans la base de données ont les
#'   champs:
#'   * "_id" (ObjectId générés),
#'   * "alert", qui peut prendre les valeurs _Alerte seuil F1_, _Alerte seuil F2_ et _Pas d'alerte_,
#'   * "algo" et "batch" tel qu'entrés en paramètres,
#'   * "siret", "periode", "score" et "score_diff" tel qu'extraits de la table \code{formatted_data},
#'   * "timestamp" qui donne la date et l'heure.
#'
#' @export
export_scores_to_mongodb <- function(
                                     formatted_data,
                                     algo,
                                     batch,
                                     f_scores,
                                     database,
                                     collection,
                                     mongodb_uri) {
  exported_columns <- c("siret", "periode", "score", "score_diff")
  assertthat::assert_that(
    all(exported_columns %in% names(formatted_data)),
    msg = paste(
      paste0(exported_columns, collapse = ", "),
      "are compulsary column names to export the scores to mongodb"
    )
  )
  assertthat::assert_that(is.character(batch) && length(batch) == 1,
    msg = "Batch shoud be a length 1 character vector"
  )
  assertthat::assert_that(is.character(database) && length(database) == 1,
    msg = "Database shoud be a length 1 character vector"
  )
  assertthat::assert_that(is.character(collection) && length(collection) == 1,
    msg = "Collection shoud be a length 1 character vector"
  )

  assertthat::assert_that(length(f_scores) == 2)
  assertthat::assert_that(all(c("F1", "F2") %in% names(f_scores)))

  dbconnection <- mongolite::mongo(
    collection = collection,
    db = database,
    url = mongodb_uri,
    verbose = FALSE
  )

  data_to_export <- formatted_data %>%
    dplyr::select(dplyr::one_of(exported_columns)) %>%
    dplyr::mutate(
      algo = algo,
      batch = batch,
      timestamp = Sys.time()
    )
  data_to_export <- data_to_export %>%
    mutate(alert = alert_levels(score, f_scores["F1"], f_scores["F2"]))

  dbconnection$insert(data_to_export)

  return(TRUE)
}

#' Export des scores dans un tableau csv
#'
#' Exporte les scores vers un tableau csv à partir des données formattées par la fonction
#' \code{\link{format_for_export}}.
#'
#' @param formatted_data `data.frame()` \cr Données à exporter. Typiquement,
#'   le data.frame retourné par la fonction \code{\link{format_for_export}}.
#' @param algo `character(1)` \cr Nom de l'algo qui figurera dans les objets
#'   exportés
#' @param batch `character(1)` \cr Nom du batch qui figurera dans les objets
#'   exportés
#' @param absolute_path `character(1)` \cr Chemin relatif du dossier dans lequel effectuer
#'   l'export, par rapport à la racine du paquet R.
#'
#'
#' @return `TRUE`. \cr Exporte un fichier csv, dans le dossier spécifié
#'   en entrée, nommé automatiquement "aaaa_mm_jj_export_{algo}_{batch}" avec
#'   éventuellement un suffixe "_vX" pour ne pas écraser de fichier existant.
export_scores_to_csv <- function(
                                 formatted_data,
                                 algo,
                                 batch,
                                 absolute_path) {
  assertthat::assert_that(is.character(batch) && length(batch) == 1,
    msg = "Batch shoud be a length 1 character vector"
  )

  data_to_export <- formatted_data

  # create unique filename
  full_path <- name_file(
    absolute_path,
    file_detail = paste("export", algo, batch, sep = "_"),
    file_extension = "csv",
    full_path = TRUE
  )

  utils::write.table(data_to_export,
    row.names = F,
    dec = ",",
    sep = ";",
    file = full_path,
    quote = T,
    append = F
  )

  return(TRUE)
}

#' Marks sirets from files as "known"
#'
#' Récupère des sirens d'une liste de fichier et ajoute une colonne à un
#' data.frame pour spécifier dans une colonne "connu" si le siren est présent dans l'un de ces
#' fichiers.
#'
#' @param df `data.frame()` \cr Table avec au moins une colonne "siret"
#' @param full_paths `character()` \cr Chemin d'accès absolu
#'
#' @return `data.frame()` \cr Table donnée en entrée à laquelle a été apposé
#' une colonne "connu", qui vaut 1 si le siren correspondant a été trouvé dans
#' les fichiers spécifiés, 0 sinon.
#'
#' @export
#'
mark_known_sirets <- function(df, full_paths) {
  sirens <- c()
  for (full_path in full_paths) {
    sirets <- readLines(full_path)
    sirens <- c(sirens, substr(sirets, 1, 9))
  }
  df <- df %>%
    dplyr::mutate(connu = as.numeric(substr(siret, 1, 9) %in% sirens))

  return(df)
}

#' Récupération des scores
#'
#' @inheritParams mongodb_connection
#' @param algo `character(1)`\cr Algorithm from which to take the score. Defaults to "algo".
#' @param method `character(1)`\cr Ou bien "first" ou bien "last". Se référer
#'   à la section Details.
#' @param batchs `character()` \cr Vecteur de batchs à requêter
#' @param sirets `character()` \cr Vecteur de sirets à requêter
#'
#' @section Details:
#' Si la méthode est "first", alors le dernier score disponible pour le
#' **premier** batch disponible pour cette période est récupéré. Sinon, le
#' dernier score disponible pour le **dernier** batch disponible est récupéré.
#' "first" représente plus fidèlement les scores qui ont été obtenues par le
#' passé, là où "last" représente les scores qui auraient été obtenues avec
#' le dernier algorithme en date (même si ce n'est pas toujours le cas, ça
#' dépend des périodes qui ont été exportées avec cet algorithme).
#'
#' @return Retourne un `data.frame` avec des colonnes "siret", "periode", "score"
#' (probabilité de défaillance selon la définition signaux-faibles), "batch"
#' (Nom du batch d'intégration pour lequel le score a été calculé), "algo"
#' (nom de l'algorithme utilisé) et "timestamp" (date et heure d'intégration des scores
#' à la base de données)
#'
#' @export
#'
get_scores <- function(
                       database = "test_signauxfaibles",
                       collection = "Scores",
                       mongodb_uri,
                       method = "last",
                       algo = NULL,
                       batchs = NULL,
                       sirets = NULL) {
  assertthat::assert_that(is.character(database) && length(database) == 1,
    msg = "Database name should be a length 1 string"
  )
  assertthat::assert_that(is.character(collection) && length(collection) == 1,
    msg = "Collection name should be a length 1 string"
  )
  assertthat::assert_that(tolower(method) %in% c("first", "last"))

  assertthat::assert_that(is.null(algo) || grepl("^[[:alnum:]_]+$", algo),
    msg = "Please use alphanumeric and underscore characters for algorithm name"
  )

  assertthat::assert_that(all(
    grepl("^[0-9]+$", sirets)
  ),
  msg = "Sirets should only be numeric characters"
  )

  result <- data.frame(
    siret = character(),
    periode = as.Date(character()),
    score = double(),
    batch = character()
  )

  admin <- mongolite::mongo(db = "admin", url = mongodb_uri)
  db_names <- admin$run('{"listDatabases":1}')$databases$name
  assertthat::assert_that(database %in% db_names,
    msg = "Database not found"
  )

  db <- mongolite::mongo(
    db = database,
    collection = collection,
    url = mongodb_uri
  )
  col_names <- db$run('{"listCollections":1}')$cursor$firstBatch$name # nolint
  assertthat::assert_that(collection %in% col_names,
    msg = "Collection not found"
  )

  if (tolower(method) == "first") {
    batch_sort <- 1
  } else if (tolower(method) == "last") {
    batch_sort <- -1
  }

  if (!is.null(sirets)) {
    siret_match <- paste0(
      '"siret" : {"$in" : [',
      paste0(paste0('"', sirets, '"'), collapse = ", "),
      "]}"
    )
  } else {
    siret_match <- NULL
  }
  if (!is.null(batchs)) {
    batch_match <- paste0(
      '"batch" : {"$in" : [',
      paste0(paste0('"', batchs, '"'), collapse = ", "),
      "]}"
    )
  } else {
    batch_match <- NULL
  }
  if (!is.null(algo)) {
    algo_match <- paste0('"algo" : "', algo, '"')
  } else {
    algo_match <- NULL
  }

  aggregation <- paste0(
    '[{ "$match" : {',
    paste(c(siret_match, batch_match, algo_match), collapse = ","),
    '}
    }, {
      "$sort" : {
      "siret" : 1.0,
      "batch" : ', batch_sort, ',
      "periode" : 1.0,
      "timestamp" : -1.0
        }
     },
     {
       "$group" : {
         "_id" : {
           "siret" : "$siret",
           "periode" : "$periode",
           "algo" : "$algo"
         },
         "score" : {
           "$first" : "$score"
         },
         "batch" : {
           "$first" : "$batch"
         },
         "timestamp" : {
           "$first" : "$timestamp"
         },
         "alert" : {
           "$first" : "$alert"
         },
         "score_diff" : {
           "$first" : "$score_diff"
         }
       }
     },
     {
       "$addFields" : {
         "siret" : "$_id.siret",
         "periode" : "$_id.periode",
         "algo" : "$_id.algo"
       }
     },
     {
       "$project" : {
         "_id" : 0.0
       }
     }
]'
  )

  query <- db$aggregate(
    aggregation
  )

  if (nrow(query) >= 1) result <- query

  return(result)
}
