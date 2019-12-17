#' Explains a single prediction
#'
#' @inheritParams generic_task
#' @param type `character()` \cr Ou bien "global", "local".
#' @param aggregation_matrix `data.frame()` \cr  Une matrice d'agrégation. Si
#' égal à NULL, alors les résultats ne sont pas agrégés.
#' @param group_name `character(1)` \cr Le nom de la colonne à utiliser dans
#'   le data.frame `aggregation_matrix`. Obligatoire si aggregation_matrix est
#'   différent de NULL.
#' @param data_to_explain `data.frame()` \cr Si type = "local" uniquement, les
#'   données (lignes de "hist_data" ou "new_data") à expliquer.
#'
#' @export
explain.sf_task <- function(
  task,
  type,
  aggregation_matrix = NULL,
  group_name = NULL,
  data_to_explain = NULL,
  ...
) {
  assertthat::assert_that(type %in% c("global", "local"))

  set_verbose_level(task)

  explainer <- switch(type,
    "global" = xgboost_global_explainer,
    "local" = xgboost_local_explainer
  )

  logger::log_info("Predictions are being explained")
  res <- explainer(
    task = task,
    aggregation_matrix = aggregation_matrix,
    group_name = group_name,
    data_to_explain = data_to_explain,
    ...
  )

  logger::log_info("Prediction breakdown completed.")
  return(res)
}

#' Explains global variable importance for an xgboost model
#'
#' Importance des variables d'un modèle xgboost, avec la possibilité
#' d'aggréger selon un matrice d'agrégation
#'
#' @inheritParams generic_task
#' @param aggregation_matrix `dataframe()` \cr
#'   Table de correspondance qui possède une colonne "variable" et une colonne
#'   correspondant au paramètre `group_name`. Si égal à NULL (défaut), alors les variables ne
#'   sont pas aggrégées. Utiliser `read_aggregation_matrix()` pour lire la
#'   table de corrsepondance.
#' @param group_name `character()` \cr Nom de la colonne de
#'   `aggregation_matrix` qui sert pour l'agrégation.
#'
xgboost_global_explainer <- function(
  task,
  aggregation_matrix,
  group_name,
  ...
) {
  imp <- xgboost::xgb.importance(task[["features"]], task[["model"]])
  if (!is.null(aggregation_matrix)) {
    imp <- aggregate_importance_frame(imp, aggregation_matrix, group_name)
  }
  return(imp)
}

#' Explication locale d'une prédiction
#'
#' @inheritParams generic_task
#' @inheritParams xgboost_global_explainer
#' @param siret `character()` \cr
#'   Vecteur de sirets pour lesquels une explication est requise.
xgboost_local_explainer <- function(
  task,
  aggregation_matrix,
  group_name,
  data_to_explain
  ) {

  if (requireNamespace("xgboostExplainer")) {

    explainer <- xgboostExplainer::buildExplainer(
      xgb.model = task[["model"]],
      trainingData = xgboost::xgb.DMatrix(task[["prepared_train_data"]]),
      type = "binary",
      base_score = 0.5,
      trees_idx = NULL
    )
    explain_task <- task
    explain_task[["new_data"]] <- data_to_explain
    explain_task <- prepare(explain_task, data_names = "new_data")
    data_to_explain <- explain_task[["prepared_new_data"]]
    pred_breakdown <- xgboostExplainer::explainPredictions(
      xgb.model = task[["model"]],
      explainer = explainer,
      data = data_to_explain
    )
    if (!is.null(aggregation_matrix)) {
      pred_breakdown <- aggregate_local_explainer(
        pred_breakdown,
        aggregation_matrix,
        group_name
      )
    }
    return(pred_breakdown)
  }
}

#' Agrège les facteurs d'importance selon une colonne d'une matrice
#' d'agrégation
#'
#' Les variables absentes de la matrice d'agrégation sont automatiquement
#'
#' @param frame `data.frame()` \cr
#'   Table d'importance comme produite en sortie de xgb.importance.
#' @inheritParams xgboost_global_explainer
aggregate_importance_frame <- function(
  frame,
  aggregation_matrix,
  group_name
) {
    merged <- dplyr::left_join(
      frame,
      aggregation_matrix,
      by = c("Feature" = "variable")
    )
    merged[, group_name][is.na(merged[, group_name])] <- "Autre"
    merged <- merged %>%
      dplyr::group_by_at(group_name) %>%
      dplyr::summarize_at(
        c("Gain", "Cover", "Frequency"),
        sum
      )
    colnames(merged)[1]  <- "Feature"
    return(merged)
}


#' Agrège les facteurs d'importance selon une colonne d'une matrice
#' d'agrégation
#'
#' @inheritParams aggregate_importance_frame
aggregate_local_explainer <- function(
  dt_frame,
  aggregation_matrix,
  group_name
) {
  t_frame <- as.data.frame(t(dt_frame))
  colnames(t_frame) <- paste0("Value", seq_len(nrow(dt_frame)))
  t_frame <- t_frame %>%
    dplyr::mutate(Feature = rownames(t_frame))

  # Add intercept
  aggregation_matrix <- aggregation_matrix  %>%
    dplyr::select(variable, dplyr::one_of(group_name))
  aggregation_matrix <- rbind(
    aggregation_matrix,
    c("intercept", "intercept")
    )

  merged <- dplyr::left_join(
    t_frame,
    aggregation_matrix,
    by = c("Feature" = "variable")
  )
  merged[, group_name][is.na(merged[, group_name])] <- "Autre"
  merged <- merged %>%
    dplyr::group_by_at(group_name) %>%
    dplyr::summarize_at(
      dplyr::vars(dplyr::starts_with("Value")),
      sum
    )
  colnames(merged)[1] <- "Feature"
  intercept_position <- which(merged$Feature == "intercept")
  new_order <- unique(c(
    intercept_position,
    order(abs(merged$Value1))
  ))
  merged <- merged[new_order, ]
  aggregated_frame <- data.table::data.table(t(as.matrix(
    merged %>% dplyr::select(dplyr::starts_with("Value"))
  )))
  names(aggregated_frame) <- merged$Feature

  return(aggregated_frame)
}

#' Lecture d'un fichier d'agrégation
#'
#' @param filepath   `character()`\cr
#'   Chemin d'accès du fichier d'agrégation à lire.
#' @export
read_aggregation_matrix <- function(filepath){
  aggregation <- read.csv(
    file = filepath,
    header = TRUE,
    stringsAsFactors = FALSE,
    fileEncoding = "latin1",
    sep = ";"
  )
  return(aggregation)
}


#' Affiche le graphique des importances relatives
#'
#' @param frame `data.table`
#'
#' @return
plot_waterfall <- function(frame){
  inverse_logit_labels <- function(x){
    return(
      paste0((1 / (1 + exp(-x))) * 100, "%")
    )
  }
  browser()
  breakdown_summary <- as.numeric(as.data.frame(frame)) %>%
    dplyr::mutate(is_negative = FALSE)
  logit = function(x){return(log(x/(1-x)))}
  ybreaks<-logit(c(0.001,0.002,0.005,0.01,0.02,0.05,0.1,0.2))
  #permet de créer des labels en probabilité par différence de probabilité
  cumulated_weight = cumsum(as.numeric(as.matrix(breakdown_summary)))
  cumulated_proba = 1 / (1 + exp(-cumulated_weight))
}


#' Plots  a correlation graph
#'
#' Représente graphihquement la matrice de corrélation
#'
#' @param task Tâche d'apprentissage avec un champs "hist_data"
#' @param fields `character()` \cr
#'  Sous-ensemble des variables pour lesquelles la corrélation doit être
#'  affichée.
#'
#' @export
plot_correlation <- function(task, fields){
  if (requireNamespace("corrplot")){
    data <- task[["hist_data"]] %>%
      select(fields)
    corrplot::corrplot(cor(data, use = "pairwise"), method = "color", type = "upper")
  }
}


