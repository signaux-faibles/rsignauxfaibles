% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/io_export.R
\name{export_scores_to_mongodb}
\alias{export_scores_to_mongodb}
\title{Export des scores dans une collection mongodb}
\usage{
export_scores_to_mongodb(
  formatted_data,
  algo,
  batch,
  f_scores,
  database,
  collection,
  mongodb_uri
)
}
\arguments{
\item{formatted_data}{\code{data.frame()} \cr Données avec les champs "siret",
"periode", "score" et "score_diff". C'est le cas des données formatées par
\code{\link{format_for_export}}.}

\item{algo}{\code{character(1)} \cr Nom de l'algo qui figurera dans les objets
exportés}

\item{batch}{\code{character(1)} \cr Nom du batch qui figurera dans les objets
exportés}

\item{f_scores}{\code{character(2)} \cr Vecteur de scores F1 et F2. Doit être
nommé avec comme noms "F1" et "F2"}

\item{database}{\code{character(1)} \cr Nom de la base de données vers laquelle
param exporter.}

\item{collection}{`character(1)' \cr Nom de la collection vers laquelle
exporter.}

\item{mongodb_uri}{\code{character(1)} \cr url to the database in mongodb uri
format.}
}
\value{
Retourne TRUE. \cr Les objets insérés dans la base de données ont les
champs:
\itemize{
\item "_id" (ObjectId générés),
\item "alert", qui peut prendre les valeurs \emph{Alerte seuil F1}, \emph{Alerte seuil F2} et \emph{Pas d'alerte},
\item "algo" et "batch" tel qu'entrés en paramètres,
\item "siret", "periode", "score" et "score_diff" tel qu'extraits de la table \code{formatted_data},
\item "timestamp" qui donne la date et l'heure.
}
}
\description{
Exporte les scores vers une collection mongodb à partir des données formattées par la fonction
\code{\link{format_for_export}}.
}
