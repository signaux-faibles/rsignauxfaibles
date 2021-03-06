% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/io_export.R
\name{format_for_export}
\alias{format_for_export}
\title{Prepare_for_export}
\usage{
format_for_export(
  data_to_export,
  export_fields,
  database,
  collection,
  mongodb_uri,
  last_batch,
  known_sirens_full_path
)
}
\arguments{
\item{data_to_export}{\code{data.frame(1)} \cr Données à exporter. Doit avoir
les champs "siret", "periode", et "score".}

\item{export_fields}{\code{character()} \cr Liste des champs à charger pour
enrichir les scores. Ces champs sont chargés directement depuis la base
avec les informations de connection.}

\item{database}{\code{character(1)} \cr Nom de la base de données vers laquelle
param exporter. Par défaut, celle stockée dans \code{task}.}

\item{collection}{\code{character(1)} \cr Nom de la collection vers laquelle
exporter. Par défaut, celle stockée dans \code{task}.}

\item{mongodb_uri}{\code{character(1)} \cr url to the database in mongodb uri
format.}

\item{last_batch}{\code{character(1)} \cr Nom du batch à prendre en compte pour
les données.}

\item{known_sirens_full_path}{\code{character()} \cr Chemins absolus des
fichiers contenant des listes de sirens connus.}
}
\value{
\code{data.frame()}\cr
Données formatées avec les champs "siret", "periode", "score",
"score_diff", "connu" ainsi que les champs spécifiés dans
\code{export_fields}.
}
\description{
Si le nombre de périodes disponible dans les données est supérieur à deux,
alors la dernière période disponible est absente du data.frame de retour.
}
