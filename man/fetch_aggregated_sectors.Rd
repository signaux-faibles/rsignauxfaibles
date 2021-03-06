% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/post_sector_correction.R
\name{fetch_aggregated_sectors}
\alias{fetch_aggregated_sectors}
\title{Requete la base de données pour récupérer les secteurs agrégés}
\usage{
fetch_aggregated_sectors(
  task,
  ape_to_naf = get_ape_to_naf(),
  ape_to_secteur = get_ape_to_secteur()
)
}
\arguments{
\item{task}{\verb{[sf_task]} \cr Objet s3 de type sf_task}

\item{ape_to_naf}{\code{data.frame} table de passage du code ape au code naf}

\item{ape_to_secteur}{\code{data.frame} table de passage du code ape au secteur}

\item{adjust}{\code{logical} Faut-il redresser la série ? (anomalies en base,
saisonnalité)}
}
\value{
\code{data.frame} avec les colonnes "code_ape", "periode", "count" qui
correspond au nombre d'observations total, "count_outcome" qui compte le
nombre de défaillances/défauts à 18 mois, "count_new_outcome", qui compte
le nombre de défaillances/défauts à 1 mois, "secteur", "code_naf"
}
\description{
La collection "secteurs" contient des informations agrégés par code_ape et
periode, pour les entreprises avec un effectif connu supérieur à 10.
}
\details{
Le mois de janvier 2015 est retiré des données, car ce mois présente un
pic dû au fait qu'on prend en compte les données de défaut URSSAF
uniquement à partir de décembre 2014 (12 mois d'observations pour la
cotisation moyenne), ce qui créé artificiellement des
time_til_outcome == 1 en janvier 2015.

/!\ Le premier semestre ne dure en conséquence que 5 mois, penser à
redresser en cas d'agrégation.
}
