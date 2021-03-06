% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/post_sector_correction.R
\name{add_missing_first_month_aux}
\alias{add_missing_first_month_aux}
\title{Fonction utilitaire: Ajout d'un mois manquant dans le premier trimestre
avec des valeurs moyennes}
\usage{
add_missing_first_month_aux(data_secteur_n_months)
}
\arguments{
\item{data_secteur_n_months}{\code{data.frame()} Données sectorielles propres à
un secteur. Colonnes attendues: "periode", "count", "count_new_outcome",
"prop_new_outcome".}
}
\value{
le même data.frame avec une ligne pour la période 2015-01-01
moyennée sur les données de la première plage de "n_months" mois.
}
\description{
Cette fonction est une fonction utilitaire utilisée dans
aggregate_by_n_months, et prend un data.frame obtenu après un
\code{group_by(secteur, nom_secteur) \%>\% nest()} sur un data.frame de
défaillances agrégés par secteur. Notamment utilisé dans
aggregate_by_secteur
}
