% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/post_sector_correction.R
\name{get_conjuncture_predictions}
\alias{get_conjuncture_predictions}
\title{Train and predict a model on conjuncture data}
\usage{
get_conjuncture_predictions(
  task,
  df_conj = fetch_conj_data(),
  df_agg_failure = fetch_aggregated_sectors(task)
)
}
\arguments{
\item{df_conj}{données de conjoncture}

\item{df_agg_failure}{données d'agrégation sectorielle.}
}
\value{
\code{data.frame} with columns: prediction (in \link{0, 1}, n_month_period,
secteur)
}
\description{
Train and predict a model on conjuncture data
}
