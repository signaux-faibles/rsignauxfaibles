% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/io_database.R
\name{get_sirets_of_detected}
\alias{get_sirets_of_detected}
\title{Get sirets of companies detected by SF}
\usage{
get_sirets_of_detected(database = "test_signauxfaibles",
  collection = "Scores")
}
\arguments{
\item{database}{\code{character(1)} \cr Nom de la base de données vers laquelle
param exporter. Par défaut, celle stockée dans \code{task}.}

\item{collection}{\code{character(1)} \cr Nom de la collection vers laquelle
exporter. Par défaut, celle stockée dans \code{task}.}
}
\value{
vector of unique sirets
}
\description{
Under construction: TODO
\itemize{
\item Possibility to filter by batch, algo, periods
\item custom threshold (F1, F2, other)
}
}