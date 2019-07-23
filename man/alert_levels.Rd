% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{alert_levels}
\alias{alert_levels}
\title{Gives alert levels from prediction and F-scores}
\usage{
alert_levels(prediction, F1, F2)
}
\arguments{
\item{prediction}{Vector, list of predictions between 0 and 1.}

\item{F1}{\code{Double(1)}. F1_score}

\item{F2}{\code{Double(1)}. F2_score}
}
\value{
A factor vector with alert levels.
}
\description{
Lower thresholds are strict (a prediction falling on the threshold is
binned to the lower alert level)
}