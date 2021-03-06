% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/task_operations.R
\name{copy_for_new_run}
\alias{copy_for_new_run}
\title{Copie et nettoie une tâche pour un nouvel entraînement}
\usage{
copy_for_new_run(task, end_run_status = NULL, keep_resampling = TRUE)
}
\arguments{
\item{task}{\code{sf_task}}

\item{end_run_status}{\code{c("FINISHED", "FAILED", "KILLED")} Si spécifié,
termine le précédent run de mlflow avec le statut donné. Sinon, ne termine
pas le précédent run.}

\item{keep_resampling}{\code{logical(1)} Faut-il garder l'échantillonnage de la
tâche mère ?}
}
\description{
Retourne une tâche d'apprentissage où les éléments du précédent
apprentissage sont nettoyées.
Les données ne sont pas copiées tant qu'elles ne sont pas modifiées.
Attention, une incohérence entre "hist_data" et "mlr3task" ne sera pas
corrigée.
}
