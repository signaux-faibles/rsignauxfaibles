# Entraînement et prédictions du modèle signaux faibles

Ce package contient les éléments qui permettent de charger les données, 
d'entraîner un modèle et de l'appliquer aux dernières données disponibles. 

Il est également utilisé dans le cadre de l'expérimentation de différents 
modèles. 

Il s'agit essentiellement de fonctions wrapper autour de mlr3 et de requêtes 
vers la base de données. 

## Installation

L'installation se fait avec le package `devtools`:
```r
devtools::install_github("signaux-faibles/rsignauxfaibles")
```

## Exemple de code

Il faut connaître les identifiants de la base de données.

```r
library(rsignauxfaibles)
database <- "<database_name>"
collection <- "<collection_name>"
mongodb_uri <- "<mongodb://uri>"

# Création d'une tâche d'apprentissage
task <- sf_task(
  mongodb_uri,
  database,
  collection
)

last_batch <- "2005_2" # Nom du dernier batch importé
sample_size <- 10000 # Échantillonnage pour l'entraînement de 10.000 couples (siret x période). 
# (en réalité 800.000 en production)

# Chargement des données historiques
task <- load_hist_data(
  task = task,
  batch = last_batch,
  subsample = sample_size
)

# On entraîne sur toute la base.
task <- split_data(task, resampling_strategy = "none") 

# Utilitaire pour lister les champs
fields <- get_fields(training = TRUE) 

# Pipeline de préparation des données. task <- prepare(
  task,
  training_fields = training_fields
)

# Entraînement avec le modèle par défaut (xgboost)
task <- train(task)
# Ou alors avec un autre modèle (issu de mlr3 ou mlr3learners)
task <- train(task, learner = mlr3::lrn("classif.rpart"))

# Chargement de nouvelles données
last_period <- as.Date("2020-01-01")  # Dernière période importée task <- 
load_new_data(
  task = task,
  periods = last_period,
  batch = last_batch
  )

# Prédiction sur les nouvelles données
task <- predict(task, data_names = "new_data")
```

Pour explorer les modèles, on peut remplacer `"none"` par `"cv"` ou `"holdout"` 
pour faire de la validation croisée. On peut alors utiliser en plus la fonction 
`evaluate` qui permet de mesurer la performance et de comparer les modèles, ou 
encore `optimize_hyperparameters` qui permet de faire un grid_search sur les 
paramètres du modèle. 

 






 




