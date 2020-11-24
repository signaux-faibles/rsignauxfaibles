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
Le paramètre additionnel `ref = <nom_de_branche>` permet de consulter au besoin 
une autre branche que master. Si le package est déjà en mémoire au moment de 
télécharger la nouvelle version, alors précéder l'installation de:
```r
detach("package:rsignauxfaibles", unload = TRUE)
```
puis redémarrer R. 

## Entraîner le modèle et prédire 

Il faut connaître les identifiants de la base de données.

```r
library(rsignauxfaibles)
database <- "<database_name>"
collection <- "<collection_name>"
mongodb_uri <- "<mongodb://uri>"

# Création d'une tâche d'apprentissage
# C'est un objet de type "S3", donc une liste dans laquelle sont stockées les 
# informations de la tâche d'apprentissage
task <- sf_task(
  mongodb_uri,
  database,
  collection
)

last_batch <- "2009_5" # Nom du dernier batch importé

# Échantillonnage aléatoire pour l'entraînement de 10.000 
# couples (siret x période).  (en réalité 800.000 en production)
sample_size <- 10000 

# Chargement des données historiques (par défaut, 2015 et 2016)
task <- load_hist_data(
  task = task,
  batch = last_batch,
  subsample = sample_size
)


# Utilitaire pour lister les champs
fields <- get_fields(training = TRUE) 

# Si l'on entraîne sur toute la base, pas besoin de créer d'échantillons de 
# test. Se référer au script de test pour voir comment faire. 

# prepare spécifie une suite d'opérations sur les données, sous la forme d'un
# `mlr3pipelines::Graph` ou assimilable. 
# Les opérations ne seront effectuées qu'au moment de l'entraînement.
task <- prepare(
  task,
  training_fields = fields
)

# Si l'on veut tout de même consulter l'effet de la préparation sans entraîner 
# de modèle, on peut utiliser:
prepared_data <- get_prepared_data(task, data = task$mlr3task$data())

# Entraînement avec le modèle par défaut (gam)
task <- train(task)
# Ou alors avec un autre modèle (issu de mlr3 ou mlr3learners)
task <- train(task, learner = mlr3::lrn("classif.rpart"))

# Chargement de nouvelles données
last_period <- as.Date("2020-01-01")  # Dernière période importée
task <- load_new_data(
  task = task,
  periods = last_period,
  batch = last_batch
  )

# Prédiction sur les nouvelles données
task <- predict(task, data_names = "new_data")

# Les prédictions sont au format mlr3::Predictions, mais peuvent être 
# converties 
require(data.table)
prediction <- as.data.table(task$prediction_new)
```

# Évaluer et comparer les modèles

Pour explorer les modèles, on peut utiliser `split_data` pour faire de la 
validation croisée. On peut alors utiliser en plus la fonction `evaluate` qui 
permet de mesurer la performance et de comparer les modèles, ou encore 
`optimize_hyperparameters` qui permet de faire un grid_search sur les 
paramètres du modèle. 

```r
library(rsignauxfaibles)
library(dplyr)
database <- "<database_name>"
collection <- "<collection_name>"
mongodb_uri <- "<mongodb://uri>"

task <- sf_task(
  mongodb_uri,
  database,
  collection
)
last_batch <- "2009_5" 
sample_size <- 10000
task <- load_hist_data(
  task = task,
  batch = last_batch,
  subsample = sample_size
)

fields <- get_fields(training = TRUE)

# Les opérations peuvent être enchaînées via l'opérateur pipe
gam_task <- task %>%
  split_data(resampling_strategy = "cv") %>%
  prepare(training_fields = fields) %>%
  train()

# Créons un autre modèle pour comparer.
# Pour cela, on utilise "copy_for_new_run()", qui ne conserve que l'essentiel
# afin de ne pas mélanger les résultats de modèles.
# Les modèles doivent être définis sous la forme de mlr3::Learner.
learner <- mlr3::lrn("classif.rpart") # Arbre de décisions
rpart_task <- gam_task %>%
  copy_for_new_run() %>%
  prepare(training_fields = fields) %>%
  train(learner = learner)

# On peut alors comparer la performance des modèles
evaluation <- evaluate(gam_task, rpart_task, should_remove_strong_signals =
FALSE)

# Vous pouvez à tout moment inspecter les objets mlr3 sous-jacents à la tâche
# d'apprentissage:
task$mlr3task # mlr3::ClassifTask
task$mlr3rsmp # mlr3::Resampling: stratégie d'échantillonnage
task$mlr3pipeline # mlr3pipelines::Graph: Pipeline de préparation
task$mlr3graphlearner # mlr3pipelines::GraphLearner: Préparation + modèle à entraîner
task$mlr3resample_result # mlr3::ResampleResult: après entraînement

# Pour faire de l'optimisation d'hyperparamètres, que ce soit de
# l'entraînement ou de la préparation.
# Optimise les paramètre du "GraphLearner" stocké dans la tâche.
# Le modèle résultant se trouve dans task$mlr3auto_tuner
gam_task <- optimize_hyperparameters(gam_task, measure = mlr3::msr("classif.ce"))
```

# Documentation

Les fonctions sont documentées, mais comme une `sf_task` est un type `S3`, 
l'accès à la documentation de certaines fonctions se fait en ajoutant le 
suffixe ".sf_task" au nom de la fonction; par exemple: 

```r
?train.sf_task
?predict.sf_task
```

Une documentation du modèle et de la procédure d'évaluation est disponible 
[ici](https://github.com/signaux-faibles/documentation/blob/master/algorithme-evaluation.md). 
