#' Optimize the hyperparameters of a model
#'
#' Optimise les hyperparamètres du modèle. Nécessite des données
#' d'entraînement et de validation préparées.
#'
#' @param task `[sf_task]` \cr Objet s3 de type sf_task
#' @param fields `character()` \cr Liste des variables pour l'entraînement. Cf
#' `[get_fields]` pour les variables par défaut.
#' @param n_init `integer(1)` \cr Nombre d'évaluations aléatoires initiales.
#' @param n_iter `integer(1)` \cr Nombre d'itérations (d'évaluations) d'optimisation.
#' @param train_pipe `function` \cr Fonction d'entraînement et d'évaluation
#'   compatible avec l'optimisation avec "mlrMBO" (donc avec des attributs
#'   spécifiques), notamment comme celles créées avec
#'   `[smoof::makeSingleObjectiveFunction]`
#' @param optim_bounds `ParamSet` \cr Objet précisant l'espace exploré par
#'   l'optimisation, tel que donné par `[ParamHelpers::ParamSet]`. Si NULL,
#'   alors des limites par défaut sont fixées.
#'
#' @describeIn optimize_hyperparameters
#'
#' @return La `task` donnée en entrée, auquel a été ajouté un champ
#' "model_parameters" avec les paramètres optimaux calculées.
#' @export
#'
optimize_hyperparameters.sf_task <- function( #nolint
  task,
  fields = get_fields(training = TRUE),
  n_init = NULL,
  n_iter = 12,
  train_pipe = NULL,
  optim_bounds = NULL,
  ...
  ) {

  requireNamespace("DiceKriging") #nolint
  requireNamespace("rgenoud")
  requireNamespace("mlrMBO") #nolint
  requireNamespace("ParamHelpers") #nolint

  if (is.null(optim_bounds)) {
    optim_bounds <- ParamHelpers::makeParamSet(
      ParamHelpers::makeNumericParam("learn_rate",  lower = 0.003, upper = 0.2),
      ParamHelpers::makeIntegerParam("max_depth", lower = 2L, upper = 12L),
      ParamHelpers::makeIntegerParam("ntrees", lower = 10L, upper = 300L),
      ParamHelpers::makeIntegerParam(
        "min_child_weight",
        lower = 1L,
        upper = 9L
      )
    )
  }

  if (is.null(train_pipe)) {
    #1 Objective function
    train_pipe <- smoof::makeSingleObjectiveFunction(
      name = "lgb_pipe",
      fn = function(
        x
        ) {

        new_task <- train(
          task,
          parameters = as.list(x),
          fields = fields
        )
        new_task <- predict(new_task, data_names = c("test_data"))
        new_task <- evaluate(
          new_task,
          data_name = c("test_data"),
          plot = FALSE
        )
        # TODO: Bayesian optimization criteria should be more flexible
        # 77d91ced-beac-4e91-9b48-e8fd16e956ee
        aucpr <- new_task[["model_performance"]] %>% .$evaluation %>% .[[1]]
        cat(aucpr)
        return(aucpr)
      },
      par.set = optim_bounds,
      minimize = FALSE
    )
  }

  set.seed(159)
  #2 Initial design
  if (!is.null(n_init)) {
    des <- ParamHelpers::generateDesign(n = n_init,
      par.set = ParamHelpers::getParamSet(train_pipe),
      fun = lhs::randomLHS)
  } else {
    des <- NULL
  }

  #3 Surrogate model
  surrogate  <- mlr::makeLearner(
    "regr.km",
    predict.type = "se",
    covtype = "matern3_2",
    control = list(trace = FALSE)
  )

  #4 Control
  control <- mlrMBO::makeMBOControl()
  control <- mlrMBO::setMBOControlTermination(
    control,
    iters = n_iter
  )
  control <- mlrMBO::setMBOControlInfill( #nolint
    control,
    crit = mlrMBO::makeMBOInfillCritEI() #nolint
  )

  #5 optimization
  run <- mlrMBO::mbo(fun = train_pipe,
    design = des,
    control = control,
    show.info = TRUE)

  task[["model_parameters"]] <- as.list(run$x)

  all_res <- run$opt.path$env$path
  if (is.null(n_init)) {
    n_init  <- 4 * smoof::getNumberOfParameters(train_pipe)
  }
  run$opt.path$env$path  %>%
    mutate(round = row_number()) %>%
    mutate(type = case_when(
        round <= n_init ~ "initial design",
        TRUE ~ "mlrMBO optimization")) %>%
    ggplot2::ggplot(ggplot2::aes(x = round, y = y, color = type)) +
    ggplot2::geom_point() +
    ggplot2::labs(title = "mlrMBO optimization")

  return(task)
}
