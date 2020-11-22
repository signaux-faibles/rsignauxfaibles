context("Test des fonctions de correction de la prédiction")

data_secteur_semestre <- data.frame(
  periode = c(as.Date("2015-02-01"), as.Date("2015-03-01")),
  count = c(100, 150),
  count_new_outcome = c(10, 30),
  prop_new_outcome = c(0.1, 0.2)
)

testthat::test_that("add_missing_first_month_aux ajoute bien la ligne comme attendue", {
  actual <- add_missing_first_month_aux(data_secteur_semestre)
  expected <- bind_rows(
    data.frame(periode = as.Date("2015-01-01"), count = 125, count_new_outcome = 20, prop_new_outcome = 20 / 125),
    data_secteur_semestre
  )

  expect_equal(actual, expected)
})

testthat::test_that("add_missing_first_month ajoute bien la ligne comme attendue", {
  # On croise deux secteurs et deux semestres
  df <- bind_cols(
    bind_rows(
      data_secteur_semestre,
      data_secteur_semestre,
      data_secteur_semestre,
      data_secteur_semestre
    ),
    n_month_period = rep(c(as.Date("2015-01-01"), as.Date("2015-04-01")), 4),
    secteur = rep(c("secteur2", "secteur1"), each = 4),
    nom_secteur = rep(c("secteur2", "secteur1"), each = 4)
  )
  actual <- add_missing_first_month(df)
  expect_true(all(table(actual$n_month_period) == c(6, 4)))
  expect_true(all(table(actual$n_month_period, actual$secteur) == matrix(c(3, 2, 3, 2), nrow = 2)))
})
