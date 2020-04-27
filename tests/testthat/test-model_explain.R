# context("Test explain function")
#
# test_task <- get_test_task_2()
#
# test_task <- train(test_task)
# test_task <- predict(test_task, data_names = "test_data")
#
# test_aggregation_matrix <- data.frame(
#   variable = c(
#     "effectif",
#     "excedent_brut_d_exploitation",
#     "taux_marge",
#     "montant_part_patronale"
#   ),
#    group = c("effectif", "financier", "financier", "urssaf"),
#    stringsAsFactors = FALSE
#    )
#
# test_that("model_explain works as expected", {
#   without_aggregation <- explain(test_task, type = "global")
#
#   with_aggregation <- explain(
#     test_task,
#     type = "global",
#     aggregation_matrix = test_aggregation_matrix,
#     group_name = "group"
#   )
# })
#
# test_that("xgboost_explainer works as expected", {
#   without_aggregation <- explain(
#     test_task,
#     type = "local",
#     data_to_explain = test_task[["validation_data"]]
#     )
#
#   with_aggregation <- explain(
#     test_task,
#     type = "local",
#     aggregation_matrix = test_aggregation_matrix,
#     group_name = "group",
#     data_to_explain = test_task[["validation_data"]]
#     )
# })
#
# test_that("plot_waterfall works as expected", {
#   without_aggregation <- explain(
#     test_task,
#     type = "local",
#     data_to_explain = test_task[["validation_data"]]
#     )
#   plot_waterfall(without_aggregation[7,])
#
# })
