context("Test explain function")


test_task <- get_test_task()
train <- test_task[["train_data"]]
validation <- test_task[["validation_data"]]

Xtrain <- train
Xtrain$score <- NULL
ytrain <- train$target

Xtest <- validation
Xtest$score <-NULL
ytest <- validation$target
xgb_train_data <- xgboost::xgb.DMatrix(data.matrix(Xtrain), label = ytrain, missing = NA)
xgb_test_data <- xgboost::xgb.DMatrix(data.matrix(Xtest), missing = NA)

xgb_model <- xgboost::xgboost(
  data = xgb_train_data,
  nrounds = 3
)
xgb_preds <- predict(xgb_model, xgb_test_data)

test_task[["prepared_train_data"]] <- xgb_train_data
test_task[["prepared_validation_data"]] <- xgb_test_data
test_task[["model"]] <- xgb_model
test_task[["features"]] <- c("siret", "periode", "score")

test_that("model_explain works as expected", {
  without_aggregation <- xgboost_importance(test_task, NULL, NULL)
  with_aggregation <- xgboost_importance(
    test_task,
    data.frame(
      variable = c("siret", "periode", "score"),
      source = c("a", "a", "a")
      ),
    group_name = "source"
  )
})

test_that("xgboost_explainer works as expected", {
  without_aggregation <- xgboost_local_explainer(
    test_task,
    aggregation_matrix = NULL,
    data_to_explain = test_task[["prepared_validation_data"]]
    )

  with_aggregation <- xgboost_local_explainer(
    test_task,
    aggregation_matrix = dplyr::data_frame(
      variable = c("siret", "periode", "target"),
      source = c("a", "a", "a")
      ),
    group_name = "source",
    data_to_explain = test_task[["prepared_validation_data"]]
    )
})
