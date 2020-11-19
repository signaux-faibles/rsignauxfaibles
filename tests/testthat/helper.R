# Executed before the tests
requireNamespace("lgr")
previous_threshold <- lgr::lgr$threshold
lgr::lgr$set_threshold("error")
logger <- lgr::get_logger("mlr3")
logger$set_threshold("error")
