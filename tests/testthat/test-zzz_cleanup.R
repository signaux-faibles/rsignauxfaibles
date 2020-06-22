# Exectuted after the tests
lgr::lgr$set_threshold(previous_threshold)
logger <- lgr::get_logger("mlr3")
logger$set_threshold(previous_threshold)
