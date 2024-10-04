test_that("calc_stat works", {
  set.seed(123)

  dummy <- dummy_xzdmodel_fitted()

  test_stat <- CondKolmY_RCM$new()
  test_stat$calc_stat(dummy$data, dummy$model)
  expect_equal(test_stat$get_value(), 0.72501392)
})

test_that("calc_stat does not work for unfitted model", {
  set.seed(123)

  dummy <- dummy_xzdmodel_fitted()
  dummy$model$set_params(NA)

  test_stat <- CondKolmY_RCM$new()
  expect_error(test_stat$calc_stat(dummy$data, dummy$model))
})

test_that("calc_stat does not work for wrong type of data", {
  set.seed(123)

  dummy <- dummy_xzdmodel_fitted()
  dummy$data <- dplyr::select(dummy$data, -z)

  test_stat <- CondKolmY_RCM$new()
  expect_error(test_stat$calc_stat(dummy$data, dummy$model))
})
