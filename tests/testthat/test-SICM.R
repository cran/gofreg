test_that("calc_stat works", {
  set.seed(123)

  dummy <- dummy_xymodel_fitted()

  test_stat <- SICM$new(c = 5)
  test_stat$calc_stat(dummy$data, dummy$model)
  expect_equal(test_stat$get_value(), 0.000002631423620)
})

test_that("initialization does not work without value of c", {
  set.seed(123)

  expect_error(SICM$new())
})

test_that("calc_stat does not work for unfitted model", {
  set.seed(123)

  dummy <- dummy_xymodel_fitted()
  dummy$model$set_params(NA)

  test_stat <- MEP$new()
  expect_error(test_stat$calc_stat(dummy$data, dummy$model))
})

test_that("calc_stat does not work for wrong type of data", {
  set.seed(123)

  dummy <- dummy_xymodel_fitted()
  dummy$data <- dplyr::select(dummy$data, -y)

  test_stat <- MEP$new()
  expect_error(test_stat$calc_stat(dummy$data, dummy$model))
})
