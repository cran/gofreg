test_that("resample works without resampling x", {
  set.seed(123)

  dummy <- dummy_xymodel_fitted()
  data <- dummy$data
  model <- dummy$model

  set.seed(123)
  data.rs <- resample_param(data, model)
  expect_equal(data.rs$x, data$x)
  set.seed(123)
  expect_equal(data.rs$y, model$sample_yx(data$x))
})

test_that("resample works with resampling x", {
  set.seed(123)

  dummy <- dummy_xymodel_fitted()
  data <- dummy$data
  model <- dummy$model

  set.seed(123)
  data.rs <- resample_param_rsmplx(data, model)
  set.seed(123)
  x <- as.matrix(data[, "x"])
  expect_equal(data.rs$x, x[sample(nrow(x), size = nrow(x), replace = T), ])
  expect_equal(data.rs$y, model$sample_yx(data.rs$x))
})

test_that("resample under censoring works without resampling x", {
  set.seed(123)

  dummy <- dummy_xzdmodel_fitted()
  data <- dummy$data
  model <- dummy$model

  set.seed(123)
  data.rs <- resample_param_cens(data, model)

  # compute reference values
  set.seed(123)
  y <- model$sample_yx(data$x)
  c <- rkm(km_features(data$z, 1 - data$delta), n = length(y))

  expect_equal(data.rs$x, data$x)
  expect_equal(data.rs$z, pmin(y, c))
  expect_equal(data.rs$delta, as.numeric(y <= c))
})
