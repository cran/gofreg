test_that("get_stat_orig works", {
  set.seed(123)

  dummy <- dummy_xymodel_fitted()
  data <- dummy$data
  model <- dummy$model

  test_stat <- CondKolmY$new()
  goftest <- GOFTest$new(data, model, test_stat, nboot = 10)

  ts1 <- goftest$get_stat_orig()
  expect_error(test_stat$get_value())
  ts2 <- test_stat$calc_stat(data, model)
  expect_equal(ts1, ts2)
})

test_that("get_stat_boot and get_pvalue work", {
  set.seed(123)

  dummy <- dummy_xymodel_fitted()
  data <- dummy$data
  model <- dummy$model

  test_stat <- CondKolmY$new()
  goftest <- GOFTest$new(data, model, test_stat, nboot = 10)
  ts_boot <- goftest$get_stats_boot()

  get_val <- function(ts) {
    ts$get_value()
  }

  ts_boot_vals <- sapply(ts_boot, get_val)
  expect_equal(ts_boot_vals, c(0.52479397, 0.97484393, 0.63576482, 0.64580577, 0.79234386, 0.58805946, 0.58614489, 0.62749552, 0.69325262, 0.32980569))

  pval <- goftest$get_pvalue()
  expect_equal(pval, mean(goftest$get_stat_orig()$get_value() < ts_boot_vals))
  expect_equal(pval, 0.8)
})


test_that("get_pvalue works alone", {
  set.seed(123)

  dummy <- dummy_xymodel_fitted()
  data <- dummy$data
  model <- dummy$model

  test_stat <- CondKolmY$new()
  goftest <- GOFTest$new(data, model, test_stat, nboot = 10)
  pval <- goftest$get_pvalue()
  expect_equal(pval, 0.8)
})

test_that("default resampling scheme works", {
  set.seed(123)

  dummy <- dummy_xymodel_fitted()
  data <- dummy$data
  model <- dummy$model

  test_stat <- CondKolmY$new()
  goftest1 <- GOFTest$new(data, model, test_stat, nboot = 10)
  goftest2 <- GOFTest$new(data, model, test_stat, nboot = 10, resample = resample_param)
  expect_equal(goftest1$get_stats_boot(), goftest2$get_stats_boot())
})

test_that("get_stat_orig works under censoring", {
  set.seed(123)

  dummy <- dummy_xzdmodel_fitted()
  data <- dummy$data
  model <- dummy$model

  test_stat <- CondKolmY_RCM$new()
  goftest <- GOFTest$new(data, model, test_stat, nboot = 10, resample = resample_param_cens, loglik = loglik_xzd)

  ts1 <- goftest$get_stat_orig()
  expect_error(test_stat$get_value())
  ts2 <- test_stat$calc_stat(data, model)
  expect_equal(ts1, ts2)
})

test_that("get_stat_boot and get_pvalue work under censoring", {
  set.seed(123)

  dummy <- dummy_xzdmodel_fitted()
  data <- dummy$data
  model <- dummy$model

  test_stat <- CondKolmY_RCM$new()
  goftest <- GOFTest$new(data, model, test_stat, nboot = 10, resample = resample_param_cens, loglik = loglik_xzd)
  ts_boot <- goftest$get_stats_boot()

  get_val <- function(ts) {
    ts$get_value()
  }

  ts_boot_vals <- sapply(ts_boot, get_val)
  expect_equal(ts_boot_vals, c(1.29386408, 1.03976675, 0.80157753, 1.23348327, 1.11222768, 0.56454107, 1.05206233, 1.53276045, 1.58962918, 0.78449612))

  pval <- goftest$get_pvalue()
  expect_equal(pval, mean(goftest$get_stat_orig()$get_value() < ts_boot_vals))
  expect_equal(pval, 0.9)
})


test_that("get_pvalue works alone under censoring", {
  set.seed(123)

  dummy <- dummy_xzdmodel_fitted()
  data <- dummy$data
  model <- dummy$model

  test_stat <- CondKolmY_RCM$new()
  goftest <- GOFTest$new(data, model, test_stat, nboot = 10, resample = resample_param_cens, loglik = loglik_xzd)
  pval <- goftest$get_pvalue()
  expect_equal(pval, 0.9)
})
