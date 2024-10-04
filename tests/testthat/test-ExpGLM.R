test_that("f_yx, F_yx and F1_yx work", {
  distr <- "exp"
  params <- list(beta = c(1, 2, 3))
  new.params <- list(beta = c(2, 3, 4))
  t <- c(0, 0.2) # values at which f_yx and F_yx shall be evaluated
  p <- 0.5 # value at which F1_yx shall be evaluated

  # true values of f_yx and F_yx given model parameters
  true_vals <- function(t, p, x, g1, params) {
    rate <- 1 / g1(x %*% params$beta)
    dens <- dexp(t, rate)
    dist <- pexp(t, rate)
    quan <- qexp(p, rate)
    list(dens = dens, dist = dist, quan = quan)
  }

  test_glm_fF1_yx(distr, params, new.params, t, p, true_vals)
})

test_that("sample_yx works", {
  distr <- "exp"
  params <- list(beta = c(1, 2, 3))
  new.params <- list(beta = c(2, 3, 4))

  # expected sample for given model parameters
  expected_sample <- function(x, g1, params) {
    rate <- 1 / g1(x %*% params$beta)
    rexp(nrow(x), rate)
  }

  test_glm_sample_yx(distr, params, new.params, expected_sample)
})

test_that("fit works for univariate covariates", {
  distr <- "exp"
  params_true <- list(beta = 3)
  params_error <- list(beta = -10)
  tol <- 0.1

  test_glm_fit(distr, params_true, params_error, tol, multi = FALSE)
})

test_that("fit works for multidimensional covariates", {
  distr <- "exp"
  params_true <- list(beta = c(1, 2, 3))
  params_error <- list(beta = c(0, 0, 0))
  tol <- 0.1

  test_glm_fit(distr, params_true, params_error, tol, multi = TRUE)
})

test_that("fit works with censoring for univariate covariates", {
  distr <- "exp"
  params_true <- list(beta = 3)
  params_error <- list(beta = -10)
  tol <- 0.1

  test_glm_fit(distr, params_true, params_error, tol, multi = FALSE, cens = TRUE)
})

test_that("fit works with censoring for multidimensional covariates", {
  distr <- "exp"
  params_true <- list(beta = c(1, 2, 3))
  params_error <- list(beta = c(0, 0, 0))
  tol <- 0.1

  test_glm_fit(distr, params_true, params_error, tol, multi = TRUE, cens = TRUE)
})

test_that("default linkinv in constructor works", {
  distr <- "exp"
  params_true <- list(beta = c(1, 2, 3))
  tol <- 0.1

  # create model and data
  set.seed(123)
  n <- 1000
  x <- cbind(runif(n), runif(n), rbinom(n, 1, 0.5))
  model <- GLM.new(distr)
  y <- model$sample_yx(x, params_true)
  data <- dplyr::tibble(x = x, y = y)

  # estimated parameters are close to true values
  params_est <- model$fit(data, params_init = params_true)
  expect_params_range(params_est, params_true, tol)
})

test_that("params in constructor works", {
  distr <- "exp"

  model1 <- GLM.new(distr)
  checkmate::expect_scalar_na(model1$get_params())

  params <- list(beta = c(1, 2, 3))
  model2 <- GLM.new(distr, params = params)
  expect_equal(model2$get_params(), params)
})
