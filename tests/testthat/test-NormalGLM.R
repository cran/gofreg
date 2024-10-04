test_that("f_yx, F_yx and F1_yx work", {
  distr <- "normal"
  params <- list(beta = c(1, 2, 3), sd = 2)
  new.params <- list(beta = c(2, 3, 4), sd = 5)
  t <- c(0, 4) # values at which f_yx and F_yx shall be evaluated
  p <- 0.5 # value at which F1_yx shall be evaluated

  # true values of f_yx and F_yx given model parameters
  true_vals <- function(t, p, x, g1, params) {
    mean <- g1(x %*% params$beta)
    dens <- dnorm(t, mean = mean, sd = params$sd)
    dist <- pnorm(t, mean = mean, sd = params$sd)
    quan <- qnorm(p, mean = mean, sd = params$sd)
    list(dens = dens, dist = dist, quan = quan)
  }

  test_glm_fF1_yx(distr, params, new.params, t, p, true_vals)
})

test_that("sample_yx works", {
  distr <- "normal"
  params <- list(beta = c(1, 2, 3), sd = 2)
  new.params <- list(beta = c(2, 3, 4), sd = 5)

  # expected sample for given model parameters
  expected_sample <- function(x, g1, params) {
    mean <- g1(x %*% params$beta)
    rnorm(nrow(x), mean = mean, sd = params$sd)
  }

  test_glm_sample_yx(distr, params, new.params, expected_sample)
})

test_that("fit works for univariate covariates", {
  distr <- "normal"
  params_true <- list(beta = 3, sd = 2)
  params_error <- list(beta = 0, sd = 0)
  tol <- 0.1

  test_glm_fit(distr, params_true, params_error, tol, multi = FALSE)
})

test_that("fit works for multidimensional covariates", {
  distr <- "normal"
  params_true <- list(beta = c(1, 2, 3), sd = 2)
  params_error <- list(beta = c(0, 0, 0), sd = 0)
  tol <- 0.1

  test_glm_fit(distr, params_true, params_error, tol, multi = TRUE)
})

test_that("fit works with censoring for univariate covariates", {
  distr <- "normal"
  params_true <- list(beta = 3, sd = 2)
  params_error <- list(beta = 0, sd = 0)
  tol <- 0.1

  test_glm_fit(distr, params_true, params_error, tol, multi = FALSE, cens = TRUE)
})

test_that("fit works with censoring for multidimensional covariates", {
  distr <- "normal"
  params_true <- list(beta = c(1, 2, 3), sd = 2)
  params_error <- list(beta = c(0, 0, 0), sd = 0)
  tol <- 0.1

  test_glm_fit(distr, params_true, params_error, tol, multi = TRUE, cens = TRUE)
})

test_that("default linkinv in constructor works", {
  distr <- "normal"
  params_true <- list(beta = c(1, 2, 3), sd = 0.5)
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
  distr <- "normal"

  model1 <- GLM.new(distr)
  checkmate::expect_scalar_na(model1$get_params())

  params <- list(beta = c(1, 2, 3), sd = 0.5)
  model2 <- GLM.new(distr, params = params)
  expect_equal(model2$get_params(), params)
})
