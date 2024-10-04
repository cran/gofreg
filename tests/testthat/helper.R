#' Create dummy GLM data and model (with 1-dim x)
#'
#' @param params_true true model parameters (list)
#' @param distr distribution family used for the model
#'
#' @return `data.frame()` with columns `data` and `model`
dummy_xymodel_x1 <- function(params_true, distr, n = 1000) {
  g1 <- function(u) {
    exp(u)
  }

  x <- as.matrix(runif(n))

  model <- GLM.new(distr, g1)
  y <- model$sample_yx(x, params_true)
  list(data = dplyr::tibble(x = x, y = y), model = model)
}

#' Create dummy GLM data and model (with 3-dim x)
#'
#' @param params_true true model parameters (list)
#' @param distr distribution family used for the model
#'
#' @return `data.frame()` with columns `data` and `model`
dummy_xymodel_x3 <- function(params_true, distr, n = 1000) {
  g1 <- function(u) {
    exp(u)
  }

  x <- cbind(runif(n), runif(n), rnorm(n))

  model <- GLM.new(distr, g1)
  y <- model$sample_yx(x, params_true)
  list(data = dplyr::tibble(x = x, y = y), model = model)
}

#' Create dummy GLM data and model (with 3-dim x) and fit it to the data
#'
#' @return `data.frame()` with columns `data` and `model`
dummy_xymodel_fitted <- function() {
  params_true <- list(beta = c(1, 2, 3), sd = 2)
  distr <- "normal"

  dummy <- dummy_xymodel_x3(params_true, distr, n = 100)
  dummy$model$fit(dummy$data, params_init = params_true, inplace = TRUE)
  dummy
}

#' Create dummy censored GLM data and model (with 1-dim x)
#'
#' @param params_true true model parameters (list)
#' @param distr distribution family used for the model
#'
#' @return `data.frame()` with columns `data` and `model`
dummy_xzdmodel_x1 <- function(params_true, distr, n = 1000) {
  g1 <- function(u) {
    exp(u)
  }

  x <- as.matrix(runif(n))

  model <- GLM.new(distr, g1)
  y <- model$sample_yx(x, params_true)

  c <- rnorm(n, mean(y) * 1.2, sd(y) * 0.5)
  z <- pmin(y, c)
  delta <- as.numeric(y <= c)

  list(data = dplyr::tibble(x = x, z = z, delta = delta), model = model)
}

#' Create dummy censored GLM data and model (with 3-dim x)
#'
#' @param params_true true model parameters (list)
#' @param distr distribution family used for the model
#'
#' @return `data.frame()` with columns `data` and `model`
dummy_xzdmodel_x3 <- function(params_true, distr, n = 1000) {
  g1 <- function(u) {
    exp(u)
  }

  x <- cbind(runif(n), runif(n), rnorm(n))

  model <- GLM.new(distr, g1)
  y <- model$sample_yx(x, params_true)

  c <- rnorm(n, mean(y) * 1.2, sd(y) * 0.5)
  z <- pmin(y, c)
  delta <- as.numeric(y <= c)

  list(data = dplyr::tibble(x = x, z = z, delta = delta), model = model)
}

#' Create dummy censored GLM data and model (with 3-dim x) and fit it to the
#' data
#'
#' @return `data.frame()` with columns `data` and `model`
dummy_xzdmodel_fitted <- function() {
  params_true <- list(beta = c(1, 2, 3), sd = 2)
  distr <- "normal"

  dummy <- dummy_xzdmodel_x3(params_true, distr, n = 100)
  dummy$model$fit(dummy$data, params_init = params_true, inplace = TRUE, loglik = loglik_xzd)
  dummy
}
