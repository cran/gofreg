#' Parametric resampling scheme for a parametric regression model
#'
#' @description Generate a new, resampled dataset of the same shape as data
#'   following the given model. The covariates are kept the same and the
#'   response variables are drawn according to `model$sample_yx()`.
#'
#' @param data `data.frame()` with columns x and y containing the original data
#' @param model [ParamRegrModel] to use for the resampling
#'
#' @returns `data.frame()` with columns x and y containing the resampled data
#'
#' @export
#' @examples
#' # Create an example dataset
#' n <- 10
#' x <- cbind(runif(n), rbinom(n, 1, 0.5))
#' model <- NormalGLM$new()
#' params <- list(beta = c(2, 3), sd = 1)
#' y <- model$sample_yx(x, params = params)
#' data <- dplyr::tibble(x = x, y = y)
#'
#' # Fit the model to the data
#' model$fit(data, params_init = params, inplace = TRUE)
#'
#' # Resample from the model given data
#' resample_param(data, model)
resample_param <- function(data, model) {
  checkmate::assert_data_frame(data)
  checkmate::assert_names(names(data), must.include = c("x", "y"))
  checkmate::assert_class(model, "ParamRegrModel")

  n <- length(data$y)

  # sample new survival times (according to parametric model)
  y.b <- model$sample_yx(data$x)

  dplyr::tibble(x = data$x, y = y.b)
}

#' Parametric resampling scheme for a parametric regression model with
#' resampling of covariates
#'
#' @description Generate a new, resampled dataset of the same shape as data
#'   following the given model. The covariates are resampled from `data$x` and
#'   the response variables are drawn according to `model$sample_yx()`.
#'
#' @param data `data.frame()` with columns x and y containing the original data
#' @param model [ParamRegrModel] to use for the resampling
#'
#' @returns `data.frame()` with columns x and y containing the resampled data
#'
#' @export
#' @examples
#' # Create an example dataset
#' n <- 10
#' x <- cbind(runif(n), rbinom(n, 1, 0.5))
#' model <- NormalGLM$new()
#' params <- list(beta = c(2, 3), sd = 1)
#' y <- model$sample_yx(x, params = params)
#' data <- dplyr::tibble(x = x, y = y)
#'
#' # Fit the model to the data
#' model$fit(data, params_init = params, inplace = TRUE)
#'
#' # Resample from the model given data
#' resample_param(data, model)
resample_param_rsmplx <- function(data, model) {
  checkmate::assert_data_frame(data)
  checkmate::assert_names(names(data), must.include = c("x", "y"))
  checkmate::assert_class(model, "ParamRegrModel")

  n <- length(data$y)

  # resample covariates
  x <- as.matrix(data[, "x"])
  x.b <- x[sample(nrow(x), size = n, replace = T), ]

  # sample new survival times (according to parametric model)
  y.b <- model$sample_yx(x.b)

  dplyr::tibble(x = x.b, y = y.b)
}

#' Parametric resampling scheme for a parametric regression model under random
#' censorship
#'
#' @description Generate a new, resampled dataset of the same shape as data
#'   following the given model. The covariates X are kept the same. Survival
#'   times Y are drawn according to `model$sample_yx()` and censoring times C
#'   according to the KM estimator.
#'
#' @param data `data.frame()` with columns x, z and delta containing the
#'   original data
#' @param model [ParamRegrModel] to use for the resampling
#'
#' @returns `data.frame()` with columns x, z and delta containing the resampled
#'   data
#'
#' @export
#' @examples
#' # Create an example dataset
#' n <- 10
#' x <- cbind(runif(n), rbinom(n, 1, 0.5))
#' model <- NormalGLM$new()
#' params <- list(beta = c(2, 3), sd = 1)
#' y <- model$sample_yx(x, params = params)
#' c <- rnorm(n, mean(y) * 1.2, sd(y) * 0.5)
#' z <- pmin(y, c)
#' delta <- as.numeric(y <= c)
#' data <- dplyr::tibble(x = x, z = z, delta = delta)
#'
#' # Fit the model to the data
#' model$fit(data, params_init = params, inplace = TRUE, loglik = loglik_xzd)
#'
#' # Resample from the model given data
#' resample_param_cens(data, model)
resample_param_cens <- function(data, model) {
  checkmate::assert_data_frame(data)
  checkmate::assert_names(names(data), must.include = c("x", "z", "delta"))
  checkmate::assert_class(model, "ParamRegrModel")

  n <- length(data$z)

  # sample new survival times (according to parametric model)
  y.b <- model$sample_yx(data$x)

  # sample new censoring times (according to KM estimator for C, i.e. for pairs
  # (Z,1-delta))
  c.b <- rkm(km_features(data$z, 1 - data$delta, dist1 = TRUE), n)

  # assign observed times and censoring indicators accordings
  z.b <- pmin(y.b, c.b)
  delta.b <- as.numeric(y.b <= c.b)

  dplyr::tibble(x = data$x, z = z.b, delta = delta.b)
}
