##' @title Parametric regression model (abstract class)
##' @description This is the abstract base class for parametric regression model
##'   objects like [NormalGLM].
##'
##'   Parametric regression models are built around the following key tasks:
##'   * A method `fit()` to fit the model to given data, i.e. compute the MLE
##'     for the model parameters
##'   * Methods `f_yx()`, `F_yx()` and `mean_yx()` to evaluate the conditional
##'     density, distribution and regression function
##'   * A method `sample_yx()` to generate a random sample of response variables
##'     following the model given a vector of covariates
##'
##' @param x vector of covariates
##' @param params model parameters to use, defaults to the fitted parameter
##'   values
##'
##' @importFrom R6 R6Class
##'
##' @export
ParamRegrModel <- R6::R6Class("ParamRegrModel", public = list(
  #' @description Set the value of the model parameters used as default for the
  #'   class functions.
  #'
  #' @param params model parameters to use as default
  #'
  #' @return The modified object (`self`), allowing for method chaining.
  #'
  #' @export
  set_params = function(params) {
    private$params <- params
    invisible(self)
  },

  #' @description Returns the value of the model parameters used as default for
  #'   the class functions.
  #'
  #' @return model parameters used as default
  #'
  #' @export
  get_params = function() {
    private$params
  },

  #' @description Calculates the maximum likelihood estimator for the model
  #'   parameters based on given data.
  #'
  #' @param data list containing the data to fit the model to
  #' @param params_init initial value of the model parameters to use for the
  #'   optimization (defaults to the fitted parameter values)
  #' @param loglik `function(data, model, params)` defaults to [loglik_xy()]
  #'
  #' @return MLE of the model parameters for the given data, same shape as
  #'   `params_init`
  #' @export
  fit = function(data, params_init = private$params, loglik = loglik_xy) {
    if (anyNA(params_init)) {
      stop("Starting value of model parameters needs to be defined for the optimization.")
    }
    checkmate::assert_function(loglik, nargs = 3, args = c("data", "model", "params"), ordered = TRUE)
    lik_init <- loglik(data, model = self, params_init)
    if (lik_init == 1e100) {
      stop("Starting value of model parameters not feasible for the given data.")
    }
    if (length(params_init) == 1) {
      params_opt <- optim(
        par = params_init,
        fn = loglik, data = data, model = self,
        lower = 0, upper = 20, method = "Brent"
      )
    } else {
      params_opt <- optim(
        par = params_init,
        fn = loglik, data = data, model = self,
        method = "Nelder-Mead"
      )
    }
    params_opt$par
  },

  #' @description Evaluates the conditional density function.
  #'
  #' @param t value(s) at which the conditional density shall be evaluated
  #'
  #' @return value(s) of the conditional density function, same shape as `t`
  #' @export
  f_yx = function(t, x, params = private$params) {
    stop("Abstract method. Needs to be implemented.")
  },

  #' @description Evaluates the conditional distribution function.
  #'
  #' @param t value(s) at which the conditional distribution shall be
  #'   evaluated
  #'
  #' @return value(s) of the conditional distribution function, same shape as
  #'   `t`
  #' @export
  F_yx = function(t, x, params = private$params) {
    stop("Abstract method. Needs to be implemented.")
  },

  #' @description Evaluates the conditional quantile function.
  #'
  #' @param t value(s) at which the conditional quantile function shall be
  #'   evaluated
  #'
  #' @return value(s) of the conditional quantile function, same shape as
  #'   `t`
  #' @export
  F1_yx = function(t, x, params = private$params) {
    stop("Abstract method. Needs to be implemented.")
  },

  #' @description Generates a new sample of response variables with the same
  #'   conditional distribution.
  #'
  #' @return vector of sampled response variables, same length as `x`
  #' @export
  sample_yx = function(x, params = private$params) {
    stop("Abstract method. Needs to be implemented.")
  },

  #' @description Evaluates the regression function or in other terms the
  #'   expected value of Y given X=x.
  #'
  #' @return value of the regression function
  #' @export
  mean_yx = function(x, params = private$params) {
    stop("Abstract method. Needs to be implemented.")
  }
), private = list(
  params = NA,

  # @description Check that `params` are not NA, otherwise throw an error.
  check_params = function(params) {
    if (anyNA(params)) {
      stop("Model parameters need to be defined. Use fit(x, y, model) or set_params(params) to set default values.")
    }
  }
))
