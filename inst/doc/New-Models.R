## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  comment = "#>"
)

## ----echo = FALSE, message = FALSE--------------------------------------------
library(gofreg)

## -----------------------------------------------------------------------------
CustomModel <- R6::R6Class(
  classname = "CustomModel",
  inherit = ParamRegrModel,
  public = list(
    
    f_yx = function(t, x, params = private$params) {
      if (checkmate::test_atomic_vector(params)) {
        # reshape plain numeric vector into list with appropriate tags
        xcol <- ncol(as.matrix(x))
        checkmate::assert_atomic_vector(params, len = 1 + 2 * xcol)
        params <- list(a = params[1], 
                       b = params[2:(1+xcol)], 
                       c = params[(2+xcol):(1+2*xcol)])
      } else {
        private$check_params(params, x)
      }
      dnorm(t, mean = self$mean_yx(x, params), 
               sd = as.matrix(x)^2 %*% params$c)
    },
    
    F_yx = function(t, x, params = private$params) {
      if (checkmate::test_atomic_vector(params)) {
        # reshape plain numeric vector into list with appropriate tags
        xcol <- ncol(as.matrix(x))
        checkmate::assert_atomic_vector(params, len = 1 + 2 * xcol)
        params <- list(a = params[1], 
                       b = params[2:(1+xcol)], 
                       c = params[(2+xcol):(1+2*xcol)])
      } else {
        private$check_params(params, x)
      }
      pnorm(t, mean = self$mean_yx(x, params), 
               sd = as.matrix(x)^2 %*% params$c)
    },
    
    F1_yx = function(t, x, params = private$params) {
      private$check_params(params, x)
      qnorm(t, mean = self$mean_yx(x, params), 
               sd = as.matrix(x)^2 %*% params$c)
    },
    
    sample_yx = function(x, params = private$params) {
      private$check_params(params, x)
      rnorm(nrow(as.matrix(x)), mean = self$mean_yx(x, params), 
                                sd = as.matrix(x)^2 %*% params$c)
    },
    
    mean_yx = function(x, params = private$params) {
      private$check_params(params, x)
      params$a + exp(as.matrix(x) %*% params$b)
    },
    
    fit = function(data, params_init = private$params, loglik = loglik_xy, inplace = FALSE) {
      checkmate::assert_names(names(data), must.include = c("x"))
      private$check_params(params_init, data$x)
      params_opt <- super$fit(data, params_init = unlist(params_init, use.names = FALSE), 
                                    loglik = loglik)
      xcol <- ncol(as.matrix(x))
      params_opt <-list(a = params_opt[1], 
                        b = params_opt[2:(1+xcol)], 
                        c = params_opt[(2+xcol):(1+2*xcol)])
      if (inplace) {
        private$params <- params_opt
        invisible(self)
      } else {
        params_opt
      }
    }
  ),
  
  private = list(
    check_params = function(params, x) {
      checkmate::assert_list(params, len = 3)
      checkmate::assert_names(names(params), identical.to = c("a", "b", "c"))
      checkmate::assert_vector(params$b, len = ncol(as.matrix(x)))
      checkmate::assert_vector(params$c, len = ncol(as.matrix(x)))
    }
  )
)

## -----------------------------------------------------------------------------
set.seed(123)
n  <- 100
x <- cbind(rnorm(n), runif(n))
model <- CustomModel$new()
params_true <- list(a = 0.8, b = c(0.5, 0.7), c = c(0.1, 0.2))
y <- model$sample_yx(x, params_true)
data <- dplyr::tibble(x = x, y = y)
head(data)

## -----------------------------------------------------------------------------
model$fit(data, params_init = list(a = 1, b = c(1,1), c = c(1,1)), inplace = TRUE)
model$get_params()

## -----------------------------------------------------------------------------
gt <- GOFTest$new(data = data, model_fitted = model, test_stat = CondKolmY$new(), nboot = 100)
gt$get_pvalue()

