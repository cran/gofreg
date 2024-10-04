## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  comment = "#>",
  fig.width = 6
)

## ----setup, echo = FALSE, message = FALSE-------------------------------------
set.seed(123)
library(gofreg)
library(ggplot2)

## -----------------------------------------------------------------------------
set.seed(123)
n  <- 100
x <- cbind(rnorm(n, mean = 3), runif(n, min = 1, max = 10))
model_true <- GLM.new(distr = "normal", linkinv = identity)
params_true <- list(beta = c(2, 6), sd = 1)
y <- model_true$sample_yx(x, params_true)
data <- dplyr::tibble(x = x, y = y)

## -----------------------------------------------------------------------------
model_test <- GLM.new(distr = "normal", linkinv = identity)
model_test$fit(data, params_init = list(beta = c(1,1), sd = 5), inplace = TRUE)
print(model_test$get_params())

## -----------------------------------------------------------------------------
gt <- GOFTest$new(data = data, model_fitted = model_test, 
                  test_stat = CondKolmY$new(), nboot = 100)
print(gt$get_pvalue())

## -----------------------------------------------------------------------------
model_test <- GLM.new(distr = "normal", linkinv = identity)
data_miss <- dplyr::tibble(x = data$x[,1], y = data$y)
model_test$fit(data_miss, params_init = list(beta = c(2), sd = 2), 
               inplace = TRUE)
print(model_test$get_params())

## -----------------------------------------------------------------------------
gt2 <- GOFTest$new(data = data_miss, model_fitted = model_test, 
                  test_stat = CondKolmY$new(), nboot = 100)
print(gt2$get_pvalue())

## -----------------------------------------------------------------------------
gt2$plot_procs()

## -----------------------------------------------------------------------------
gt$plot_procs()

## ----echo = FALSE, message = FALSE--------------------------------------------
set.seed(123)

## -----------------------------------------------------------------------------
n <- 100
x <- cbind(runif(n), rbinom(n, 1, 0.5))
model <- NormalGLM$new()
y <- model$sample_yx(x, params = list(beta = c(2, 3), sd = 1))
c <- rnorm(n, mean(y) * 1.2, sd(y) * 0.5)
data <- dplyr::tibble(x = x, z = pmin(y, c), delta = as.numeric(y <= c))

model$fit(data, params_init = list(beta = c(1, 1), sd = 3), inplace = TRUE, 
          loglik = loglik_xzd)
print(model$get_params())

## -----------------------------------------------------------------------------
gt <- GOFTest$new(
  data = data, model_fitted = model, test_stat = CondKolmY_RCM$new(), 
  nboot = 100, resample = resample_param_cens, loglik = loglik_xzd
)
print(gt$get_pvalue())

