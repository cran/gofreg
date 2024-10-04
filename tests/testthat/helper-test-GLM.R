expect_params_range <- function(params_est, params_true, tol) {
  params_est_unlist <- unlist(params_est, use.names = FALSE)
  params_true_unlist <- unlist(params_true, use.names = FALSE)
  q <- length(params_est_unlist)
  for (i in 1:q) {
    expect_lt(params_est_unlist[i], params_true_unlist[i] + tol)
    expect_gt(params_est_unlist[i], params_true_unlist[i] - tol)
  }
}

test_glm_fF1_yx <- function(distr, params, new.params, t, p, true_vals) {
  g1 <- function(u) {
    1 / u
  }
  x <- rbind(c(1, 2, 3), c(4, 5, 6))

  # create model
  model <- GLM.new(distr = distr, linkinv = g1)

  # no parameters specified (nor saved yet)
  expect_error(model$f_yx(t, x))
  expect_error(model$F_yx(t, x))
  expect_error(model$F1_yx(p, x))

  # wrong shape of parameters
  expect_error(model$f_yx(t, x, params = list(useless_param = 3)))
  expect_error(model$F_yx(t, x, params = list(useless_param = 3)))
  expect_error(model$F1_yx(p, x, params = list(useless_param = 3)))

  # use saved parameters
  model$set_params(params)

  # print(model$f_yx(t, x))

  tv <- true_vals(t, p, x, g1, params)

  expect_equal(model$f_yx(t, x), tv$dens)
  expect_equal(model$F_yx(t, x), tv$dist)
  expect_equal(model$F1_yx(p, x), tv$quan)

  tv <- true_vals(t, p, x, g1, new.params)

  # use new parameters if specified
  expect_equal(model$f_yx(t, x, new.params), tv$dens)
  expect_equal(model$F_yx(t, x, new.params), tv$dist)
  expect_equal(model$F1_yx(p, x, new.params), tv$quan)
}

test_glm_sample_yx <- function(distr, params, new.params, expected_sample) {
  g1 <- function(u) {
    1 / u
  }
  x <- rbind(c(1, 2, 3), c(4, 5, 6))

  # create model
  model <- GLM.new(distr, g1)

  # no parameters specified (nor saved yet)
  expect_error(model$sample_yx(x))

  # wrong shape of parameters
  expect_error(model$sample_yx(x, params = list(useless_param = 3)))

  # use saved parameters
  model$set_params(params)
  set.seed(123)
  s1 <- model$sample_yx(x)
  # print(s1)
  set.seed(123)
  s2 <- expected_sample(x, g1, params)
  expect_equal(s1, s2)

  # use new parameters if specified
  set.seed(123)
  s1 <- model$sample_yx(x, new.params)
  set.seed(123)
  s2 <- expected_sample(x, g1, new.params)
  expect_equal(s1, s2)
}

test_glm_fit <- function(distr, params_true, params_error, tol, multi, cens = FALSE) {
  set.seed(123)

  dummy_model_func <- "dummy_"

  # with or without censoring
  if (cens) {
    dummy_model_func <- paste0(dummy_model_func, "xzdmodel_")
    loglik <- loglik_xzd
  } else {
    dummy_model_func <- paste0(dummy_model_func, "xymodel_")
    loglik <- loglik_xy
  }

  # univariate or multidimensional covariates x
  if (multi) {
    dummy_model_func <- paste0(dummy_model_func, "x3")
  } else {
    dummy_model_func <- paste0(dummy_model_func, "x1")
  }

  # create data and model
  dummy <- do.call(dummy_model_func, args = list(params_true = params_true, distr = distr))
  data <- dummy$data
  model <- dummy$model

  # no initial parameter values
  expect_error(model$fit(data, loglik = loglik))

  # wrong shape of initial parameters
  expect_error(model$fit(data, params_init = list(useless_param = 3), loglik = loglik))

  # print( model$f_yx(data$z[data$delta == 1], as.matrix(data[data$delta == 1, "x"]), params_error) )
  # print( 1 - model$F_yx(data$z[data$delta == 0], as.matrix(data[data$delta == 0, "x"]), params_error) )

  # non-feasible initial parameter values
  expect_error(model$fit(data, params_init = params_error, loglik = loglik))

  # estimated parameters are close to true values
  params_est <- model$fit(data, params_init = params_true, loglik = loglik)

  expect_params_range(params_est, params_true, tol)

  # by default, fit does not set model parameters
  expect(is.na(model$get_params()), "Model parameters should not be defined yet.")

  # model parameters are set if inplace=TRUE
  model$fit(data, params_init = params_true, inplace = TRUE, loglik = loglik)
  expect_equal(model$get_params(), params_est)
}
