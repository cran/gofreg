## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  comment = "#>",
  fig.width = 4
)

## ----echo = FALSE, message = FALSE--------------------------------------------
library(gofreg)

## -----------------------------------------------------------------------------
MEP_Stute97 <- R6::R6Class(
  classname = "MEP_Stute97",
  inherit = TestStatistic,
  public = list(
    calc_stat = function(data, model) {
      # check for correct shape of data and definedness of model params
      checkmate::assert_data_frame(data)
      checkmate::assert_names(names(data), must.include = c("x", "y"))
      checkmate::assert_matrix(as.matrix(x), ncols = 1)
      checkmate::assert_class(model, "ParamRegrModel")
      params <- model$get_params()
      if (anyNA(params)) {
        stop("Model first needs to be fitted to the data.")
      }
      
      # compute residuals and order them according to X
      res <- data$y - model$mean_yx(data$x)
      ord.id <- order(c(data$x))
      res.ord <- res[ord.id]
      
      # compute MEP (cumulative sum of the ordered residuals)
      proc <- cumsum(res.ord) / sqrt(n)

      # set private fields accordingly
      private$value <- max(abs(proc))
      private$plot.x <- c(data$x)[ord.id]
      private$plot.y <- proc
      invisible(self)
    }
  )
)

## -----------------------------------------------------------------------------
set.seed(123)
n  <- 100
x <- rnorm(n)
model <- NormalGLM$new()
params_true <- list(beta = 3, sd = 0.5)
y <- model$sample_yx(x^2, params_true)
data <- dplyr::tibble(x = x, y = y)
head(data)

## -----------------------------------------------------------------------------
model$fit(data, params_init = list(beta = 1, sd = 5), inplace = TRUE)
model$get_params()
gt <- GOFTest$new(data = data, model_fitted = model, test_stat = MEP_Stute97$new(), nboot = 100)
gt$get_pvalue()

## -----------------------------------------------------------------------------
gt$plot_procs()

## -----------------------------------------------------------------------------
data_x2 <- dplyr::tibble(x = data$x^2, y = data$y)
model$fit(data_x2, params_init = list(beta = 1, sd = 5), inplace = TRUE)
model$get_params()
gt <- GOFTest$new(data = data_x2, model_fitted = model, test_stat = MEP_Stute97$new(), nboot = 100)
gt$get_pvalue()

## -----------------------------------------------------------------------------
gt$plot_procs()

