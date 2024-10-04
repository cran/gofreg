##' @title Conditional Kolmogorov test statistic for the marginal distribution
##'   of Y
##' @description This class inherits from [TestStatistic] and implements a
##'   function to calculate the test statistic (and x-y-values that can be used
##'   to plot the underlying process).
##'
##'   The process underlying the test statistic is given in Kremling & Dikta
##'   (2024) \url{https://arxiv.org/abs/2409.20262} and defined by \deqn{\tilde{\alpha}_n(t) = \frac{1}{\sqrt{n}}
##'   \sum_{i=1}^n \left( I_{\{Y_i \le t\}} - F(t|\hat{\vartheta}_n, X_i)
##'   \right), \quad -\infty \le t \le \infty.}{(see formula given in paper).}
##'
##' @importFrom R6 R6Class
##'
##' @export
##'
##' @examples
##' # Create an example dataset
##' n <- 100
##' x <- cbind(runif(n), rbinom(n, 1, 0.5))
##' model <- NormalGLM$new()
##' y <- model$sample_yx(x, params=list(beta=c(2,3), sd=1))
##' data <- dplyr::tibble(x = x, y = y)
##'
##' # Fit the correct model
##' model$fit(data, params_init=list(beta=c(1,1), sd=3), inplace = TRUE)
##'
##' # Print value of test statistic and plot corresponding process
##' ts <- CondKolmY$new()
##' ts$calc_stat(data, model)
##' print(ts)
##' plot(ts)
##'
##' # Fit a wrong model
##' model2 <- NormalGLM$new(linkinv = function(u) {u+10})
##' model2$fit(data, params_init=list(beta=c(1,1), sd=3), inplace = TRUE)
##'
##' # Print value of test statistic and plot corresponding process
##' ts2 <- CondKolmY$new()
##' ts2$calc_stat(data, model2)
##' print(ts2)
##' plot(ts2)
CondKolmY <- R6::R6Class(
  classname = "CondKolmY",
  inherit = TestStatistic,
  public = list(
    #' @description Calculate the value of the test statistic for given data
    #'  and a model to test for.
    #'
    #' @param data `data.frame()` with columns x and y containing the data
    #' @param model [ParamRegrModel] to test for, already fitted to the data
    #'
    #' @return The modified object (`self`), allowing for method chaining.
    #'
    #' @export
    calc_stat = function(data, model) {
      # check for correct shape of data and definedness of model params
      checkmate::assert_data_frame(data)
      checkmate::assert_names(names(data), must.include = c("x", "y"))
      checkmate::assert_class(model, "ParamRegrModel")
      if (anyNA(model$get_params())) {
        stop("Model first needs to be fitted to the data.")
      }

      # compute ECDF of Y (non-parametric estimator for distribution of Y)
      Fyn <- ecdf(data$y)

      # determine jump points and value of the ECDF at these points
      t.vals <- knots(Fyn)
      Fyn.vals <- Fyn(t.vals)

      # determine semi-parametric estimator for distribution of Y evaluated at the same (jump) points
      n <- length(data$y)
      Fypar.vals <- sapply(t.vals, function(t) {
        sum(model$F_yx(t, data$x)) / n
      })

      # determine KS statistic by computing the difference at the jump points
      proc <- sqrt(n) * (Fyn.vals - Fypar.vals)

      # compute sup(abs(proc)): compare parametric fit to the upper and lower
      # step function value of KM estimator
      diff_upper <- Fyn.vals - Fypar.vals
      diff_lower <- c(0, Fyn.vals[-length(Fyn.vals)]) - Fypar.vals
      proc.sup <- sqrt(n) * max(abs(c(diff_upper, diff_lower)))

      # plot(Fyn, do.points=FALSE)
      # lines(t.vals, Fypar.vals, col="blue")

      # set private fields accordingly
      private$value <- proc.sup
      private$plot.x <- t.vals
      private$plot.y <- proc
      invisible(self)
    }
  )
)
