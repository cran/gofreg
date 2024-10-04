##' @title Goodness-of-fit test for parametric regression
##' @description This class implements functions to calculate the test statistic
##'   for the original data as well as the statistics for bootstrap samples. It
##'   also offers the possibility to compute the corresponding bootstrap
##'   p-value.
##'
##' @importFrom R6 R6Class
##' @importFrom ggplot2 ggplot waiver ggtitle xlab ylab
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
##' # Calculate the bootstrap p-value and plot the corresponding processes
##' goftest <- GOFTest$new(data, model, test_stat = CondKolmY$new(), nboot = 10)
##' goftest$get_pvalue()
##' goftest$plot_procs()
##'
##' # Fit a wrong model
##' model2 <- NormalGLM$new(linkinv = function(u) {u+10})
##' model2$fit(data, params_init=list(beta=c(1,1), sd=3), inplace = TRUE)
##'
##' # Calculate the bootstrap p-value and plot the corresponding processes
##' goftest2 <- GOFTest$new(data, model2, test_stat = CondKolmY$new(), nboot = 10)
##' goftest2$get_pvalue()
##' goftest2$plot_procs()
GOFTest <- R6::R6Class(
  classname = "GOFTest",
  public = list(
    #' @description Initialize an instance of class [GOFTest].
    #'
    #' @param data `data.frame()` containing the data
    #' @param model_fitted object of class [ParamRegrModel] with fitted
    #'   parameters
    #' @param test_stat object of class [TestStatistic]
    #' @param nboot number of bootstrap iterations
    #' @param resample `function(data, model)` used to resample data in
    #'   bootstrap iterations, defaults to [resample_param()]
    #' @param loglik `function(data, model, params)` negative log-likelihood
    #'   function used to fit model to resampled data in bootstrap iterations,
    #'   defaults to [loglik_xy()]
    #'
    #' @return a new instance of the class
    #'
    #' @export
    initialize = function(data, model_fitted, test_stat, nboot, resample = resample_param, loglik = loglik_xy) {
      checkmate::assert_class(model_fitted, "ParamRegrModel")
      checkmate::assert_class(test_stat, "TestStatistic")
      checkmate::assert_function(resample, nargs = 2, args = c("data", "model"), ordered = TRUE)
      checkmate::assert_function(loglik, nargs = 3, args = c("data", "model", "params"), ordered = TRUE)
      private$data <- data
      private$model <- model_fitted
      private$test_stat <- test_stat
      private$nboot <- nboot
      private$resample <- resample
      private$loglik <- loglik
    },

    #' @description Calculates the test statistic for the original data and
    #'   model.
    #'
    #' @return object of class [TestStatistic]
    #' @export
    get_stat_orig = function() {
      if (!checkmate::test_class(private$stat_orig, "TestStatistic") && anyNA(private$stat_orig)) {
        private$stat_orig <- private$test_stat$clone(deep = TRUE)
        private$stat_orig$calc_stat(private$data, private$model)
      }
      private$stat_orig
    },

    #' @description Calculates the test statistics for the resampled data and
    #'   corresponding models.
    #'
    #' @return vector of length `nboot` containing objects of class
    #'   [TestStatistic]
    #' @export
    get_stats_boot = function() {
      if (!checkmate::test_class(private$stats_boot, "TestStatistic") && anyNA(private$stats_boot)) {
        private$stats_boot <- replicate(private$nboot, private$bootstrap())
      }
      private$stats_boot
    },

    #' @description Calculates the bootstrap p-value for the given model.
    #'
    #' @return p-value for the null hypothesis that `y` was generated according
    #'   to `model`
    #' @export
    get_pvalue = function() {
      stat_orig <- self$get_stat_orig()
      stats_boot <- self$get_stats_boot()

      get_val <- function(stat) {
        stat$get_value()
      }

      val_orig <- stat_orig$get_value()
      val_boot <- sapply(stats_boot, get_val)
      mean(val_orig < val_boot)
    },

    #' @description Plots the processes underlying the bootstrap test statistics
    #'   (gray) and the original test statistic (red)
    #'
    #' @param title text to be displayed as title of the plot; defaults to "Test
    #'   statistic: xxx, p-value: xxx"
    #' @param subtitle text to be displayed as subtitle of the plot; default is
    #'   no subtitle
    #' @param color_boot color used to plot bootstrap test statistics; default
    #'   is "red"
    #' @param color_orig color used to plot original test statistic; default is
    #'   "gray40"
    #' @param x_lab label to use for the x-axis; default is "plot.x"
    #' @param y_lab label to use for the y-axis; default is "plot.y"
    #'
    #' @return The object (`self`), allowing for method chaining.
    #'
    #' @export
    plot_procs = function(title = sprintf("Test Statistic: %s, p-value: %s",
                                          class(private$test_stat)[1],
                                          self$get_pvalue()),
                          subtitle = ggplot2::waiver(),
                          color_boot = "gray40", color_orig = "red",
                          x_lab = "plot.x", y_lab = "plot.y") {
      gpl <- ggplot2::ggplot() +
        ggplot2::ggtitle(title, subtitle) +
        ggplot2::xlab(x_lab) + ggplot2::ylab(y_lab)
      for (ts in private$stats_boot) {
        gpl <- gpl + ts$geom_ts_proc(col = color_boot)
      }
      gpl <- gpl + private$stat_orig$geom_ts_proc(col = color_orig)
      print(gpl)
      invisible(self)
    }
  ),
  private = list(
    data = NA,
    model = NA,
    test_stat = NA,
    nboot = NA,
    resample = NA,
    loglik = NA,
    stat_orig = NA,
    stats_boot = NA,

    # Implements a single bootstrap iteration
    #
    # @description Generates a bootstrap sample according to the resampling
    #   scheme, fits the model and computes the corresponding test statistic.
    #
    # @returns object of class [TestStatistic]
    bootstrap = function() {
      # create bootstrap data
      data.b <- private$resample(private$data, private$model)

      # find MLE and test statistic for bootstrap data
      model.b <- private$model$clone(deep = TRUE)
      model.b$fit(data.b, loglik = private$loglik, inplace = TRUE)

      stat.b <- private$test_stat$clone(deep = TRUE)
      stat.b$calc_stat(data.b, model.b)

      # return requested statistic
      stat.b
    }
  )
)
