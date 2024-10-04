##' @title Conditional Kolmogorov test statistic for the marginal distribution
##'   of Y under random censorship
##' @description This class inherits from [TestStatistic] and implements a
##'   function to calculate the test statistic (and x-y-values that can be used
##'   to plot the underlying process).
##'
##'   The process underlying the test statistic is defined by
##'   \deqn{\tilde{\alpha}_n^{KM}(t) = \sqrt{n} \left( \hat{F}^{KM}_n(t) -
##'   \frac{1}{n} \sum_{i=1}^n F(t|\hat{\vartheta}_n, X_i) \right), \quad
##'   -\infty \le t \le \infty.}{(formula cannot be displayed here, see package
##'   website).}
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
##' c <- rnorm(n, mean(y)*1.2, sd(y)*0.5)
##' data <- dplyr::tibble(x = x, z = pmin(y,c), delta = as.numeric(y <= c))
##'
##' # Fit the correct model
##' model$fit(data, params_init=list(beta=c(1,1), sd=3), inplace = TRUE, loglik = loglik_xzd)
##'
##' # Print value of test statistic and plot corresponding process
##' ts <- CondKolmY_RCM$new()
##' ts$calc_stat(data, model)
##' print(ts)
##' plot(ts)
##'
##' # Fit a wrong model
##' model2 <- NormalGLM$new(linkinv = function(u) {u+10})
##' model2$fit(data, params_init=list(beta=c(1,1), sd=3), inplace = TRUE, loglik = loglik_xzd)
##'
##' # Print value of test statistic and plot corresponding process
##' ts2 <- CondKolmY_RCM$new()
##' ts2$calc_stat(data, model2)
##' print(ts2)
##' plot(ts2)
CondKolmY_RCM <- R6::R6Class(
  classname = "CondKolmY_RCM",
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
      checkmate::assert_names(names(data), must.include = c("x", "z", "delta"))
      checkmate::assert_class(model, "ParamRegrModel")
      if (anyNA(model$get_params())) {
        stop("Model first needs to be fitted to the data.")
      }

      # compute KM estimator for Y (non-parametric estimator for distribution of Y)
      km <- km_features(data$z, data$delta)

      # determine jump points and value of the KM estimator at these points
      t.vals <- km$z[km$delta == 1]
      Fkm.vals <- km$pkm[km$delta == 1]

      # optional: exclude last point
      m <- length(t.vals)
      t.vals <- t.vals[-m]
      Fkm.vals <- Fkm.vals[-m]

      # determine semi-parametric estimator for distribution of Y evaluated at the same (jump) points
      n <- length(data$z)
      Fpar.vals <- sapply(t.vals, function(t) {
        sum(model$F_yx(t, data$x)) / n
      })

      # determine test statistic by computing the difference at the jump points
      proc <- sqrt(n) * (Fkm.vals - Fpar.vals)

      # compute sup(abs(proc)): compare parametric fit to the upper and lower
      # step function value of KM estimator
      diff_upper <- Fkm.vals - Fpar.vals
      diff_lower <- c(0, Fkm.vals[-length(Fkm.vals)]) - Fpar.vals
      proc.sup <- sqrt(n) * max(abs(c(diff_upper, diff_lower)))

      # plot(survival::survfit(survival::Surv(data$z, data$delta) ~ 1), fun="F")
      # lines(t.vals, Fpar.vals, col="blue")

      # set private fields accordingly
      private$value <- proc.sup
      private$plot.x <- t.vals
      private$plot.y <- proc
      invisible(self)
    }
  )
)
