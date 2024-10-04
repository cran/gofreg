##' @title Marked empirical process test statistic for a given GLM
##' @description This class inherits from [TestStatistic] and implements a
##'   function to calculate the test statistic (and x-y-values that can be used
##'   to plot the underlying process).
##'
##'   The process underlying the test statistic is given in Dikta & Scheer
##'   (2021) \doi{10.1007/978-3-030-73480-0} and defined by \deqn{\bar{R}^1_n(u)
##'   = \frac{1}{\sqrt{n}} \sum_{i=1}^n \left( Y_i - m(X_i, \hat{\beta}_n)
##'   \right) I_{\{\hat{\beta}_n X_i \le u\}}, \quad -\infty \le u \le \infty.}{
##'   (see formula given in paper).}
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
##' ts <- MEP$new()
##' ts$calc_stat(data, model)
##' print(ts)
##' plot(ts)
##'
##' # Fit a wrong model
##' model2 <- NormalGLM$new(linkinv = function(u) {u+10})
##' model2$fit(data, params_init=list(beta=c(1,1), sd=3), inplace = TRUE)
##'
##' # Print value of test statistic and plot corresponding process
##' ts2 <- MEP$new()
##' ts2$calc_stat(data, model2)
##' print(ts2)
##' plot(ts2)
MEP <- R6::R6Class(
  classname = "MEP",
  inherit = TestStatistic,
  public = list(
    #' @description Calculate the value of the test statistic for given data
    #'   and a model to test for.
    #'
    #' @param data `data.frame()` with columns x and y containing the data
    #' @param model [ParamRegrModel] to test for
    #'
    #' @return The modified object (`self`), allowing for method chaining.
    #'
    #' @export
    calc_stat = function(data, model) {
      # check for correct shape of data and definedness of model params
      checkmate::assert_data_frame(data)
      checkmate::assert_names(names(data), must.include = c("x", "y"))
      checkmate::assert_class(model, "ParamRegrModel")
      params <- model$get_params()
      if (anyNA(params)) {
        stop("Model first needs to be fitted to the data.")
      }

      # check for beta in params since MEP can only be evaluated for GLMs
      checkmate::assert_names(names(params), must.include = c("beta"))
      beta <- params$beta
      checkmate::assert_vector(beta, len = ncol(as.matrix(data$x)))

      # compute linear combination beta^T*X and residuals
      beta.x <- as.matrix(data$x) %*% beta
      res <- data$y - model$mean_yx(data$x)

      # order residuals by beta^T*X, compute scaled cumsum (Rn1)
      n <- length(data$y)
      ord.id <- order(beta.x)
      res.ord <- res[ord.id]
      Rn1 <- cumsum(res.ord) / sqrt(n)

      # determine KS statistic by computing the supremum of |Rn1|
      private$value <- max(abs(Rn1)) #mean(Rn1^2)
      private$plot.x <- beta.x[ord.id]
      private$plot.y <- Rn1
      invisible(self)
    }
  )
)
