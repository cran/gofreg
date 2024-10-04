##' @title Conditional Kolmogorov test statistic for the joint distribution of
##'   (X,Y)
##' @description This class inherits from [TestStatistic] and implements a
##'   function to calculate the test statistic (and x-y-values that can be used
##'   to plot the underlying process).
##'
##'   The process underlying the test statistic is given in Andrews (1997)
##'   \doi{10.2307/2171880} and defined by \deqn{\nu_n(x,y) = \frac{1}{\sqrt{n}}
##'   \sum_{i=1}^n \left(I_{\{Y_i \le y\}} - F(y|\hat{\vartheta}_n, X_i) \right)
##'   I_{\{X_i \le x\}},\quad (x,y) \in R^{p+1}.}{(see formula given in paper).}
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
##' ts <- CondKolmXY$new()
##' ts$calc_stat(data, model)
##' print(ts)
##' plot(ts)
##'
##' # Fit a wrong model
##' model2 <- NormalGLM$new(linkinv = function(u) {u+10})
##' model2$fit(data, params_init=list(beta=c(1,1), sd=3), inplace = TRUE)
##'
##' # Print value of test statistic and plot corresponding process
##' ts2 <- CondKolmXY$new()
##' ts2$calc_stat(data, model2)
##' print(ts2)
##' plot(ts2)
CondKolmXY <- R6::R6Class(
  classname = "CondKolmXY",
  inherit = TestStatistic,
  public = list(
    #' @description Calculate the value of the test statistic for given data
    #'   and a model to test for.
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

      # compute sum_{i=1}^n (1{Yi<=Yj} - F(Yj|theta,Xi)) 1{Xi<=Xj} for each j
      n <- length(data$y)
      x <- as.matrix(data$x)
      proc <- 1 / sqrt(n) * sapply(seq(1, n), function(j) {
        sum(((data$y <= data$y[j]) - model$F_yx(data$y[j], x)) *
          (apply(x, 1, function(r) {all(r <= x[j,])})))
      })

      # set private fields accordingly
      private$value <- max(abs(proc))
      private$plot.x <- seq(1, n)
      private$plot.y <- proc
      invisible(self)
    }
  )
)
