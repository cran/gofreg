##' @title Simulated integrated conditional moment test statistic
##' @description This class inherits from [TestStatistic] and implements a
##'   function to calculate the test statistic (and x-y-values that can be used
##'   to plot the underlying process).
##'
##'   The process underlying the test statistic is given in Bierens & Wang
##'   (2012) \doi{10.1017/S0266466611000168} and defined by
##'   \deqn{\hat{T}_n^{(s)}(c) = \frac{1}{(2c)^{p+1}} \int_{[-c,c]^p}
##'   \int_{-c}^c \left|\frac{1}{\sqrt{n}} \sum_{j=1}^n \Big(\exp(i \tau Y_j) -
##'   \exp(i \tau \tilde{Y}_j)\Big) \exp(i \xi^T X_j)\right|^2 d\tau d\xi }{(see
##'   formula given in paper).}
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
##' ts <- SICM$new(c = 5)
##' ts$calc_stat(data, model)
##' print(ts)
##' plot(ts)
##'
##' # Fit a wrong model
##' model2 <- NormalGLM$new(linkinv = function(u) {u+10})
##' model2$fit(data, params_init=list(beta=c(1,1), sd=3), inplace = TRUE)
##'
##' # Print value of test statistic and plot corresponding process
##' ts2 <- SICM$new(c = 5)
##' ts2$calc_stat(data, model2)
##' print(ts2)
##' plot(ts2)
SICM <- R6::R6Class(
  classname = "SICM",
  inherit = TestStatistic,
  public = list(
    #' @description Initialize an instance of class [SICM].
    #'
    #' @param c chosen value for integral boundaries (see Bierens & Wang (2012))
    #' @param transx `function(values)` used to transform x-values to be
    #'   standardized and bounded; default is standardization by subtracting the
    #'   mean and dividing by the standard deviation and then applying arctan
    #' @param transy `function(values, data)` used to transform y-values to be
    #'   standardized and bounded (same method is used for simulated y-values);
    #'   default is standardization by subtracting the mean and dividing by the
    #'   standard deviation and then applying arctan
    #'
    #' @return a new instance of the class
    #'
    #' @export
    initialize = function(c, transx = function(values) {tvals <- atan(scale(values)); tvals[, apply(values,2,sd) == 0] <- 0; return(tvals)},
                          transy = function(values, data) {array(atan(scale(values, center = mean(data$y), scale = sd(data$y))))}) {
      checkmate::assert_function(transx, nargs = 1, args = c("values"))
      checkmate::assert_function(transy, nargs = 2, args = c("values", "data"), ordered = TRUE)
      private$c <- c
      private$transx <- transx
      private$transy <- transy
    },

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

      n <- length(data$y)

      # standardize x- and y-values and apply arctan to ensure their boundedness
      x <- private$transx(data$x)
      y <- private$transy(data$y, data)

      # draw a random sample for \tilde{Y}
      ys.orig <- model$sample_yx(data$x)
      ys <- private$transy(ys.orig, data)

      # compute statistic
      y.diff <- outer(y, y, "-")
      ys.diff <- outer(ys, ys, "-")
      y.ys.diff <- outer(y, ys, "-")

      sin.frac <- function(mat) {
        ifelse(mat != 0, sin(private$c * mat) / (private$c * mat), 1)
      }

      x.diff <- simplify2array(lapply(1:ncol(x), function(col) {outer(x[,col], x[,col], "-")}))
      x.diff.sf <- sin.frac(x.diff)
      x.diff.sf.prod <- x.diff.sf[,,1]
      for(i in 2:dim(x.diff.sf)[3]){
        x.diff.sf.prod <- x.diff.sf.prod*x.diff.sf[,,i]
      }

      Tns <- sum((sin.frac(y.diff) + sin.frac(ys.diff) - sin.frac(y.ys.diff)
                  - sin.frac(t(y.ys.diff))) * x.diff.sf.prod)

      # Tns <- 0
      # for (j1 in 1:(n-1)) {
      #   for (j2 in (j1+1):(n)) {
      #     y.diff <- private$c * c(y[j1] - y[j2], ys[j1] - ys[j2], y[j1] - ys[j2], ys[j1] - y[j2])
      #     y.sign <- c(1,1,-1,-1)
      #     y.diff <- ifelse(y.diff != 0, sin(y.diff)/y.diff, 1)
      #     y.part <- sum(y.sign*y.diff)
      #
      #     x.diff <- private$c * (x[j1,] - x[j2,])
      #     x.diff <- ifelse(x.diff != 0, sin(x.diff)/x.diff, 1)
      #     x.part <- prod(x.diff)
      #
      #     Tns <- Tns + y.part * x.part
      #   }
      #   if (y[j1] - ys[j1] != 0) {
      #     Tns <- Tns + 1 - (sin(private$c * (y[j1] - ys[j1])) / (private$c * (y[j1] - ys[j1])))
      #   }
      # }
      # if (y[n] - ys[n] != 0) {
      #   Tns <- Tns + 1 - (sin(private$c * (y[n] - ys[n])) / (private$c * (y[n] - ys[n])))
      # }
      # Tns <- 2*Tns

      # set class attributes
      # TODO: WHAT TO USE FOR PLOTTING?
      private$value <- Tns/n
      private$plot.x <- c(1)
      private$plot.y <- c(1)
      invisible(self)
    }
  ),
  private = list(
    c = NA,
    transx = NA,
    transy = NA
  )
)
