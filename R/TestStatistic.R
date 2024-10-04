##' @title Test Statistic for parametric regression models (abstract class)
##' @description This is the abstract base class for test statistic objects like
##'   [CondKolmY] or [MEP].
##'
##'   Test statistics are built around the key method
##'   `calc_stat()` which calculates the particular test statistic (and
##'   x-y-values that can be used to plot the underlying process).
##'
##' @importFrom R6 R6Class
##' @importFrom ggplot2 ggplot geom_line aes
##'
##' @export
TestStatistic <- R6::R6Class(
  classname = "TestStatistic",
  public = list(
    #' @description Returns the value of the test statistic.
    #'
    #' @return value of the test statistic
    #' @export
    get_value = function() {
      if (is.na(private$value)) {
        stop("Value of the test statistic is not assigned yet. Call calc_stat(data, model) first!")
      }
      private$value
    },

    #' @description Calculate the value of the test statistic for given data
    #'   and a model to test for.
    #'
    #' @param data `list()` containing the data
    #' @param model [ParamRegrModel] to test for
    #'
    #' @return The modified object (`self`), allowing for method chaining.
    #'
    #' @export
    calc_stat = function(data, model) {
      stop("Abstract method. Needs to be implemented.")
    },

    #' @description Returns vectors of x and y that can be used to plot the
    #'   process corresponding to the test statistic.
    #'
    #' @return list with plot.x and plot.y being vectors of the same length
    #' @export
    get_plot_xy = function() {
      if (anyNA(c(private$plot.x, private$plot.y))) {
        stop("The values of plot.x and plot.y are not assigned yet. Call calc_stat(data, model) first!")
      }
      list(plot.x = private$plot.x, plot.y = private$plot.y)
    },

    #' @description Overrides the print-method for objects of type
    #'   `TestStatistic` to only print its value.
    #'
    #' @return The object (`self`), allowing for method chaining.
    #'
    #' @export
    print = function() {
      cat("Test statistic with value", private$value)
      invisible(self)
    },

    #' @description Creates a line plot showing the underlying process of the
    #'   test statistic.
    #'
    #' @param ... Other arguments passed on to [ggplot2::geom_line()]. These are
    #'   often aesthetics, used to set an aesthetic to a fixed value, like
    #'   `colour = "red"` or `size = 3`.
    #'
    #' @return A ggplot2 layer representing a line plot.
    #'
    #' @export
    geom_ts_proc = function(...) {
      ggplot2::geom_line(ggplot2::aes(x = private$plot.x, y = private$plot.y), ...)
    },

    #' @description Creates a new ggplot showing the underlying process of the
    #'   test statistic.
    #'
    #' @param ... Other arguments passed on to [ggplot2::geom_line()]. These are
    #'   often aesthetics, used to set an aesthetic to a fixed value, like
    #'   `colour = "red"` or `size = 3`.
    #'
    #' @return A ggplot2 object representing the complete plot, including a line
    #'   geometry.
    #'
    #' @export
    plot = function(...) {
      ggplot2::ggplot() + self$geom_ts_proc(...)
    }
  ),
  private = list(
    value = NA,
    plot.x = NA,
    plot.y = NA
  )
)
