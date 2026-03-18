#' Convenience function to ensure an argument is not null
#'
#' @param a An argument to check if null
#' @param b The argument to return if a is null
#'
#' @return Either a if it is not null, or b if it is
#' @noRd
`%||%` <- function(a, b){
  if (is.null(a)) b else a
}

#' Initiate a mdsDisplay object.
#'
#' This will keep track of all the traces and layout attributes of the secondary biplot.
#'
#' @return A list containing trace data and layout configurations compatible with plotly
#' @noRd
mdsDisplay_new <- function() {
  list(
    trace_data = list(),
    layout = list(annotations = list()),
    config = list()
  )
}

#' Add traces to a mdsDisplay object
#'
#' @param mdsDisplay The mdsDisplay object to be updated
#' @param traces The traces to be appended to the mdsDisplay
#'
#' @return An updated mdsDisplay object
#' @noRd
mdsDisplay_add_traces <- function(mdsDisplay, traces) {
  mdsDisplay$trace_data <- c(mdsDisplay$trace_data, traces)
  mdsDisplay
}

#' Add layout attributes to a mdsDisplay object
#'
#' @param mdsDisplay The mdsDisplay object to be updated
#' @param layout The layout attributes to be appended to the mdsDisplay
#'
#' @return An updated mdsDisplay object
#' @noRd
mdsDisplay_add_layout <- function(mdsDisplay, layout) {
  mdsDisplay$layout <- mdsDisplay$layout %||% list()
  for (nm in names(layout)) {
    if (nm == "annotations") {
      mdsDisplay$layout$annotations <- c(mdsDisplay$layout$annotations %||% list(),
                                      layout$annotations %||% list())
    } else if (is.list(layout[[nm]]) && is.list(mdsDisplay$layout[[nm]])) {
      mdsDisplay$layout[[nm]] <- utils::modifyList(mdsDisplay$layout[[nm]], layout[[nm]])
    } else {
      mdsDisplay$layout[[nm]] <- layout[[nm]]
    }
  }
  mdsDisplay
}

#' Add configuration attributes to a mdsDisplay object
#'
#' @param mdsDisplay The mdsDisplay object to be updated
#' @param config The config attributes to be appended to the mdsDisplay
#'
#' @return An updated mdsDisplay object
#' @noRd
mdsDisplay_add_config <- function(mdsDisplay, config) {
  mdsDisplay$config <- utils::modifyList(mdsDisplay$config %||% list(), config)
  mdsDisplay
}
