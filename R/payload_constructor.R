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

#' Initiate a payload object.
#'
#' This will keep track of all the traces and layout attributes of the secondary biplot.
#'
#' @return A list containing trace data and layout configurations compatible with plotly
#' @noRd
payload_new <- function() {
  list(
    trace_data = list(),
    layout = list(annotations = list()),
    config = list()
  )
}

#' Add traces to a payload object
#'
#' @param payload The payload object to be updated
#' @param traces The traces to be appended to the payload
#'
#' @return An updated payload object
#' @noRd
payload_add_traces <- function(payload, traces) {
  payload$trace_data <- c(payload$trace_data, traces)
  payload
}

#' Add layout attributes to a payload object
#'
#' @param payload The payload object to be updated
#' @param layout The layout attributes to be appended to the payload
#'
#' @return An updated payload object
#' @noRd
payload_add_layout <- function(payload, layout) {
  payload$layout <- payload$layout %||% list()
  for (nm in names(layout)) {
    if (nm == "annotations") {
      payload$layout$annotations <- c(payload$layout$annotations %||% list(),
                                      layout$annotations %||% list())
    } else if (is.list(layout[[nm]]) && is.list(payload$layout[[nm]])) {
      payload$layout[[nm]] <- utils::modifyList(payload$layout[[nm]], layout[[nm]])
    } else {
      payload$layout[[nm]] <- layout[[nm]]
    }
  }
  payload
}

#' Add configuration attributes to a payload object
#'
#' @param payload The payload object to be updated
#' @param config The config attributes to be appended to the payload
#'
#' @return An updated payload object
#' @noRd
payload_add_config <- function(payload, config) {
  payload$config <- utils::modifyList(payload$config %||% list(), config)
  payload
}
