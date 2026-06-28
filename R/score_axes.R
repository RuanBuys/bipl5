#' Annotate a biplot with Alves direct-reading errors
#'
#' `score_axes()` augments the hover information of every `mdsDisplay` in a
#' `bipl5_biplot` with the direct-reading diagnostic of Alves (2012). For each
#' observation \eqn{i} and variable \eqn{j} the direct-reading error is
#'
#' \deqn{\delta_{ij} = \frac{|x_{ij} - \widehat{x}_{ij}|}{s_j},}
#'
#' where \eqn{x_{ij}} is the actual value, \eqn{\widehat{x}_{ij}} is the value
#' read directly off the calibrated axis for variable \eqn{j} (i.e. the
#' orthogonal projection of the sample point onto the displayed axis), and
#' \eqn{s_j} is the scaling constant used when the biplot was drawn. The scaling
#' constant is \code{1} when the data were not scaled and the column standard
#' deviation \eqn{s_j} when \code{scale = TRUE} was passed to [init_biplot()].
#' The quantity is reported in percentage form (\eqn{100\,\delta_{ij}}) as an
#' extra `Error` column in the hover table shown when a data point is hovered
#' over.
#'
#' The diagnostic is computed separately for each `mdsDisplay` present in the
#' object, because the read-off value \eqn{\widehat{x}_{ij}} depends on the
#' dimension pair being displayed. The methodology applies uniformly to PCA,
#' PCO and CVA biplots, which all use calibrated linear axes. Spline (non-linear)
#' PCO axes do not admit a single calibrated reading and are left unchanged.
#'
#' This step is deliberately *not* performed by default: it is only applied when
#' `score_axes()` is inserted into the pipeline, e.g.
#'
#' \preformatted{init_biplot(data) |> scale_mds() |> score_axes() |> plot()}
#'
#' @param x A `bipl5_biplot` produced by [scale_mds()].
#' @param digits Number of decimal places used when displaying the reading
#'   error percentage. Defaults to `2`.
#' @param ... Currently unused.
#'
#' @return A `bipl5_biplot` whose observation hover tables carry an additional
#'   `Error` column. The object remains fully plottable.
#'
#' @references
#' Alves, M. R. (2012). Evaluation of the predictive power of biplot axes to
#' automate the construction and layout of biplots based on the accuracy of
#' direct readings from common outputs of multivariate analyses: application to
#' principal component analysis. *Journal of Chemometrics*, 26(5), 180-190.
#'
#' @export
score_axes <- function(x, digits = 2, ...) {
  UseMethod("score_axes")
}

#' @rdname score_axes
#' @export
score_axes.bipl5_biplot <- function(x, digits = 2, ...) {
  if (isTRUE(x$meta$spline)) {
    warning(
      "score_axes() does not support spline axes; ",
      "direct-reading errors are only defined for calibrated linear axes. ",
      "Returning the biplot unchanged.",
      call. = FALSE
    )
    return(x)
  }

  ez <- x$meta$x
  if (is.null(ez)) {
    stop("score_axes() requires a bipl5_biplot created by scale_mds().", call. = FALSE)
  }

  raw_X <- as.matrix(ez$X)
  p <- ncol(raw_X)

  # Scaling constant s_j: 1 when unscaled, the column sd when scale = TRUE.
  s <- rep(1, p)
  if (isTRUE(ez$scaled) && !is.null(ez$sd)) {
    s <- as.numeric(ez$sd)
  }
  s_mat <- matrix(s, nrow = nrow(raw_X), ncol = p, byrow = TRUE)

  group <- x$meta$group
  sample_pred <- ez$sample.predictivity %||% ez$within.class.sample.predictivity

  display_names <- names(x$meta$pc_info)
  for (nm in display_names) {
    mds <- x[[nm]]
    if (is.null(mds)) {
      next
    }

    Z <- mds$Data$sample_coordinates
    z.axes <- mds$Data$axes_coordinates
    if (is.null(Z) || is.null(z.axes)) {
      next
    }

    # Direct read-off values for this dimension pair, in raw data units.
    pred <- direct_reading_values(Z, raw_X, z.axes)

    reading_error <- 100 * abs(raw_X - pred) / s_mat
    dimnames(reading_error) <- dimnames(raw_X)

    obj <- list(
      Z = Z,
      group = group,
      n = ez$n,
      x = raw_X,
      XHat = pred,
      sample.predictivity = sample_pred,
      reading_error = reading_error,
      reading_error_digits = digits
    )

    x[[nm]] <- rewrite_data_hovertext(mds, obj)
  }

  x$meta$reading_errors <- TRUE
  x
}

#' @exportS3Method
score_axes.default <- function(x, digits = 2, ...) {
  stop(
    "score_axes() expects a bipl5_biplot created by scale_mds().",
    call. = FALSE
  )
}

#' Compute direct read-off values from calibrated linear axes
#'
#' For each variable, projects every sample point onto the displayed calibrated
#' axis and reads the corresponding tick value. Reuses [obtain_xhat()]'s
#' calibrated-axis interpolation so the predicted values exactly match the
#' "Pred" column already shown in the hover table.
#'
#' @param Z Numeric \eqn{n \times 2} matrix of sample coordinates.
#' @param X Data matrix supplying variable names.
#' @param z.axes List of per-variable calibrated axis coordinate matrices.
#'
#' @return Numeric \eqn{n \times p} matrix of read-off values in raw units.
#' @noRd
direct_reading_values <- function(Z, X, z.axes) {
  fake <- list(
    p = ncol(X),
    n = nrow(Z),
    Z = as.matrix(Z),
    X = as.matrix(X)
  )
  class(fake) <- "regress"
  obtain_xhat(fake, z.axes = z.axes)
}

#' Rewrite the observation hover tables of a mdsDisplay
#'
#' Regenerates the hovertext of every data (observation) trace in a
#' `bipl5_mdsDisplay`, using `obj` (which carries the optional
#' `reading_error` matrix) so the Alves error column is added.
#'
#' @param mds A `bipl5_mdsDisplay` object.
#' @param obj A list shaped like the object consumed by
#'   [hovertext_generator()], including a `reading_error` matrix.
#'
#' @return The updated `bipl5_mdsDisplay`.
#' @noRd
rewrite_data_hovertext <- function(mds, obj) {
  group_levels <- levels(obj$group)
  hover_by_level <- lapply(
    seq_along(group_levels),
    function(i) hovertext_generator(obj, i, "<br />")
  )
  names(hover_by_level) <- group_levels

  traces <- mds$mdsDisplay$trace_data
  for (k in seq_along(traces)) {
    tr <- traces[[k]]
    if (!trace_is_data(tr)) {
      next
    }
    lvl <- tr$name
    if (!is.null(lvl) && !is.null(hover_by_level[[lvl]])) {
      traces[[k]]$hovertext <- hover_by_level[[lvl]]
    }
  }
  mds$mdsDisplay$trace_data <- traces
  mds
}

#' @noRd
trace_is_data <- function(tr) {
  meta <- tr$meta
  if (is.null(meta)) {
    return(FALSE)
  }
  "data" %in% unlist(meta)
}
