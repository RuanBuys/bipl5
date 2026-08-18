# ─────────────────────────────────────────────────────────────────────────────
# Geometry helpers shared by the ggplot2 calibrated-axis layer
#
# The functions below mirror the internal calculations that
# \pkg{gggda}/\pkg{ordr} perform inside \code{GeomAxis$draw_panel()} when they
# render calibrated (Gower) axes.  The essential difference is that
# \pkg{gggda} *derives* marker positions from the \code{center}/\code{scale}
# aesthetics using Wilkinson's extended breaks algorithm, whereas \pkg{bipl5}
# already has exact calibrated marker coordinates from
# \code{biplotEZ::axes_coordinates()}.  These helpers therefore recover the
# affine value-to-position calibration implied by those coordinates so that the
# same drawing code can be reused without re-deriving the calibration.
# ─────────────────────────────────────────────────────────────────────────────

#' Fold an angle into the half-open interval (-pi/2, pi/2]
#'
#' Text drawn along a biplot axis should never appear upside down.  Both
#' \pkg{gggda} and \pkg{bipl5} therefore rotate axis labels and tick labels by
#' the \emph{folded} axis angle, i.e. \code{atan(tan(theta))}, which maps a
#' direction to the equivalent undirected slope angle.
#'
#' @param angle Numeric vector of angles in radians.
#'
#' @return Numeric vector of angles in radians lying in \code{(-pi/2, pi/2]}.
#' @noRd
gg_fold_angle <- function(angle) {
  folded <- atan(tan(angle))
  folded[!is.finite(folded)] <- pi / 2
  folded
}

#' Recover the panel ranges in data space
#'
#' \code{draw_panel()} methods need the visible data range to size tick marks
#' and to locate the border at which axis labels are drawn.  Different ggplot2
#' releases expose this through slightly different helpers, so the lookup is
#' centralised here.
#'
#' @param panel_params Panel parameters supplied to a \code{draw_panel()}
#'   method.
#' @param coord The coordinate system supplied to a \code{draw_panel()} method.
#'
#' @return A list with numeric elements \code{x} and \code{y}, each of length
#'   two.
#' @noRd
gg_panel_ranges <- function(panel_params, coord = NULL) {
  ranges <- NULL

  if (!is.null(coord) && is.function(coord$backtransform_range)) {
    ranges <- tryCatch(
      coord$backtransform_range(panel_params),
      error = function(e) NULL
    )
  }

  if (is.null(ranges) || is.null(ranges$x) || is.null(ranges$y)) {
    ranges <- list(x = panel_params$x.range, y = panel_params$y.range)
  }

  ranges
}

#' Recover the affine calibration of one linear biplot axis
#'
#' A calibrated linear biplot axis is the image of the real line under an
#' affine map \eqn{v \mapsto \mathbf{p}_0 + v\mathbf{b}}, where \eqn{v} is the
#' value of the variable and \eqn{\mathbf{b}} is the displacement in display
#' space per unit of \eqn{v}.  \code{biplotEZ::axes_coordinates()} returns a
#' sample of that map: a matrix whose first two columns hold marker positions
#' and whose third column holds the corresponding variable values.  This
#' function inverts that sample back into the underlying map so that markers
#' can be regenerated at draw time for whatever plotting window ggplot2
#' eventually settles on.
#'
#' The regression uses all supplied markers rather than only the endpoints,
#' which keeps the recovered direction stable when marker coordinates carry
#' rounding noise.
#'
#' @param ax A numeric matrix with at least three columns, as returned for a
#'   single variable by \code{biplotEZ::axes_coordinates()}.
#'
#' @return \code{NULL} when the calibration is degenerate (fewer than two
#'   distinct markers, or a zero-length axis), otherwise a list with elements
#'   \describe{
#'     \item{\code{x0}, \code{y0}}{Display position of the value \code{0}.}
#'     \item{\code{dxdv}, \code{dydv}}{Displacement per unit of the variable.}
#'     \item{\code{angle}}{Direction of increasing variable value, in radians.}
#'     \item{\code{vref}}{A marker value supplied by \pkg{biplotEZ}, used to
#'       anchor the phase of any regenerated markers.}
#'     \item{\code{vstep}}{Spacing between successive \pkg{biplotEZ} markers.}
#'     \item{\code{offset}}{Numeric vector of length two giving the foot of the
#'       perpendicular from the origin to the axis; \code{c(0, 0)} for an axis
#'       through the origin.}
#'   }
#' @noRd
gg_axis_calibration <- function(ax) {
  ax <- as.matrix(ax)
  if (nrow(ax) < 2L || ncol(ax) < 3L) {
    return(NULL)
  }

  px <- ax[, 1]
  py <- ax[, 2]
  vv <- ax[, 3]

  keep <- is.finite(px) & is.finite(py) & is.finite(vv)
  px <- px[keep]
  py <- py[keep]
  vv <- vv[keep]
  if (length(vv) < 2L) {
    return(NULL)
  }

  vbar <- mean(vv)
  vdev <- vv - vbar
  denom <- sum(vdev^2)
  if (!is.finite(denom) || denom <= 0) {
    return(NULL)
  }

  dxdv <- sum(vdev * (px - mean(px))) / denom
  dydv <- sum(vdev * (py - mean(py))) / denom
  if (!is.finite(dxdv) || !is.finite(dydv)) {
    return(NULL)
  }
  if (sqrt(dxdv^2 + dydv^2) <= 0) {
    return(NULL)
  }

  x0 <- mean(px) - dxdv * vbar
  y0 <- mean(py) - dydv * vbar

  steps <- diff(sort(unique(vv)))
  vstep <- if (length(steps) > 0) stats::median(steps) else NA_real_
  if (!is.finite(vstep) || vstep <= 0) {
    vstep <- NA_real_
  }

  # Foot of the perpendicular from the origin onto the axis; non-zero only for
  # axes that biplotEZ has translated away from the origin.
  len2 <- dxdv^2 + dydv^2
  proj <- (x0 * dxdv + y0 * dydv) / len2
  offset <- c(x0 - proj * dxdv, y0 - proj * dydv)
  offset[abs(offset) < 1e-10] <- 0

  list(
    x0 = x0,
    y0 = y0,
    dxdv = dxdv,
    dydv = dydv,
    angle = atan2(dydv, dxdv),
    vref = vv[1],
    vstep = vstep,
    offset = offset
  )
}

#' Local direction of increasing value along a curved axis
#'
#' Spline (PCO) axes are polylines rather than straight lines, so each marker
#' carries its own orientation.  Directions are estimated by central
#' differences, with one-sided differences at the ends of the curve.
#'
#' @param px,py Numeric vectors of vertex coordinates along the curve.
#' @param vv Numeric vector of variable values at those vertices.
#'
#' @return Numeric vector of angles in radians, one per vertex.
#' @noRd
gg_curve_angles <- function(px, py, vv) {
  n <- length(px)
  if (n < 2L) {
    return(rep(NA_real_, n))
  }

  lo <- pmax(seq_len(n) - 1L, 1L)
  hi <- pmin(seq_len(n) + 1L, n)

  dx <- px[hi] - px[lo]
  dy <- py[hi] - py[lo]
  dv <- vv[hi] - vv[lo]

  # Orient every tangent in the direction of increasing variable value.
  flip <- is.finite(dv) & dv < 0
  dx[flip] <- -dx[flip]
  dy[flip] <- -dy[flip]

  ang <- atan2(dy, dx)
  ang[!is.finite(dx) | !is.finite(dy) | (dx == 0 & dy == 0)] <- NA_real_
  ang
}

#' Span of a line inside the plotting rectangle
#'
#' Parameterises the axis as \eqn{\mathbf{p}(t) = \mathbf{p}_0 + t\mathbf{u}}
#' and returns the interval of \eqn{t} for which the point lies inside the
#' panel.  This is the bipl5 counterpart of \code{gggda:::delimit_rules()},
#' which projects the window corners onto the axis; solving the interval
#' directly additionally handles axes that do not pass through the origin.
#'
#' @param p0 Numeric vector of length two: a point on the line.
#' @param u Numeric vector of length two: the direction of the line.
#' @param x.range,y.range Numeric vectors of length two giving the panel range.
#'
#' @return \code{NULL} when the line misses the panel, otherwise a numeric
#'   vector \code{c(tmin, tmax)}.
#' @noRd
gg_line_window_span <- function(p0, u, x.range, y.range) {
  tol <- 1e-12
  tmin <- -Inf
  tmax <- Inf

  limits <- list(
    list(p = p0[1], d = u[1], lo = min(x.range), hi = max(x.range)),
    list(p = p0[2], d = u[2], lo = min(y.range), hi = max(y.range))
  )

  for (lim in limits) {
    if (abs(lim$d) < tol) {
      # Line is constant in this coordinate: either always or never inside.
      if (lim$p < lim$lo || lim$p > lim$hi) {
        return(NULL)
      }
      next
    }
    t1 <- (lim$lo - lim$p) / lim$d
    t2 <- (lim$hi - lim$p) / lim$d
    tmin <- max(tmin, min(t1, t2))
    tmax <- min(tmax, max(t1, t2))
  }

  if (!is.finite(tmin) || !is.finite(tmax) || tmax < tmin) {
    return(NULL)
  }

  c(tmin, tmax)
}

#' Format calibrated marker values for printing
#'
#' Markers regenerated from a recovered calibration are floating-point sums and
#' would otherwise print as \code{"0.30000000000000004"}.  The number of
#' decimals is derived from the marker spacing so that neighbouring markers
#' remain distinguishable.
#'
#' @param values Numeric vector of marker values.
#' @param step Numeric scalar giving the spacing between markers.
#'
#' @return Character vector the same length as \code{values}.
#' @noRd
gg_format_tick_values <- function(values, step = NA_real_) {
  if (length(values) == 0L) {
    return(character(0))
  }

  digits <- if (is.finite(step) && step > 0) {
    max(0L, min(10L, ceiling(-log10(step)) + 1L))
  } else {
    3L
  }

  out <- formatC(
    round(values, digits),
    format = "fg",
    digits = 15,
    drop0trailing = TRUE
  )
  trimws(out)
}

#' Resolve a per-axis aesthetic specification
#'
#' Axis aesthetics may be supplied as a single value (recycled), as one value
#' per axis, as \code{NULL} (fall back to \code{default}), or as the string
#' \code{"biplotEZ"} to adopt the aesthetics stored on the biplotEZ object by
#' \code{biplotEZ::axes()}.
#'
#' @param value User-supplied specification.
#' @param ez_value The corresponding vector stored on the biplotEZ object.
#' @param default Value used when \code{value} is \code{NULL}.
#' @param n Number of axes.
#'
#' @return A vector of length \code{n}.
#' @noRd
gg_axis_aes <- function(value, ez_value, default, n) {
  if (is.null(value)) {
    value <- default
  }
  if (is.character(value) && length(value) == 1L && value == "biplotEZ") {
    value <- ez_value
  }
  if (is.null(value) || length(value) == 0L) {
    value <- default
  }
  rep_len(value, n)
}
