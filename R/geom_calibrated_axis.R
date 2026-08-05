# ─────────────────────────────────────────────────────────────────────────────
# GeomCalibratedAxis: calibrated (Gower) biplot axes for ggplot2
#
# The rendering strategy follows gggda::GeomAxis (the engine behind
# ordr::geom_cols_axis): the axis is drawn as a line spanning the whole panel,
# markers are short segments orthogonal to it, marker values are printed rotated
# to the axis angle and dodged to one side, and the variable name is written at
# the panel border on the increasing end of the axis.
#
# The calibration itself is *not* recomputed here.  gggda derives marker values
# from the `center` and `scale` aesthetics using Wilkinson's extended breaks
# algorithm; bipl5 instead consumes the exact calibrated coordinates produced by
# biplotEZ::axes_coordinates(), recovering the underlying affine map so that
# markers can still be laid out against whatever plotting window ggplot2
# eventually chooses.
#
# Every axis aesthetic is deliberately given a non-standard name
# (`axis_colour`, `tick_colour`, `text_size`, ...).  Mapping `colour` or
# `linewidth` directly would create ggplot2 scales and legends for the axes and
# would collide with the sample-point scales, so the axis aesthetics are passed
# through unscaled and resolved inside draw_panel().
# ─────────────────────────────────────────────────────────────────────────────

#' Calibrated biplot axes for ggplot2
#'
#' Draws calibrated (Gower) biplot axes: a line spanning the plotting window,
#' orthogonal tick marks at calibrated marker positions, marker values rotated
#' to the angle of the axis, and the variable name written at the window border.
#'
#' The layer is designed to consume the calibrated axis coordinates returned by
#' \code{biplotEZ::axes_coordinates()}, which \code{\link{wrap_bipl5_gg}} maps
#' onto the aesthetics documented below.  Because the marker positions are
#' supplied rather than derived, the layer honours whatever calibration
#' \pkg{biplotEZ} produced, including axes translated away from the origin and
#' the curved spline axes used by principal coordinate analysis biplots.
#'
#' @section Aesthetics:
#' \code{geom_calibrated_axis()} understands the following aesthetics
#' (required aesthetics are in bold):
#' \itemize{
#'   \item \strong{\code{x}}, \strong{\code{y}} — calibrated marker positions
#'     in display space.  These do \emph{not} contribute to the plotting window,
#'     so axes never inflate the panel range.
#'   \item \code{label} — the marker value printed next to each tick.
#'   \item \code{axis_label} — the variable name written at the window border.
#'   \item \code{angle} — direction of increasing variable value, in radians.
#'   \item \code{tick} — logical flag marking which rows carry a tick mark;
#'     used by curved axes, where most rows are only curve vertices.
#'   \item \code{value} — the variable value at each marker, used to identify
#'     the increasing end of a curved axis.
#'   \item \code{cal_x0}, \code{cal_y0}, \code{cal_dxdv}, \code{cal_dydv},
#'     \code{cal_vref}, \code{cal_vstep} — the affine calibration recovered from
#'     \code{biplotEZ::axes_coordinates()}, used when \code{tick_extend = TRUE}.
#'   \item \code{off_x}, \code{off_y} — the foot of the perpendicular from the
#'     origin to a translated axis.
#'   \item \code{axis_colour}, \code{axis_linewidth}, \code{axis_linetype},
#'     \code{axis_alpha}
#'   \item \code{tick_colour}, \code{tick_linewidth}
#'   \item \code{text_colour}, \code{text_size}
#'   \item \code{label_colour}, \code{label_size}
#'   \item \code{family}, \code{fontface}, \code{group}
#' }
#' Element aesthetics left at \code{NA} inherit from the axis line, mirroring
#' the \code{sync()} fallbacks of \pkg{gggda}.
#'
#' @param mapping Set of aesthetic mappings created by
#'   \code{\link[ggplot2]{aes}}.
#' @param data The data to be displayed in this layer.
#' @param stat The statistical transformation to use on the data.
#' @param position Position adjustment.
#' @param axis_type Either \code{"line"} for straight axes or \code{"curve"}
#'   for the polyline (spline) axes of a principal coordinate analysis biplot.
#' @param axis_line,axis_labels,axis_ticks,axis_text Logical; whether to draw
#'   the axis line, the variable name, the tick marks, and the marker values.
#' @param tick_extend Logical; if \code{TRUE} (the default) markers are
#'   regenerated from the recovered calibration so that they cover the whole
#'   visible axis, preserving the marker spacing and phase chosen by
#'   \pkg{biplotEZ}.  If \code{FALSE} only the supplied markers are drawn.
#'   Ignored when \code{axis_type = "curve"}.
#' @param tick_length Length of the tick marks, as a proportion of the smaller
#'   of the panel width and height.
#' @param text_dodge Orthogonal distance of the marker values from the axis, as
#'   a proportion of the smaller of the panel width and height.
#' @param label_dodge Orthogonal distance of the variable name from the axis, as
#'   a proportion of the smaller of the panel width and height.
#' @param label_placement One of \code{"positive"} (the default; the increasing
#'   end of the axis), \code{"negative"}, or \code{"peripheral"} (the end
#'   farther from the origin).
#' @param parse,check_overlap Passed to the underlying text grobs.
#' @param na.rm Logical; silently remove missing values.
#' @param show.legend,inherit.aes Standard \pkg{ggplot2} layer arguments.
#' @param ... Additional arguments passed to \code{\link[ggplot2]{layer}}.
#'
#' @return A \pkg{ggplot2} layer.
#'
#' @seealso \code{\link{wrap_bipl5_gg}}, which assembles a complete biplot
#'   around this layer.
#'
#' @references
#' Gower, J. C. and Hand, D. J. (1996). \emph{Biplots}. London: Chapman and Hall.
#'
#' Gower, J. C., Lubbe, S. and le Roux, N. J. (2011).
#' \emph{Understanding Biplots}. Chichester: Wiley.
#'
#' Brunson, J. C. (2025). \emph{gggda: A 'ggplot2' Extension for Geometric Data
#' Analysis}. R package version 0.2.0.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(biplotEZ)
#'
#' bp <- biplot(iris[, 1:4], scale = TRUE) |>
#'   PCA(group.aes = iris[, 5]) |>
#'   axes()
#'
#' axis_df <- bipl5:::calibrated_axis_frame(bp)$data
#'
#' ggplot() +
#'   geom_point(
#'     data = data.frame(x = bp$Z[, 1], y = bp$Z[, 2]),
#'     mapping = aes(x = x, y = y)
#'   ) +
#'   geom_calibrated_axis(
#'     data = axis_df,
#'     mapping = aes(
#'       x = x, y = y, group = group, angle = angle,
#'       label = label, axis_label = axis_label
#'     )
#'   ) +
#'   coord_equal()
#' }
#'
#' @export
geom_calibrated_axis <- function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  axis_type = c("line", "curve"),
  axis_line = TRUE,
  axis_labels = TRUE,
  axis_ticks = TRUE,
  axis_text = TRUE,
  tick_extend = TRUE,
  tick_length = 0.025,
  text_dodge = 0.03,
  label_dodge = 0.03,
  label_placement = c("positive", "negative", "peripheral"),
  parse = FALSE,
  check_overlap = FALSE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  ...
) {
  axis_type <- match.arg(axis_type)
  label_placement <- match.arg(label_placement)

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomCalibratedAxis,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      axis_type = axis_type,
      axis_line = axis_line,
      axis_labels = axis_labels,
      axis_ticks = axis_ticks,
      axis_text = axis_text,
      tick_extend = tick_extend,
      tick_length = tick_length,
      text_dodge = text_dodge,
      label_dodge = label_dodge,
      label_placement = label_placement,
      parse = parse,
      check_overlap = check_overlap,
      na.rm = na.rm,
      ...
    )
  )
}


#' @rdname geom_calibrated_axis
#' @format NULL
#' @usage NULL
#' @export
GeomCalibratedAxis <- ggplot2::ggproto(
  "GeomCalibratedAxis",
  ggplot2::Geom,

  required_aes = c("x", "y"),

  default_aes = ggplot2::aes(
    # axis line
    axis_colour = "black",
    axis_linewidth = 0.25,
    axis_linetype = "solid",
    axis_alpha = NA,
    # tick marks
    tick_colour = NA,
    tick_linewidth = NA,
    # marker values
    text_colour = NA,
    text_size = 2.6,
    # variable name
    label_colour = NA,
    label_size = 3.88,
    # shared text aesthetics
    family = "",
    fontface = 1L,
    # geometry, normally supplied by wrap_bipl5_gg()
    angle = 0,
    label = "",
    axis_label = "",
    tick = TRUE,
    value = NA_real_,
    cal_x0 = NA_real_,
    cal_y0 = NA_real_,
    cal_dxdv = NA_real_,
    cal_dydv = NA_real_,
    cal_vref = NA_real_,
    cal_vstep = NA_real_,
    off_x = 0,
    off_y = 0
  ),

  # Marker coordinates lie along the axis and routinely fall far outside the
  # cloud of sample points.  Renaming them here hides them from the round of
  # scale training that follows setup_data(), so calibrated axes never inflate
  # the panel range -- exactly the behaviour of gggda::GeomAxis.
  setup_data = function(data, params) {
    data$x_t <- data$x
    data$y_t <- data$y
    data$x <- NULL
    data$y <- NULL
    data
  },

  draw_panel = function(
    data,
    panel_params,
    coord,
    axis_type = "line",
    axis_line = TRUE,
    axis_labels = TRUE,
    axis_ticks = TRUE,
    axis_text = TRUE,
    tick_extend = TRUE,
    tick_length = 0.025,
    text_dodge = 0.03,
    label_dodge = 0.03,
    label_placement = "positive",
    parse = FALSE,
    check_overlap = FALSE,
    na.rm = FALSE
  ) {
    if (nrow(data) == 0L) {
      return(ggplot2::zeroGrob())
    }

    ranges <- gg_panel_ranges(panel_params, coord)
    plot_whmin <- min(diff(ranges$x), diff(ranges$y))

    data <- gg_fill_axis_defaults(data)
    axes <- split(data, data$group)
    axes <- axes[vapply(axes, nrow, integer(1)) > 0L]

    grobs <- list()

    # ── axis lines ─────────────────────────────────────────────────────────
    if (axis_line) {
      if (identical(axis_type, "curve")) {
        path_data <- gg_line_aes(data)
        path_data$x <- data$x_t
        path_data$y <- data$y_t
        grobs <- c(grobs, list(ggplot2::GeomPath$draw_panel(
          path_data,
          panel_params = panel_params,
          coord = coord
        )))
      } else {
        line_data <- gg_axis_line_data(axes)
        vertical <- line_data$.vertical
        if (any(!vertical)) {
          grobs <- c(grobs, list(ggplot2::GeomAbline$draw_panel(
            line_data[!vertical, , drop = FALSE],
            panel_params = panel_params,
            coord = coord
          )))
        }
        if (any(vertical)) {
          grobs <- c(grobs, list(ggplot2::GeomVline$draw_panel(
            line_data[vertical, , drop = FALSE],
            panel_params = panel_params,
            coord = coord
          )))
        }
      }
    }

    # ── calibrated markers ─────────────────────────────────────────────────
    mark_data <- NULL
    if (axis_ticks || axis_text) {
      mark_data <- gg_axis_mark_data(
        axes,
        ranges = ranges,
        axis_type = axis_type,
        tick_extend = tick_extend
      )
    }

    if (!is.null(mark_data) && nrow(mark_data) > 0L) {
      dodge <- gg_dodge_direction(mark_data)

      if (axis_ticks) {
        rtick <- plot_whmin * tick_length / 2
        tick_data <- gg_line_aes(mark_data)
        tick_data$colour <- mark_data$tick_colour
        tick_data$linewidth <- mark_data$tick_linewidth
        tick_data$linetype <- "solid"
        tick_data$x <- mark_data$x_t + dodge$x * rtick
        tick_data$y <- mark_data$y_t + dodge$y * rtick
        tick_data$xend <- mark_data$x_t - dodge$x * rtick
        tick_data$yend <- mark_data$y_t - dodge$y * rtick

        grobs <- c(grobs, list(ggplot2::GeomSegment$draw_panel(
          tick_data,
          panel_params = panel_params,
          coord = coord
        )))
      }

      if (axis_text) {
        keep <- !is.na(mark_data$label) &
          nzchar(as.character(mark_data$label))
        if (any(keep)) {
          marks <- mark_data[keep, , drop = FALSE]
          text_data <- gg_text_aes(
            marks,
            colour = marks$text_colour,
            size = marks$text_size,
            angle = gg_fold_angle(marks$angle) * 180 / pi,
            label = as.character(marks$label)
          )
          text_data$x <- marks$x_t -
            dodge$x[keep] * plot_whmin * text_dodge
          text_data$y <- marks$y_t -
            dodge$y[keep] * plot_whmin * text_dodge

          grobs <- c(grobs, list(ggplot2::GeomText$draw_panel(
            text_data,
            panel_params = panel_params,
            coord = coord,
            parse = parse,
            check_overlap = check_overlap,
            na.rm = na.rm
          )))
        }
      }
    }

    # ── variable names ─────────────────────────────────────────────────────
    if (axis_labels) {
      label_data <- gg_axis_label_data(
        axes,
        ranges = ranges,
        axis_type = axis_type,
        label_placement = label_placement,
        label_dodge = label_dodge,
        plot_whmin = plot_whmin
      )

      if (!is.null(label_data) && nrow(label_data) > 0L) {
        grobs <- c(grobs, list(ggplot2::GeomText$draw_panel(
          label_data,
          panel_params = panel_params,
          coord = coord,
          parse = parse,
          na.rm = na.rm
        )))
      }
    }

    gg_axis_grob_tree(grobs)
  },

  draw_key = ggplot2::draw_key_blank
)


# ─────────────────────────────────────────────────────────────────────────────
# draw_panel() helpers
# ─────────────────────────────────────────────────────────────────────────────

#' Combine the axis element grobs into a single named grob tree
#'
#' Kept outside the \code{ggproto} object so that the \pkg{grid} dependency is
#' visible to \code{R CMD check}, which does not descend into \code{ggproto}
#' members.
#'
#' @param grobs List of grobs.
#'
#' @return A grob tree, or an empty grob when there is nothing to draw.
#' @noRd
gg_axis_grob_tree <- function(grobs) {
  if (length(grobs) == 0L) {
    return(ggplot2::zeroGrob())
  }

  grob <- do.call(grid::grobTree, grobs)
  grob$name <- grid::grobName(grob, "geom_calibrated_axis")
  grob
}

#' Resolve element aesthetics against the axis aesthetics
#'
#' Tick marks, marker values and variable names each accept their own colour
#' and width.  When these are left at \code{NA} they inherit from the axis line,
#' mirroring the \code{sync()} fallbacks used by \pkg{gggda}.
#'
#' @param data Layer data inside \code{draw_panel()}.
#'
#' @return \code{data} with the element aesthetics resolved.
#' @noRd
gg_fill_axis_defaults <- function(data) {
  fill_in <- function(value, fallback, n) {
    if (is.null(value)) {
      return(rep_len(fallback, n))
    }
    value <- rep_len(value, n)
    missing <- is.na(value)
    value[missing] <- rep_len(fallback, n)[missing]
    value
  }

  n <- nrow(data)
  if (is.null(data$axis_colour)) data$axis_colour <- "black"
  if (is.null(data$axis_linewidth)) data$axis_linewidth <- 0.25
  if (is.null(data$axis_linetype)) data$axis_linetype <- "solid"
  if (is.null(data$axis_alpha)) data$axis_alpha <- NA

  data$tick_colour <- fill_in(data$tick_colour, data$axis_colour, n)
  data$text_colour <- fill_in(data$text_colour, data$axis_colour, n)
  data$label_colour <- fill_in(data$label_colour, data$axis_colour, n)
  data$tick_linewidth <- fill_in(
    data$tick_linewidth,
    data$axis_linewidth,
    n
  )

  if (is.null(data$angle)) data$angle <- 0
  if (is.null(data$tick)) data$tick <- TRUE
  if (is.null(data$label)) data$label <- ""
  if (is.null(data$axis_label)) data$axis_label <- ""
  if (is.null(data$family)) data$family <- ""
  if (is.null(data$fontface)) data$fontface <- 1L
  for (nm in c(
    "cal_x0", "cal_y0", "cal_dxdv", "cal_dydv", "cal_vref", "cal_vstep"
  )) {
    if (is.null(data[[nm]])) data[[nm]] <- NA_real_
  }
  data$off_x <- fill_in(data$off_x, 0, n)
  data$off_y <- fill_in(data$off_y, 0, n)

  data
}

#' Assemble the aesthetic columns expected by the line-based grobs
#'
#' @param data Layer data inside \code{draw_panel()}.
#'
#' @return A data frame carrying \code{colour}, \code{linewidth},
#'   \code{linetype}, \code{alpha} and \code{group}.
#' @noRd
gg_line_aes <- function(data) {
  data.frame(
    colour = data$axis_colour,
    linewidth = data$axis_linewidth,
    linetype = data$axis_linetype,
    alpha = data$axis_alpha,
    group = data$group,
    stringsAsFactors = FALSE
  )
}

#' Assemble the aesthetic columns expected by the text grobs
#'
#' @param data Layer data inside \code{draw_panel()}.
#' @param colour,size,angle,label Resolved text aesthetics.
#' @param hjust,vjust Text justification.
#'
#' @return A data frame accepted by \code{GeomText$draw_panel()}.
#' @noRd
gg_text_aes <- function(
  data,
  colour,
  size,
  angle,
  label,
  hjust = 0.5,
  vjust = 0.5
) {
  out <- data.frame(
    label = label,
    colour = colour,
    size = size,
    angle = angle,
    alpha = data$axis_alpha,
    family = data$family,
    fontface = data$fontface,
    lineheight = 1.2,
    group = data$group,
    stringsAsFactors = FALSE
  )
  out$hjust <- hjust
  out$vjust <- vjust
  out
}

#' Orthogonal dodge direction for tick marks and marker values
#'
#' Markers are dodged away from the origin on translated axes and towards the
#' upward normal of the axis otherwise, which keeps marker values below the axis
#' as \pkg{biplotEZ} and \pkg{gggda} both do.
#'
#' @param data Marker data inside \code{draw_panel()}.
#'
#' @return A list of numeric vectors \code{x} and \code{y}.
#' @noRd
gg_dodge_direction <- function(data) {
  fold <- gg_fold_angle(data$angle)
  offset_len <- sqrt(data$off_x^2 + data$off_y^2)
  offset_axis <- offset_len > 1e-10

  list(
    x = ifelse(
      offset_axis,
      data$off_x / pmax(offset_len, 1e-10),
      -sin(fold)
    ),
    y = ifelse(
      offset_axis,
      data$off_y / pmax(offset_len, 1e-10),
      cos(fold)
    )
  )
}

#' Build one row of slope/intercept data per straight axis
#'
#' @param axes List of per-axis data frames.
#'
#' @return A data frame accepted by \code{GeomAbline} / \code{GeomVline}, with
#'   an extra logical column \code{.vertical}.
#' @noRd
gg_axis_line_data <- function(axes) {
  rows <- lapply(axes, function(ax) {
    first <- ax[1L, , drop = FALSE]
    angle <- first$angle
    px <- if (isTRUE(is.finite(first$cal_x0))) first$cal_x0 else first$x_t
    py <- if (isTRUE(is.finite(first$cal_y0))) first$cal_y0 else first$y_t

    vertical <- abs(cos(angle)) < 1e-10
    slope <- if (vertical) 0 else tan(angle)

    out <- gg_line_aes(first)
    out$.vertical <- vertical
    out$slope <- slope
    out$intercept <- if (vertical) 0 else py - slope * px
    out$xintercept <- px
    out
  })

  do.call(rbind, rows)
}

#' Locate the calibrated markers that fall inside the plotting window
#'
#' For straight axes with \code{tick_extend = TRUE} the markers are regenerated
#' from the recovered affine calibration, keeping the spacing and phase chosen
#' by \pkg{biplotEZ} but covering the whole visible axis.  Otherwise the
#' supplied markers are filtered to the window.
#'
#' @param axes List of per-axis data frames.
#' @param ranges Panel ranges, as returned by \code{gg_panel_ranges()}.
#' @param axis_type Either \code{"line"} or \code{"curve"}.
#' @param tick_extend Logical; regenerate markers to cover the window.
#'
#' @return A data frame of markers, or \code{NULL}.
#' @noRd
gg_axis_mark_data <- function(axes, ranges, axis_type, tick_extend) {
  rows <- lapply(axes, function(ax) {
    ticks <- ax[isTRUE_vec(ax$tick), , drop = FALSE]

    can_extend <- identical(axis_type, "line") &&
      isTRUE(tick_extend) &&
      nrow(ax) > 0L &&
      isTRUE(is.finite(ax$cal_dxdv[1])) &&
      isTRUE(is.finite(ax$cal_dydv[1])) &&
      isTRUE(is.finite(ax$cal_vstep[1])) &&
      isTRUE(ax$cal_vstep[1] > 0)

    if (can_extend) {
      # Fall back to the supplied markers when the axis misses the panel or
      # would need an unreasonable number of markers to span it; the window
      # filter below then discards whatever is not visible.
      extended <- gg_extend_axis_marks(ax[1L, , drop = FALSE], ranges)
      if (!is.null(extended)) {
        ticks <- extended
      }
    }

    if (is.null(ticks) || nrow(ticks) == 0L) {
      return(NULL)
    }

    inside <- gg_inside_window(ticks$x_t, ticks$y_t, ranges)
    if (!any(inside)) {
      return(NULL)
    }
    ticks[inside, , drop = FALSE]
  })

  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0L) {
    return(NULL)
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Regenerate calibrated markers across the visible axis
#'
#' The recovered calibration maps a variable value \eqn{v} to the display
#' position \eqn{(x_0 + v\,\partial x, y_0 + v\,\partial y)}, so the parameter of
#' the line is the variable value itself.  Markers are emitted at
#' \eqn{v_{ref} + k\,\Delta v}, retaining the spacing and phase that
#' \pkg{biplotEZ} chose.
#'
#' @param proto A one-row data frame carrying the axis calibration and
#'   aesthetics.
#' @param ranges Panel ranges.
#'
#' @return A data frame of markers, or \code{NULL} when the axis misses the
#'   panel.
#' @noRd
gg_extend_axis_marks <- function(proto, ranges) {
  dxdv <- proto$cal_dxdv[1]
  dydv <- proto$cal_dydv[1]
  vstep <- proto$cal_vstep[1]
  vref <- proto$cal_vref[1]
  x0 <- proto$cal_x0[1]
  y0 <- proto$cal_y0[1]

  if (!is.finite(vref)) {
    vref <- 0
  }

  span <- gg_line_window_span(c(x0, y0), c(dxdv, dydv), ranges$x, ranges$y)
  if (is.null(span)) {
    return(NULL)
  }

  kmin <- ceiling((span[1] - vref) / vstep - 1e-9)
  kmax <- floor((span[2] - vref) / vstep + 1e-9)
  if (!is.finite(kmin) || !is.finite(kmax) || kmax < kmin) {
    return(NULL)
  }
  if (kmax - kmin > 1000) {
    return(NULL)
  }

  values <- vref + seq(kmin, kmax) * vstep
  marks <- proto[rep(1L, length(values)), , drop = FALSE]
  marks$x_t <- x0 + values * dxdv
  marks$y_t <- y0 + values * dydv
  marks$label <- gg_format_tick_values(values, vstep)
  rownames(marks) <- NULL
  marks
}

#' Position the variable name at the plotting window border
#'
#' @param axes List of per-axis data frames.
#' @param ranges Panel ranges.
#' @param axis_type Either \code{"line"} or \code{"curve"}.
#' @param label_placement Which end of the axis to label.
#' @param label_dodge Orthogonal dodge, as a proportion of the panel size.
#' @param plot_whmin The smaller of the panel width and height.
#'
#' @return A data frame accepted by \code{GeomText}, or \code{NULL}.
#' @noRd
gg_axis_label_data <- function(
  axes,
  ranges,
  axis_type,
  label_placement,
  label_dodge,
  plot_whmin
) {
  rows <- lapply(axes, function(ax) {
    first <- ax[1L, , drop = FALSE]
    name <- as.character(first$axis_label)
    if (is.na(name) || !nzchar(name)) {
      return(NULL)
    }

    if (identical(axis_type, "curve")) {
      inside <- gg_inside_window(ax$x_t, ax$y_t, ranges)
      visible <- ax[inside, , drop = FALSE]
      if (nrow(visible) == 0L) {
        return(NULL)
      }
      # Curve vertices are stored in curve order, which need not be value
      # order; prefer the mapped value when it is available.
      value <- visible$value
      has_value <- !is.null(value) && any(is.finite(value))
      pick <- switch(
        label_placement,
        negative = if (has_value) which.min(value) else 1L,
        peripheral = which.max(visible$x_t^2 + visible$y_t^2),
        if (has_value) which.max(value) else nrow(visible)
      )
      point <- c(visible$x_t[pick], visible$y_t[pick])
      fold <- gg_fold_angle(visible$angle[pick])
    } else {
      angle <- first$angle
      px <- if (isTRUE(is.finite(first$cal_x0))) first$cal_x0 else first$x_t
      py <- if (isTRUE(is.finite(first$cal_y0))) first$cal_y0 else first$y_t
      u <- c(cos(angle), sin(angle))

      span <- gg_line_window_span(c(px, py), u, ranges$x, ranges$y)
      if (is.null(span)) {
        return(NULL)
      }

      p_pos <- c(px, py) + span[2] * u
      p_neg <- c(px, py) + span[1] * u
      point <- switch(
        label_placement,
        negative = p_neg,
        peripheral = if (sum(p_pos^2) >= sum(p_neg^2)) p_pos else p_neg,
        p_pos
      )
      fold <- gg_fold_angle(angle)
    }

    offset_len <- sqrt(first$off_x^2 + first$off_y^2)
    dodge <- if (offset_len > 1e-10) {
      c(first$off_x, first$off_y) / offset_len
    } else {
      c(-sin(fold), cos(fold))
    }

    out <- gg_text_aes(
      first,
      colour = first$label_colour,
      size = first$label_size,
      angle = fold * 180 / pi,
      label = name,
      hjust = "inward",
      vjust = "inward"
    )
    out$x <- point[1] + dodge[1] * plot_whmin * label_dodge
    out$y <- point[2] + dodge[2] * plot_whmin * label_dodge
    out
  })

  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0L) {
    return(NULL)
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Test whether points lie inside the plotting window
#'
#' @param x,y Numeric coordinate vectors.
#' @param ranges Panel ranges.
#'
#' @return Logical vector.
#' @noRd
gg_inside_window <- function(x, y, ranges) {
  tol <- 1e-9
  is.finite(x) &
    is.finite(y) &
    x >= min(ranges$x) - tol &
    x <= max(ranges$x) + tol &
    y >= min(ranges$y) - tol &
    y <= max(ranges$y) + tol
}

#' Coerce a possibly missing logical vector to \code{TRUE}/\code{FALSE}
#'
#' @param x A vector.
#'
#' @return Logical vector the same length as \code{x}.
#' @noRd
isTRUE_vec <- function(x) {
  if (is.null(x)) {
    return(logical(0))
  }
  out <- as.logical(x)
  out[is.na(out)] <- FALSE
  out
}
