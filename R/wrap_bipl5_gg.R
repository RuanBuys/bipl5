# ─────────────────────────────────────────────────────────────────────────────
# wrap_bipl5_gg(): render a biplotEZ object as a ggplot2 biplot
#
# This is the ggplot2 counterpart of wrap_bipl5(), which renders the same
# biplotEZ objects through plotly.  The calibrated axes are drawn by
# GeomCalibratedAxis (see R/geom_calibrated_axis.R), which reproduces the
# geometry of gggda::GeomAxis -- the layer behind ordr::geom_cols_axis -- while
# taking its marker coordinates from biplotEZ::axes_coordinates().
# ─────────────────────────────────────────────────────────────────────────────

# ─────────────────────────────────────────────────────────────────────────────
# Data builders
# ─────────────────────────────────────────────────────────────────────────────

#' Build the layer data for calibrated biplot axes
#'
#' Converts the per-variable coordinate matrices returned by
#' \code{biplotEZ::axes_coordinates()} into a single data frame that
#' \code{\link{geom_calibrated_axis}} understands.  For straight axes the
#' underlying affine calibration is recovered so that markers can be laid out
#' against the final plotting window; for the spline axes of a principal
#' coordinate analysis biplot the marker orientation is estimated locally along
#' the curve instead.
#'
#' @param x A \code{biplot} object from \pkg{biplotEZ}.
#' @param z.axes Optional pre-computed axis coordinates.  Defaults to
#'   \code{biplotEZ::axes_coordinates(x)}.
#' @param curved Logical; whether the axes are polylines rather than straight
#'   lines.  Defaults to detecting spline axes on \code{x}.
#' @param axis_colour,tick_colour,text_colour,label_colour Axis element colours.
#'   Each may be a single value, one value per axis, \code{NULL} to inherit from
#'   \code{axis_colour}, or the string \code{"biplotEZ"} to adopt the colours
#'   stored on \code{x} by \code{biplotEZ::axes()}.
#' @param axis_linewidth,axis_linetype Axis line width and type; both accept
#'   \code{"biplotEZ"}.
#' @param text_size,label_size Text sizes, in millimetres, for the marker values
#'   and the variable names.  Both accept \code{"biplotEZ"}, in which case the
#'   \code{cex} values stored on \code{x} are converted to millimetres.
#'
#' @return A list with elements \code{data} (the layer data frame) and
#'   \code{curved} (logical).
#' @noRd
calibrated_axis_frame <- function(
  x,
  z.axes = NULL,
  curved = NULL,
  axis_colour = "black",
  tick_colour = NULL,
  text_colour = NULL,
  label_colour = NULL,
  axis_linewidth = 0.25,
  axis_linetype = "solid",
  text_size = 2.6,
  label_size = 3.88
) {
  if (is.null(x$axes)) {
    x <- biplotEZ::axes(x)
  }
  if (is.null(z.axes)) {
    z.axes <- biplotEZ::axes_coordinates(x)
  }
  if (is.null(curved)) {
    curved <- identical(x$PCOaxes, "splines")
  }

  ax_aes <- x$axes
  which <- ax_aes$which
  if (is.null(which)) {
    which <- seq_along(z.axes)
  }
  n_ax <- length(z.axes)
  if (n_ax == 0L) {
    return(list(data = NULL, curved = curved))
  }

  var_names <- ax_aes$names
  if (is.null(var_names)) {
    var_names <- colnames(x$X)
  }
  labels <- if (length(var_names) >= max(which)) {
    var_names[which]
  } else {
    rep_len(var_names, n_ax)
  }

  # The cex values biplotEZ stores are relative to the base graphics font size;
  # ggplot2 text sizes are in millimetres, so convert on the same footing that
  # ggplot2 itself uses for its default text size.
  ez_text_size <- (ax_aes$tick.label.cex %||% 0.6) * 3.88
  ez_label_size <- (ax_aes$label.cex %||% 0.75) * 3.88

  aes_axis_colour <- gg_axis_aes(axis_colour, ax_aes$col, "black", n_ax)
  aes_tick_colour <- gg_axis_aes(
    tick_colour,
    ax_aes$tick.col,
    aes_axis_colour,
    n_ax
  )
  aes_text_colour <- gg_axis_aes(
    text_colour,
    ax_aes$tick.label.col,
    aes_axis_colour,
    n_ax
  )
  aes_label_colour <- gg_axis_aes(
    label_colour,
    ax_aes$label.col,
    aes_axis_colour,
    n_ax
  )
  aes_linewidth <- gg_axis_aes(
    axis_linewidth,
    (ax_aes$lwd %||% 1) * 0.25,
    0.25,
    n_ax
  )
  aes_linetype <- gg_axis_aes(axis_linetype, ax_aes$lty, "solid", n_ax)
  aes_text_size <- gg_axis_aes(text_size, ez_text_size, 2.6, n_ax)
  aes_label_size <- gg_axis_aes(label_size, ez_label_size, 3.88, n_ax)

  frames <- vector("list", n_ax)

  for (i in seq_len(n_ax)) {
    ax <- z.axes[[i]]
    if (is.null(ax)) {
      next
    }
    ax <- as.matrix(ax)
    if (nrow(ax) < 2L || ncol(ax) < 3L) {
      next
    }

    px <- ax[, 1]
    py <- ax[, 2]
    vv <- ax[, 3]

    if (curved) {
      cal <- NULL
      angle <- gg_curve_angles(px, py, vv)
      is_tick <- if (ncol(ax) >= 4L) {
        !is.na(ax[, 4]) & ax[, 4] == 1
      } else {
        rep(TRUE, nrow(ax))
      }
      is_tick <- is_tick & is.finite(angle)
      steps <- diff(sort(unique(vv[is_tick])))
      vstep <- if (length(steps) > 0) stats::median(steps) else NA_real_
      label <- rep("", nrow(ax))
      label[is_tick] <- gg_format_tick_values(vv[is_tick], vstep)
    } else {
      cal <- gg_axis_calibration(ax)
      if (is.null(cal)) {
        next
      }
      angle <- rep(cal$angle, nrow(ax))
      is_tick <- rep(TRUE, nrow(ax))
      label <- gg_format_tick_values(vv, cal$vstep)
    }

    frames[[i]] <- data.frame(
      group = i,
      axis = labels[i],
      axis_label = labels[i],
      x = px,
      y = py,
      value = vv,
      label = label,
      angle = angle,
      tick = is_tick,
      cal_x0 = if (is.null(cal)) NA_real_ else cal$x0,
      cal_y0 = if (is.null(cal)) NA_real_ else cal$y0,
      cal_dxdv = if (is.null(cal)) NA_real_ else cal$dxdv,
      cal_dydv = if (is.null(cal)) NA_real_ else cal$dydv,
      cal_vref = if (is.null(cal)) NA_real_ else cal$vref,
      cal_vstep = if (is.null(cal)) NA_real_ else cal$vstep,
      off_x = if (is.null(cal)) 0 else cal$offset[1],
      off_y = if (is.null(cal)) 0 else cal$offset[2],
      axis_colour = aes_axis_colour[i],
      axis_linewidth = aes_linewidth[i],
      axis_linetype = aes_linetype[i],
      tick_colour = aes_tick_colour[i],
      text_colour = aes_text_colour[i],
      text_size = aes_text_size[i],
      label_colour = aes_label_colour[i],
      label_size = aes_label_size[i],
      stringsAsFactors = FALSE
    )
  }

  frames <- frames[!vapply(frames, is.null, logical(1))]
  if (length(frames) == 0L) {
    return(list(data = NULL, curved = curved))
  }

  out <- do.call(rbind, frames)
  rownames(out) <- NULL
  list(data = out, curved = curved)
}

#' Build the layer data for the sample points
#'
#' @param x A \code{biplot} object from \pkg{biplotEZ}.
#'
#' @return A data frame with the display coordinates, group membership and
#'   sample-label metadata.
#' @noRd
sample_point_frame <- function(x) {
  Z <- as.matrix(x$Z)
  group <- x$group.aes
  if (is.null(group)) {
    group <- factor(rep("Data", nrow(Z)))
  }
  group <- droplevels(as.factor(group))

  labels <- x$samples$label
  if (is.null(labels)) {
    labels <- rep(FALSE, nrow(Z))
  }
  labels <- rep_len(as.logical(labels), nrow(Z))
  labels[is.na(labels)] <- FALSE

  label_name <- x$samples$label.name %||% rownames(x$X) %||%
    as.character(seq_len(nrow(Z)))

  data.frame(
    x = Z[, 1],
    y = Z[, 2],
    group = group,
    show_label = labels,
    point_label = rep_len(as.character(label_name), nrow(Z)),
    label_side = rep_len(x$samples$label.side %||% "bottom", nrow(Z)),
    label_offset = rep_len(x$samples$label.offset %||% 0.5, nrow(Z)),
    label_size = rep_len((x$samples$label.cex %||% 0.75) * 3.88, nrow(Z)),
    label_colour = rep_len(x$samples$label.col %||% "black", nrow(Z)),
    stringsAsFactors = FALSE
  )
}

#' Build the layer data for alpha bags and concentration ellipses
#'
#' \code{biplotEZ::alpha.bags()} and \code{biplotEZ::ellipses()} both store
#' ready-to-draw boundary coordinates, and \pkg{biplotEZ} renders them with a
#' plain \code{graphics::polygon()} call. They are therefore drawn here exactly
#' as stored, with no refitting.
#'
#' @param coords Named list of polygon coordinate matrices.
#' @param aes_list The matching aesthetics list from the biplotEZ object.
#'
#' @return A data frame, or \code{NULL} when there is nothing to draw.
#' @noRd
polygon_frame <- function(coords, aes_list) {
  if (is.null(coords) || length(coords) == 0L) {
    return(NULL)
  }

  nms <- names(coords) %||% paste0("Group_", seq_along(coords))
  frames <- vector("list", length(coords))

  for (i in seq_along(coords)) {
    xy <- as.matrix(coords[[i]])
    if (nrow(xy) < 3L) {
      next
    }

    frames[[i]] <- data.frame(
      x = xy[, 1],
      y = xy[, 2],
      polygon = nms[i],
      poly_group = i,
      poly_colour = (aes_list$col %||% "black")[i],
      poly_linewidth = ((aes_list$lwd %||% 1)[i]) * 0.5,
      poly_linetype = (aes_list$lty %||% 1)[i],
      poly_alpha = (aes_list$opacity %||% 0.25)[i],
      stringsAsFactors = FALSE
    )
  }

  frames <- frames[!vapply(frames, is.null, logical(1))]
  if (length(frames) == 0L) {
    return(NULL)
  }

  out <- do.call(rbind, frames)
  rownames(out) <- NULL
  out
}

#' Build axis titles carrying the proportion of inertia explained
#'
#' @param x A \code{biplot} object from \pkg{biplotEZ}.
#' @param dim_prefix Label prefix for the displayed basis, e.g. \code{"PC"}.
#' @param percentages Logical; append the percentage of variation explained.
#'
#' @return A character vector of length two.
#' @noRd
gg_dim_labels <- function(x, dim_prefix = "PC", percentages = TRUE) {
  basis <- x$e.vects
  if (is.null(basis) || length(basis) < 2L) {
    basis <- c(1, 2)
  }
  basis <- basis[seq_len(2)]
  titles <- paste0(dim_prefix, basis)

  eigval <- x$eigenvalues
  if (percentages && !is.null(eigval) && sum(eigval) > 0) {
    pct <- 100 * eigval[basis] / sum(eigval)
    if (all(is.finite(pct))) {
      titles <- paste0(titles, " (", format(round(pct, 1), trim = TRUE), "%)")
    }
  }

  titles
}


# ─────────────────────────────────────────────────────────────────────────────
# The shared builder
# ─────────────────────────────────────────────────────────────────────────────

#' Assemble a ggplot2 biplot from a prepared biplotEZ object
#'
#' All \code{wrap_bipl5_gg()} methods funnel into this builder; they differ only
#' in the preparation performed on the biplotEZ object beforehand and in the
#' defaults they supply.
#'
#' @inheritParams wrap_bipl5_gg
#' @param z.axes Pre-computed axis coordinates, captured by the calling method
#'   before any un-centering of \code{x$X}.
#' @param curved Logical; whether the axes are spline curves.
#' @param dim_prefix Label prefix for the displayed basis.
#' @param fit_quality_text Display-quality string shown as the plot caption.
#'
#' @return A \code{ggplot} object.
#' @noRd
build_bipl5_gg <- function(
  x,
  z.axes = NULL,
  curved = NULL,
  dim_prefix = "PC",
  fit_quality_text = NULL,
  axis_colour = "black",
  tick_colour = NULL,
  text_colour = NULL,
  label_colour = NULL,
  axis_linewidth = 0.25,
  axis_linetype = "solid",
  text_size = 2.6,
  label_size = 3.88,
  axis_labels = TRUE,
  axis_ticks = TRUE,
  axis_text = TRUE,
  tick_extend = TRUE,
  tick_length = 0.025,
  text_dodge = 0.03,
  label_dodge = 0.03,
  label_placement = c("positive", "negative", "peripheral"),
  point_size = 2,
  point_alpha = 0.6,
  sample_labels = NULL,
  class_means = NULL,
  alpha_bags = TRUE,
  conc_ellipses = TRUE,
  legend_title = "Group",
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  axis_percentages = TRUE,
  xlim = NULL,
  ylim = NULL,
  expand = TRUE,
  clip = "on",
  theme = ggplot2::theme_bw()
) {
  label_placement <- match.arg(label_placement)

  axis_layer <- calibrated_axis_frame(
    x,
    z.axes = z.axes,
    curved = curved,
    axis_colour = axis_colour,
    tick_colour = tick_colour,
    text_colour = text_colour,
    label_colour = label_colour,
    axis_linewidth = axis_linewidth,
    axis_linetype = axis_linetype,
    text_size = text_size,
    label_size = label_size
  )

  samples <- sample_point_frame(x)
  n_groups <- nlevels(samples$group)
  point_cols <- rep_len(
    x$samples$col %||% "black",
    max(n_groups, 1L)
  )
  point_pch <- rep_len(x$samples$pch %||% 16, max(n_groups, 1L))
  point_cex <- rep_len(x$samples$cex %||% 1, max(n_groups, 1L))
  if (is.null(point_size)) {
    point_size <- point_cex * 2
  }
  if (is.null(point_alpha)) {
    point_alpha <- x$samples$opacity %||% 1
  }

  p <- ggplot2::ggplot()

  # ── alpha bags and concentration ellipses ───────────────────────────────
  bag_frame <- if (isTRUE(alpha_bags)) {
    polygon_frame(x$alpha.bags, x$alpha.bag.aes)
  }
  ellipse_frame <- if (isTRUE(conc_ellipses)) {
    polygon_frame(x$conc.ellipses, x$conc.ellipse.aes)
  }
  poly_frame <- do.call(rbind, Filter(Negate(is.null), list(
    bag_frame,
    if (!is.null(ellipse_frame)) {
      ellipse_frame$poly_group <- ellipse_frame$poly_group +
        (if (is.null(bag_frame)) 0L else max(bag_frame$poly_group))
      ellipse_frame
    }
  )))

  if (!is.null(poly_frame) && nrow(poly_frame) > 0L) {
    p <- p +
      ggplot2::geom_polygon(
        data = poly_frame,
        mapping = ggplot2::aes(
          x = .data$x,
          y = .data$y,
          group = .data$poly_group
        ),
        colour = poly_frame$poly_colour,
        fill = poly_frame$poly_colour,
        alpha = poly_frame$poly_alpha,
        linewidth = poly_frame$poly_linewidth,
        linetype = poly_frame$poly_linetype,
        show.legend = FALSE
      )
  }

  # ── sample points ───────────────────────────────────────────────────────
  # Shapes are only mapped when biplotEZ actually distinguishes the groups by
  # plotting character; a redundant scale would otherwise have to be replaced
  # alongside the colour scale whenever the user restyles the plot.
  vary_shape <- n_groups > 1L && length(unique(point_pch)) > 1L

  if (n_groups > 1L) {
    if (vary_shape) {
      p <- p +
        ggplot2::geom_point(
          data = samples,
          mapping = ggplot2::aes(
            x = .data$x,
            y = .data$y,
            colour = .data$group,
            shape = .data$group
          ),
          size = point_size,
          alpha = point_alpha
        ) +
        ggplot2::scale_shape_manual(
          values = stats::setNames(point_pch, levels(samples$group)),
          name = legend_title
        )
    } else {
      p <- p +
        ggplot2::geom_point(
          data = samples,
          mapping = ggplot2::aes(
            x = .data$x,
            y = .data$y,
            colour = .data$group
          ),
          shape = point_pch[1],
          size = point_size,
          alpha = point_alpha
        )
    }

    p <- p +
      ggplot2::scale_colour_manual(
        values = stats::setNames(point_cols, levels(samples$group)),
        name = legend_title
      )
  } else {
    p <- p +
      ggplot2::geom_point(
        data = samples,
        mapping = ggplot2::aes(x = .data$x, y = .data$y),
        colour = point_cols[1],
        shape = point_pch[1],
        size = point_size,
        alpha = point_alpha
      )
  }

  # ── sample labels ───────────────────────────────────────────────────────
  show_sample_labels <- if (is.null(sample_labels)) {
    any(samples$show_label)
  } else {
    isTRUE(sample_labels)
  }
  if (show_sample_labels) {
    lab <- if (is.null(sample_labels)) {
      samples[samples$show_label, , drop = FALSE]
    } else {
      samples
    }
    if (nrow(lab) > 0L) {
      just <- gg_label_justification(lab$label_side)
      p <- p +
        ggplot2::geom_text(
          data = lab,
          mapping = ggplot2::aes(
            x = .data$x,
            y = .data$y,
            label = .data$point_label
          ),
          colour = lab$label_colour,
          size = lab$label_size,
          hjust = just$hjust,
          vjust = just$vjust,
          show.legend = FALSE
        )
    }
  }

  # ── class means ─────────────────────────────────────────────────────────
  show_means <- if (is.null(class_means)) {
    isTRUE(x$class.means)
  } else {
    isTRUE(class_means)
  }
  if (show_means) {
    means_frame <- class_mean_frame(x, samples)
    if (!is.null(means_frame) && nrow(means_frame) > 0L) {
      p <- p +
        ggplot2::geom_point(
          data = means_frame,
          mapping = ggplot2::aes(x = .data$x, y = .data$y),
          colour = means_frame$mean_colour,
          shape = means_frame$mean_shape,
          size = means_frame$mean_size,
          stroke = 1.1,
          show.legend = FALSE
        )
    }
  }

  # ── calibrated axes ─────────────────────────────────────────────────────
  if (!is.null(axis_layer$data)) {
    p <- p +
      geom_calibrated_axis(
        data = axis_layer$data,
        mapping = ggplot2::aes(
          x = .data$x,
          y = .data$y,
          group = .data$group,
          angle = .data$angle,
          label = .data$label,
          axis_label = .data$axis_label,
          tick = .data$tick,
          value = .data$value,
          cal_x0 = .data$cal_x0,
          cal_y0 = .data$cal_y0,
          cal_dxdv = .data$cal_dxdv,
          cal_dydv = .data$cal_dydv,
          cal_vref = .data$cal_vref,
          cal_vstep = .data$cal_vstep,
          off_x = .data$off_x,
          off_y = .data$off_y,
          axis_colour = .data$axis_colour,
          axis_linewidth = .data$axis_linewidth,
          axis_linetype = .data$axis_linetype,
          tick_colour = .data$tick_colour,
          text_colour = .data$text_colour,
          text_size = .data$text_size,
          label_colour = .data$label_colour,
          label_size = .data$label_size
        ),
        axis_type = if (isTRUE(axis_layer$curved)) "curve" else "line",
        axis_labels = axis_labels,
        axis_ticks = axis_ticks,
        axis_text = axis_text,
        tick_extend = tick_extend && !isTRUE(axis_layer$curved),
        tick_length = tick_length,
        text_dodge = text_dodge,
        label_dodge = label_dodge,
        label_placement = label_placement,
        inherit.aes = FALSE,
        show.legend = FALSE
      )
  }

  # ── scaffolding ─────────────────────────────────────────────────────────
  dim_labels <- gg_dim_labels(
    x,
    dim_prefix = dim_prefix,
    percentages = axis_percentages
  )
  if (is.null(caption)) {
    caption <- fit_quality_text
  }
  if (identical(caption, "")) {
    caption <- NULL
  }

  # Legend titles are set on the plot as well as on the scales, so that the
  # legend keeps its title when a user swaps in their own colour or shape scale.
  plot_labels <- list(
    x = dim_labels[1],
    y = dim_labels[2],
    title = title,
    subtitle = subtitle,
    caption = caption
  )
  if (n_groups > 1L) {
    plot_labels$colour <- legend_title
    if (vary_shape) {
      plot_labels$shape <- legend_title
    }
  }

  p +
    ggplot2::coord_equal(
      xlim = xlim,
      ylim = ylim,
      expand = expand,
      clip = clip
    ) +
    do.call(ggplot2::labs, plot_labels) +
    theme
}

#' Build the layer data for class means
#'
#' @param x A \code{biplot} object from \pkg{biplotEZ}.
#' @param samples The sample-point frame, used to derive means when the biplot
#'   object does not store them.
#'
#' @return A data frame, or \code{NULL}.
#' @noRd
class_mean_frame <- function(x, samples) {
  Zmeans <- x$Zmeans
  if (is.null(Zmeans)) {
    levels_present <- levels(samples$group)
    if (length(levels_present) == 0L) {
      return(NULL)
    }
    Zmeans <- do.call(
      rbind,
      lapply(levels_present, function(g) {
        colMeans(cbind(samples$x, samples$y)[samples$group == g, , drop = FALSE])
      })
    )
    rownames(Zmeans) <- levels_present
  }
  Zmeans <- as.matrix(Zmeans)
  if (nrow(Zmeans) == 0L) {
    return(NULL)
  }

  means_aes <- x$means.aes
  n <- nrow(Zmeans)

  data.frame(
    x = Zmeans[, 1],
    y = Zmeans[, 2],
    mean_label = rownames(Zmeans) %||% as.character(seq_len(n)),
    mean_colour = rep_len(means_aes$col %||% "black", n),
    mean_shape = rep_len(means_aes$pch %||% 15, n),
    mean_size = rep_len((means_aes$cex %||% 1) * 3, n),
    stringsAsFactors = FALSE
  )
}

#' Translate biplotEZ label sides into ggplot2 justifications
#'
#' @param side Character vector of \code{"bottom"}, \code{"top"},
#'   \code{"left"} or \code{"right"}.
#'
#' @return A list with numeric elements \code{hjust} and \code{vjust}.
#' @noRd
gg_label_justification <- function(side) {
  side <- as.character(side)
  hjust <- ifelse(
    side == "left",
    1.15,
    ifelse(side == "right", -0.15, 0.5)
  )
  vjust <- ifelse(
    side == "bottom",
    1.4,
    ifelse(side == "top", -0.4, 0.5)
  )
  list(hjust = hjust, vjust = vjust)
}


# ─────────────────────────────────────────────────────────────────────────────
# Per-family preparation
# ─────────────────────────────────────────────────────────────────────────────

#' Prepare a biplotEZ object and collect the family-specific settings
#'
#' The four biplot families \code{wrap_bipl5_gg()} supports differ only in how
#' the axis coordinates must be captured, in the label prefix for the
#' scaffolding axes, and in how display quality is reported.  Everything else is
#' shared, so those differences are resolved here rather than through separate
#' S3 methods.
#'
#' @param x A \code{biplot} object from \pkg{biplotEZ}.
#'
#' @return A list with elements \code{x}, \code{z.axes}, \code{curved},
#'   \code{dim_prefix} and \code{fit_quality_text}.
#' @noRd
gg_biplot_context <- function(x) {
  # Mirror the class fix-ups performed by wrap_bipl5() so that regression and
  # PCO biplots are not mistaken for PCA biplots.
  if (length(class(x)) < 2 && !is.null(x$PCOaxes)) {
    class(x) <- c(class(x), "PCO")
  }
  family <- intersect(c("regress", "PCO", "CVA", "PCA"), class(x))
  if (length(family) == 0L) {
    stop(
      paste0(
        "wrap_bipl5_gg() cannot render a biplot of class <",
        paste(class(x), collapse = "/"),
        ">. Apply PCA(), CVA(), regress() or PCO() first."
      ),
      call. = FALSE
    )
  }
  family <- family[1]

  if (is.null(x$samples)) {
    x <- biplotEZ::samples(x)
  }
  if (is.null(x$axes)) {
    x <- biplotEZ::axes(x)
  }

  # X is deliberately left centred and scaled: the ggplot2 renderer needs no
  # hovertext, and axes_coordinates() must see the processed matrix.
  if (identical(family, "CVA") && is.null(x$means.aes)) {
    x <- biplotEZ::means(x)
  }

  is_spline <- identical(x$PCOaxes, "splines")
  if (is_spline) {
    # axes_coordinates() builds spline axes from raw.X, so it must see the
    # processed matrix to place markers in the displayed coordinate system.
    # This mirrors the swap performed by wrap_bipl5.PCO().
    keep_raw <- x$raw.X
    x$raw.X <- x$X
    z.axes <- zero_to_near_zero(biplotEZ::axes_coordinates(x))
    x$raw.X <- keep_raw
  } else {
    z.axes <- biplotEZ::axes_coordinates(x)
  }

  dim_prefix <- switch(family, PCA = "PC", CVA = "CV", "Dim")

  fit_quality_text <- switch(
    family,
    PCA = fit_quality(x$eigenvalues, x$e.vects, dim_prefix = "PC"),
    CVA = fit_quality(x$eigenvalues, x$e.vects, dim_prefix = "CV"),
    regress = regression_fit_quality(
      X = x$X,
      Z = x$Z,
      basis = c(1, 2),
      dim_prefix = "Dim"
    ),
    NULL
  )

  list(
    x = x,
    z.axes = z.axes,
    curved = is_spline,
    dim_prefix = dim_prefix,
    fit_quality_text = fit_quality_text
  )
}


# ─────────────────────────────────────────────────────────────────────────────
# wrap_bipl5_gg
# ─────────────────────────────────────────────────────────────────────────────

#' Render a biplotEZ biplot with ggplot2
#'
#' \code{wrap_bipl5_gg()} is the \pkg{ggplot2} counterpart of
#' \code{\link{wrap_bipl5}}: it takes the same two-dimensional \pkg{biplotEZ}
#' objects and returns an ordinary \code{ggplot} object instead of a reactive
#' plotly widget.  Sample points, class means, alpha bags and concentration
#' ellipses are drawn as standard \pkg{ggplot2} layers, and the calibrated axes
#' are drawn by \code{\link{geom_calibrated_axis}}.
#'
#' @details
#' The calibrated axes reproduce the geometry used by \code{gggda::geom_axis()},
#' the layer behind \code{ordr::geom_cols_axis()}: each axis is a line spanning
#' the plotting window, tick marks are short segments orthogonal to that line,
#' marker values are printed rotated to the angle of the axis and dodged to one
#' side, and the variable name is written at the window border on the increasing
#' end of the axis.
#'
#' The two packages differ in where the markers come from.  \pkg{gggda} derives
#' them at draw time from the \code{center} and \code{scale} aesthetics using
#' Wilkinson's extended breaks algorithm, which restricts it to variables whose
#' calibration is a simple centring and scaling.  \pkg{bipl5} instead consumes
#' the calibrated coordinates that \code{biplotEZ::axes_coordinates()} has
#' already computed, and recovers the affine value-to-position map they imply.
#' That map is then used to lay markers out against the final plotting window,
#' so the marker values, their spacing and their phase are all exactly the ones
#' \pkg{biplotEZ} chose, while the axes still span the panel and never inflate
#' its range.
#'
#' By default markers are regenerated along the whole visible axis
#' (\code{tick_extend = TRUE}) using that recovered calibration, because the
#' marker range \pkg{biplotEZ} returns is tied to the range of the fitted values
#' and need not cover the corners of the plotting window.  Set
#' \code{tick_extend = FALSE} to draw only the markers
#' \code{biplotEZ::axes_coordinates()} returned.
#'
#' Axis element aesthetics accept a single value, one value per axis, or the
#' string \code{"biplotEZ"} to adopt the aesthetics stored on the object by
#' \code{biplotEZ::axes()}.  \code{tick_colour}, \code{text_colour} and
#' \code{label_colour} additionally accept \code{NULL}, in which case they
#' follow \code{axis_colour}.
#'
#' Principal coordinate analysis biplots built with spline axes are drawn as
#' polylines, with each marker oriented by the local direction of the curve.
#'
#' @param x A two-dimensional \code{biplot} object from \pkg{biplotEZ}, with
#'   \code{PCA()}, \code{CVA()}, \code{regress()} or \code{PCO()} applied.
#' @param axis_colour Colour of the axis lines.
#' @param tick_colour,text_colour,label_colour Colours of the tick marks, the
#'   marker values and the variable names; \code{NULL} follows
#'   \code{axis_colour}.
#' @param axis_linewidth,axis_linetype Width and type of the axis lines.
#' @param text_size,label_size Text sizes, in millimetres, of the marker values
#'   and the variable names.
#' @param axis_labels,axis_ticks,axis_text Logical; whether to draw the variable
#'   names, the tick marks and the marker values.
#' @param tick_extend Logical; regenerate markers along the whole visible axis
#'   from the recovered calibration.  See Details.
#' @param tick_length Length of the tick marks, as a proportion of the smaller
#'   of the panel width and height.
#' @param text_dodge,label_dodge Orthogonal distances of the marker values and
#'   the variable names from the axis, as proportions of the smaller of the
#'   panel width and height.
#' @param label_placement One of \code{"positive"} (the default; the increasing
#'   end of the axis), \code{"negative"}, or \code{"peripheral"}.
#' @param point_size,point_alpha Size and opacity of the sample points.
#' @param sample_labels Logical; whether to label the sample points.
#'   \code{NULL} (the default) follows the \code{label} setting of
#'   \code{biplotEZ::samples()}.
#' @param class_means Logical; whether to draw class means.  \code{NULL} (the
#'   default) follows the \code{class.means} flag of the biplot object.
#' @param alpha_bags,conc_ellipses Logical; whether to draw alpha bags and
#'   concentration ellipses when the object carries them.
#' @param legend_title Title of the group legend.
#' @param title,subtitle,caption Plot annotations.  \code{caption} defaults to
#'   the display-quality string that \code{\link{wrap_bipl5}} shows beneath the
#'   plotly biplot.
#' @param axis_percentages Logical; append the percentage of variation explained
#'   to the two scaffolding-axis titles.
#' @param xlim,ylim,expand,clip Passed to \code{\link[ggplot2]{coord_equal}}.
#' @param theme A \pkg{ggplot2} theme added to the plot.
#'
#' @return A \code{ggplot} object, which can be further modified with the usual
#'   \code{+} syntax.
#'
#' @seealso \code{\link{wrap_bipl5}} for the interactive plotly renderer, and
#'   \code{\link{geom_calibrated_axis}} for the underlying layer.
#'
#' @importFrom ggplot2 .data
#'
#' @references
#' Gower, J. C. and Hand, D. J. (1996). \emph{Biplots}. London: Chapman and Hall.
#'
#' Gower, J. C., Lubbe, S. and le Roux, N. J. (2011).
#' \emph{Understanding Biplots}. Chichester: Wiley.
#'
#' Brunson, J. C. and Read, Q. D. (2023). \emph{ordr: A 'tidyverse' Extension
#' for Ordinations and Biplots}. R package version 0.1.1.
#'
#' @examples
#' \dontrun{
#' library(biplotEZ)
#' library(ggplot2)
#'
#' biplot(iris[, 1:4], scale = TRUE) |>
#'   PCA(group.aes = iris[, 5]) |>
#'   wrap_bipl5_gg(title = "Predictive biplot of Anderson iris measurements")
#'
#' # the result is an ordinary ggplot, so it composes as usual
#' biplot(iris[, 1:4], scale = TRUE) |>
#'   PCA(group.aes = iris[, 5]) |>
#'   wrap_bipl5_gg(legend_title = "Species") +
#'   scale_color_brewer(type = "qual", palette = 2)
#'
#' # CVA biplots carry class means
#' biplot(iris[, 1:4]) |>
#'   CVA(classes = iris[, 5]) |>
#'   wrap_bipl5_gg()
#' }
#'
#' @export
wrap_bipl5_gg <- function(
  x,
  axis_colour = "black",
  tick_colour = NULL,
  text_colour = NULL,
  label_colour = NULL,
  axis_linewidth = 0.25,
  axis_linetype = "solid",
  text_size = 2.6,
  label_size = 3.88,
  axis_labels = TRUE,
  axis_ticks = TRUE,
  axis_text = TRUE,
  tick_extend = TRUE,
  tick_length = 0.025,
  text_dodge = 0.03,
  label_dodge = 0.03,
  label_placement = c("positive", "negative", "peripheral"),
  point_size = 2,
  point_alpha = 0.6,
  sample_labels = NULL,
  class_means = NULL,
  alpha_bags = TRUE,
  conc_ellipses = TRUE,
  legend_title = "Group",
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  axis_percentages = TRUE,
  xlim = NULL,
  ylim = NULL,
  expand = TRUE,
  clip = "on",
  theme = ggplot2::theme_bw()
) {
  if (!inherits(x, "biplot")) {
    stop(
      "'x' must be a 'biplot' object from the biplotEZ package.",
      call. = FALSE
    )
  }
  if (!is.null(x$dim.biplot) && x$dim.biplot != 2) {
    stop("wrap_bipl5_gg only accepts biplots of two dimensions", call. = FALSE)
  }

  ctx <- gg_biplot_context(x)

  build_bipl5_gg(
    ctx$x,
    z.axes = ctx$z.axes,
    curved = ctx$curved,
    dim_prefix = ctx$dim_prefix,
    fit_quality_text = ctx$fit_quality_text,
    axis_colour = axis_colour,
    tick_colour = tick_colour,
    text_colour = text_colour,
    label_colour = label_colour,
    axis_linewidth = axis_linewidth,
    axis_linetype = axis_linetype,
    text_size = text_size,
    label_size = label_size,
    axis_labels = axis_labels,
    axis_ticks = axis_ticks,
    axis_text = axis_text,
    tick_extend = tick_extend,
    tick_length = tick_length,
    text_dodge = text_dodge,
    label_dodge = label_dodge,
    label_placement = match.arg(label_placement),
    point_size = point_size,
    point_alpha = point_alpha,
    sample_labels = sample_labels,
    class_means = class_means,
    alpha_bags = alpha_bags,
    conc_ellipses = conc_ellipses,
    legend_title = legend_title,
    title = title,
    subtitle = subtitle,
    caption = caption,
    axis_percentages = axis_percentages,
    xlim = xlim,
    ylim = ylim,
    expand = expand,
    clip = clip,
    theme = theme
  )
}
