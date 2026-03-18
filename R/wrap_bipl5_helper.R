#' Build a single bipl5 mdsDisplay for one PC combination
#'
#' Constructs a complete mdsDisplay containing traces, annotations, slider
#' controls, a fit table, and a \code{bipl5_data} sub-object for one
#' principal-component pair.  All layers are built uniformly through the
#' \code{_mdsDisplay} family of functions so that every PC combination shares
#' the same structure.
#'
#' @param ez_obj A biplotEZ \code{biplot} object that has already been
#'   processed through \code{biplotEZ::PCA()}, \code{biplotEZ::axes()},
#'   and \code{biplotEZ::fit.measures()} for the specific dimension pair being
#'   built.
#' @param group Factor vector of group memberships (length \code{ez_obj$n}).
#' @param color Character vector of colours, one per group level.
#' @param symbol Character vector of plotly marker symbols, one per group
#'   level (output of \code{pch_to_plotly()}).
#' @param x_ref The primary biplotEZ object used for polygon data and class
#'   means aesthetics.  This is typically the user's originally requested
#'   biplotEZ object, not necessarily \code{ez_obj}.
#' @param include_polygons Logical; if \code{TRUE}, alpha bags and
#'   concentration ellipses from \code{x_ref} are inserted.  Should only be
#'   \code{TRUE} for the primary mdsDisplay, because polygon coordinates are only
#'   valid in the coordinate system in which they were computed.
#' @param dim_prefix Basis label prefix used by \code{fit_quality()}, usually
#'   \code{"PC"} for PCA mdsDisplays or \code{"CV"} for CVA mdsDisplays.
#' @param ax_pred Logical; whether axis-predictivity scaffolding should be
#'   included in the mdsDisplay.
#' @param vec_dis Logical; whether unit-circle and vector-loading layers should
#'   be added. This is typically \code{TRUE} for PCA and \code{FALSE} for CVA.
#' @param fit_qual Optional override for the display-quality label shown below
#'   the biplot.
#'
#' @return An object of class \code{bipl5_mdsDisplay}.
#'
#' @details
#' The mdsDisplay is built in a fixed order so downstream JavaScript sees a stable
#' trace layout:
#' \enumerate{
#'   \item plot scaffolding and fit-quality text
#'   \item primary-pair polygons, when requested
#'   \item observation traces with hovertext built from actual and reconstructed
#'   values
#'   \item class-mean traces, using coordinates from \code{ez_obj} but
#'   aesthetics from \code{x_ref}
#'   \item calibrated linear axes
#'   \item optional PCA-only vector layers
#'   \item translated density axes and slider metadata
#'   \item the nested \code{bipl5_data} object used for inspection
#' }
#'
#' The distinction between \code{ez_obj} and \code{x_ref} matters.  Coordinates
#' that depend on the current basis, such as \code{Z}, \code{Zmeans}, and axis
#' coordinates, come from \code{ez_obj}.  Display options that should remain
#' consistent across mdsDisplays, such as polygon availability and class-mean
#' aesthetics, are taken from \code{x_ref}.
#' @noRd
build_one_mdsDisplay <- function(
  ez_obj,
  group,
  color,
  symbol,
  x_ref,
  include_polygons = FALSE,
  dim_prefix = "PC",
  ax_pred = TRUE,
  vec_dis = TRUE,
  z.axes = NULL,
  fit_qual = NULL
) {
  payl <- mdsDisplay_new()
  if (is.null(fit_qual)) {
    fit_qual <- fit_quality(
      ez_obj$eigenvalues,
      ez_obj$e.vects,
      dim_prefix = dim_prefix
    )
  }

  payl$fit_qual <- fit_qual
  payl <- plot_scaffolding_mdsDisplay(
    payl,
    dpquality = fit_qual,
    basis = ez_obj$e.vects,
    PC_toggle = TRUE,
    ax_pred = ax_pred,
    TDA = TRUE,
    vec_dis = vec_dis
  )

  # Polygons (only for PC 1&2, coordinates are in that space)
  if (include_polygons) {
    if (!is.null(x_ref$alpha.bags)) {
      payl <- insert_polygon_EZ_mdsDisplay(
        payl,
        x_ref$alpha.bags,
        x_ref$alpha.bag.aes
      )
    }
    if (!is.null(x_ref$conc.ellipses)) {
      payl <- insert_polygon_EZ_mdsDisplay(
        payl,
        x_ref$conc.ellipses,
        x_ref$conc.ellipse.aes,
        "Con. Ellipses"
      )
    }
  }

  # Axis coordinates and reconstructed values
  if (is.null(z.axes)) {
    z.axes <- biplotEZ::axes_coordinates(ez_obj)
  }
  Xhat <- obtain_xhat(ez_obj, z.axes = z.axes)

  # Sample points – use sample.predictivity (PCA) or

  # within.class.sample.predictivity (CVA) as available
  sample_pred <- ez_obj$sample.predictivity
  if (is.null(sample_pred)) {
    sample_pred <- ez_obj$within.class.sample.predictivity
  }
  obj <- list(
    Z = ez_obj$Z,
    group = group,
    n = ez_obj$n,
    x = as.matrix(ez_obj$X),
    XHat = Xhat,
    sample.predictivity = sample_pred
  )
  payl <- insert_Z_coo_mdsDisplay(
    payl,
    obj,
    p_ly_pch = symbol,
    Col = color,
    visible = TRUE
  )

  # Class means
  if (x_ref$class.means) {
    if (is.null(x_ref$means.aes)) {
      x_ref <- biplotEZ::means(x_ref)
    }
    Mean_symbol <- pch_to_plotly(x_ref$means.aes$pch)
    # Use Zmeans from ez_obj when available (correct CV/PC space);
    # fall back to computing from ez_obj$Z grouped by group factor.
    if (!is.null(ez_obj$Zmeans)) {
      Zmeans <- ez_obj$Zmeans
    } else {
      Zmeans <- do.call(
        rbind,
        lapply(levels(group), function(g) {
          colMeans(ez_obj$Z[group == g, , drop = FALSE])
        })
      )
      rownames(Zmeans) <- levels(group)
    }
    payl <- insert_class_means_mdsDisplay(
      payl,
      Zmeans,
      Mean_symbol,
      x_ref$means.aes$col
    )
  }

  # Linear axes
  out <- insert_linear_axes_mdsDisplay(payl, z.axes, ez_obj)
  payl <- out$mdsDisplay
  grads <- out$grads

  # Unit circle and vector annotations (PCA only)
  if (vec_dis) {
    payl <- insert_unit_circle_mdsDisplay(payl, visible = FALSE)
    temp <- list(V = ez_obj$Vr, x = ez_obj$X, p = ez_obj$p)
    payl <- insert_vector_annots_mdsDisplay(payl, temp)
  }

  # Translated Density Axes
  tda_out <- add_TDA_mdsDisplay(
    mdsDisplay = payl,
    z.axes = z.axes,
    x = ez_obj,
    Z = ez_obj$Z,
    group = group,
    Col = color
  )

  # Bundle into the convention expected by slider_control_mdsDisplay
  # After add_TDA_mdsDisplay: tda_out = list(mdsDisplay=<full mdsDisplay>, m=..., shift=...)
  bundle <- list()
  bundle$mdsDisplay <- tda_out$mdsDisplay
  bundle$m <- tda_out$m
  bundle$shift <- tda_out$shift

  # Slider controls write into bundle$mdsDisplay$...
  bundle <- slider_control_mdsDisplay(bundle, n_inside = 17, n_outside = 4)

  # Build Data object
  data <- new_bipl5_data(
    sample_coordinates = ez_obj$Z,
    axes_coordinates = z.axes,
    translated_axes_coordinates = tda_out$shift
  )

  # Preserve fit_qual on the outer level for print/inspection
  bundle$fit_qual <- payl$fit_qual

  new_bipl5_mdsDisplay(bundle, data)
}


#' Build a spline-axes mdsDisplay for PCO biplots
#'
#' Constructs a minimal mdsDisplay containing only sample points, spline axis
#' curves with tick marks, and a bounding circle.
#' There is no \code{XHat}, no translated density axes, and no linear axes.
#' The spline JavaScript handler is attached at plot time.
#'
#' @param ez_obj A biplotEZ \code{biplot} object with \code{PCO()} applied
#'   using spline axes.
#' @param group Factor vector of group memberships.
#' @param color Character vector of colours, one per group level.
#' @param symbol Character vector of plotly marker symbols.
#' @param z.axes Pre-computed axis coordinates from
#'   \code{biplotEZ::axes_coordinates()}.
#'
#' @return An object of class \code{bipl5_mdsDisplay}.
#' @noRd
build_spline_mdsDisplay <- function(ez_obj, group, color, symbol, z.axes) {
  payl <- mdsDisplay_new()
  payl$fit_qual <- ""

  # Scaffolding with no buttons active
  payl <- plot_scaffolding_mdsDisplay(
    payl,
    dpquality = "",
    basis = ez_obj$e.vects,
    PC_toggle = FALSE,
    ax_pred = FALSE,
    TDA = FALSE,
    vec_dis = FALSE
  )

  # Sample points — bare-minimum hovertext (no XHat)
  obj <- list(
    Z = ez_obj$Z,
    group = group,
    n = ez_obj$n,
    x = as.matrix(ez_obj$X),
    XHat = NULL,
    sample.predictivity = NULL
  )
  payl <- insert_Z_coo_mdsDisplay(
    payl,
    obj,
    p_ly_pch = symbol,
    Col = color,
    visible = TRUE
  )

  # Bounding circle
  p <- ez_obj$p
  radius <- max(abs(ez_obj$Z)) * 1.2
  theta <- seq(0, 2 * pi, length.out = 200)
  elipcoords <- cbind(radius * cos(theta), radius * sin(theta))

  # Clip spline axes to circle
  z.axes <- check_inside_circle(z.axes, radius, NULL)

  traces <- list()
  annotations <- list()

  for (i in seq_len(p)) {
    AxName <- paste0("<b>", colnames(ez_obj$X)[i], "</b>")
    endp <- z.axes[[i]][which.max(z.axes[[i]][, 3]), 1:2]
    pos <- if (endp[1] < 0) "left" else "right"

    # Tick mark indices (column 4 == 1 means labelled tick)
    idx <- which(z.axes[[i]][, 4] == 1)

    # Gradients along the spline curve
    full_m <- get_gradients(z.axes[[i]])
    m_at_ticks <- full_m[idx]
    if (any(is.na(m_at_ticks))) {
      idx <- idx[!is.na(m_at_ticks)]
      m_at_ticks <- m_at_ticks[!is.na(m_at_ticks)]
    }

    # Spline curve trace
    traces[[length(traces) + 1]] <- list(
      x = z.axes[[i]][, 1],
      y = z.axes[[i]][, 2],
      type = "scatter",
      mode = "lines",
      line = list(color = "grey", width = 1, simplify = FALSE),
      name = colnames(ez_obj$X)[i],
      legendgroup = paste0("Ax", i),
      meta = list("axis"),
      xaxis = "x",
      yaxis = "y",
      customdata = full_m,
      visible = TRUE,
      hovertext = round(z.axes[[i]][, 3], 1),
      hoverinfo = "text"
    )

    # Tick label + tick mark annotations
    if (length(idx) > 0) {
      for (k in seq_along(idx)) {
        ki <- idx[k]
        ang_deg <- -atan(m_at_ticks[k]) * 180 / pi

        # Tick label
        annotations[[length(annotations) + 1]] <- list(
          x = z.axes[[i]][ki, 1],
          y = z.axes[[i]][ki, 2],
          text = as.character(z.axes[[i]][ki, 3]),
          showarrow = FALSE,
          textangle = ang_deg,
          visible = TRUE,
          yshift = -12 * cos(atan(m_at_ticks[k])),
          xshift = 12 * sin(atan(m_at_ticks[k])),
          meta = list("axis"),
          xref = "x",
          yref = "y",
          customdata = i,
          font = list(size = 10)
        )

        # Tick mark
        annotations[[length(annotations) + 1]] <- list(
          x = z.axes[[i]][ki, 1],
          y = z.axes[[i]][ki, 2],
          text = "&#124;",
          showarrow = FALSE,
          textangle = ang_deg,
          visible = TRUE,
          meta = list("axis"),
          xref = "x",
          yref = "y",
          customdata = i,
          font = list(size = 8)
        )
      }
    }

    # Axis name at endpoint
    traces[[length(traces) + 1]] <- list(
      x = list(endp[1]),
      y = list(endp[2]),
      text = AxName,
      type = "scatter",
      mode = "text",
      textposition = pos,
      legendgroup = paste0("Ax", i),
      showlegend = FALSE,
      textfont = list(size = 12),
      meta = list("axis"),
      xaxis = "x",
      yaxis = "y",
      visible = TRUE
    )
  }

  # Green bounding circle
  traces[[length(traces) + 1]] <- list(
    x = elipcoords[, 1],
    y = elipcoords[, 2],
    type = "scatter",
    mode = "lines",
    line = list(color = "green", width = 0.6),
    name = "circle",
    showlegend = FALSE,
    meta = list("circle"),
    xaxis = "x",
    yaxis = "y",
    visible = TRUE,
    hoverinfo = "none"
  )

  payl <- mdsDisplay_add_traces(payl, traces)
  payl <- mdsDisplay_add_layout(payl, list(annotations = annotations))

  # Bundle — no slider, no TDA
  bundle <- list()
  bundle$mdsDisplay <- payl
  bundle$fit_qual <- ""

  data <- new_bipl5_data(
    sample_coordinates = ez_obj$Z,
    axes_coordinates = z.axes,
    translated_axes_coordinates = NULL
  )

  new_bipl5_mdsDisplay(bundle, data)
}
