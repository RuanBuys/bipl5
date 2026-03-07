#' Build a single bipl5 payload for one PC combination
#'
#' Constructs a complete payload containing traces, annotations, slider
#' controls, a fit table, and a \code{bipl5_data} sub-object for one
#' principal-component pair.  All layers are built uniformly through the
#' \code{_payload} family of functions so that every PC combination shares
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
#'   \code{TRUE} for the primary payload, because polygon coordinates are only
#'   valid in the coordinate system in which they were computed.
#' @param dim_prefix Basis label prefix used by \code{fit_quality()}, usually
#'   \code{"PC"} for PCA payloads or \code{"CV"} for CVA payloads.
#' @param ax_pred Logical; whether axis-predictivity scaffolding should be
#'   included in the payload.
#' @param vec_dis Logical; whether unit-circle and vector-loading layers should
#'   be added. This is typically \code{TRUE} for PCA and \code{FALSE} for CVA.
#'
#' @return An object of class \code{bipl5_payload}.
#'
#' @details
#' The payload is built in a fixed order so downstream JavaScript sees a stable
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
#' consistent across payloads, such as polygon availability and class-mean
#' aesthetics, are taken from \code{x_ref}.
#' @noRd
build_one_payload <- function(
  ez_obj,
  group,
  color,
  symbol,
  x_ref,
  include_polygons = FALSE,
  dim_prefix = "PC",
  ax_pred = TRUE,
  vec_dis = TRUE
) {
  payl <- payload_new()
  payl$fit_qual <- fit_quality(ez_obj$eigenvalues, ez_obj$e.vects,
                               dim_prefix = dim_prefix)
  payl <- plot_scaffolding_payload(
    payl,
    dpquality = payl$fit_qual,
    basis = ez_obj$e.vects,
    PC_toggle = TRUE,
    ax_pred = ax_pred,
    TDA = TRUE,
    vec_dis = vec_dis
  )

  # Polygons (only for PC 1&2, coordinates are in that space)
  if (include_polygons) {
    if (!is.null(x_ref$alpha.bags)) {
      payl <- insert_polygon_EZ_payload(
        payl,
        x_ref$alpha.bags,
        x_ref$alpha.bag.aes
      )
    }
    if (!is.null(x_ref$conc.ellipses)) {
      payl <- insert_polygon_EZ_payload(
        payl,
        x_ref$conc.ellipses,
        x_ref$conc.ellipse.aes,
        "Con. Ellipses"
      )
    }
  }

  # Reconstructed values and axis coordinates
  Xhat <- obtain_xhat(ez_obj)
  z.axes <- biplotEZ::axes_coordinates(ez_obj)

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
  payl <- insert_Z_coo_payload(
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
      Zmeans <- do.call(rbind, lapply(levels(group), function(g) {
        colMeans(ez_obj$Z[group == g, , drop = FALSE])
      }))
      rownames(Zmeans) <- levels(group)
    }
    payl <- insert_class_means_payload(
      payl,
      Zmeans,
      Mean_symbol,
      x_ref$means.aes$col
    )
  }

  # Linear axes
  out <- insert_linear_axes_payload(payl, z.axes, ez_obj)
  payl <- out$payload
  grads <- out$grads

  # Unit circle and vector annotations (PCA only)
  if (vec_dis) {
    payl <- insert_unit_circle_payload(payl, visible = FALSE)
    temp <- list(V = ez_obj$Vr, x = ez_obj$X, p = ez_obj$p)
    payl <- insert_vector_annots_payload(payl, temp)
  }

  # Translated Density Axes
  tda_out <- add_TDA_payload(
    payload = payl,
    z.axes = z.axes,
    x = ez_obj,
    Z = ez_obj$Z,
    group = group,
    Col = color
  )

  # Bundle into the convention expected by slider_control_payload
  # After add_TDA_payload: tda_out = list(payload=<full payload>, m=..., shift=...)
  bundle <- list()
  bundle$payload <- tda_out$payload
  bundle$m <- tda_out$m
  bundle$shift <- tda_out$shift

  # Slider controls write into bundle$payload$...
  bundle <- slider_control_payload(bundle, n_inside = 17, n_outside = 4)

  # Build Data object
  data <- new_bipl5_data(
    sample_coordinates = ez_obj$Z,
    axes_coordinates = z.axes,
    translated_axes_coordinates = tda_out$shift
  )

  # Preserve fit_qual on the outer level for print/inspection
  bundle$fit_qual <- payl$fit_qual

  new_bipl5_payload(bundle, data)
}
