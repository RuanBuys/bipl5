#' Initialise plotly plot with layout in place
#'
#' @param dpquality Quality of display. Text string
#' @param basis Basis vectors to construct display
#' @param ax_pred Include axis predictivity button
#' @param FM include fit measures button
#' @param vec_dis include vector display button
#' @param n Number of slider steps
#' @param x_colnames Character vector of axis/variable names for AxisNames dropdown
#' @noRd
plot_scaffolding <- function(
  dpquality,
  basis,
  PC_toggle = TRUE,
  ax_pred = TRUE,
  TDA = TRUE,
  vec_dis = TRUE,
  n = 21,
  x_colnames = character(0)
) {
  Title <- "Overall quality and axis predictivities (cumulative)"
  x_colnames <- as.character(x_colnames)
  slider_steps <- lapply(seq_len(n), function(i) {
    list(
      label = "",
      value = i,
      method = "skip",
      args = list()
    )
  })
  axis_name_buttons <- lapply(seq_along(x_colnames), function(i) {
    list(
      method = "skip",
      label = x_colnames[i]
    )
  })
  p_ly <- plot_ly() |>
    layout(
      legend = list(
        tracegroupgap = 0,
        xref = "container",
        yref = "container",
        x = 1,
        y = 0.82,
        groupclick = "toggleitem"
      ),
      xaxis = list(
        title = dpquality,
        showticklabels = FALSE,
        zeroline = FALSE,
        showgrid = FALSE,
        domain = c(0, 1)
      ),
      yaxis = list(
        showticklabels = FALSE,
        zeroline = FALSE,
        scaleanchor = {
          'x'
        },
        scaleratio = 1,
        showgrid = FALSE
      ),
      xaxis2 = list(domain = c(0, 0.15), zeroline = TRUE),
      yaxis2 = list(zeroline = TRUE, side = "left", position = 0),
      xaxis3 = list(
        domain = c(0.65, 1),
        zeroline = TRUE,
        showgrid = TRUE,
        anchor = "y3",
        dtick = 1,
        title = "Dimension of Subspace"
      ),
      yaxis3 = list(
        zeroline = TRUE,
        anchor = "free",
        side = "left",
        position = 0.65,
        showgrid = TRUE,
        domain = c(0.15, 0.85),
        layer = "below traces",
        title = Title,
        range = c(0, 1)
      ),
      hoverlabel = list(font = list(family = "Courier New, monospace")),
      barmode = "stack",
      updatemenus = list(
        list(
          y = 0.8,
          type = "buttons",
          x = 0,
          pad = list(r = 0), # JS bumps this to 60 only while overlay fit measures are open
          showactive = TRUE,
          active = -1,
          buttons = list(
            list(
              method = "skip",
              args = list("type", "scatter"),
              label = "Measures of Fit",
              name = "AxisStats",
              visible = ax_pred,
              execute = FALSE
            ),
            list(
              method = "skip",
              args = list("type", "scatter"),
              label = "Translated Axes",
              name = "TransAxes",
              visible = TDA,
              execute = FALSE
            ),
            list(
              method = "skip",
              args = list("type", "scatter"),
              label = "Vector Display",
              name = "vecload",
              visible = vec_dis,
              execute = FALSE
            ),
            list(
              method = "skip",
              args = list("type", "scatter"),
              label = "Edit: Axes",
              name = "EditAxes",
              visible = FALSE,
              execute = FALSE
            )
          )
        ),
        list(
          type = "dropdown",
          x = 0,
          pad = list(r = 0), # JS bumps this to 60 only while overlay fit measures are open
          visible = PC_toggle,
          name = "PC_toggle",
          buttons = list(
            list(
              method = "skip",
              args = list("type", "scatter"),
              label = "PC 1 & 2"
            ),
            list(
              method = "skip",
              args = list("type", "histogram"),
              label = "PC 1 & 3"
            ),
            list(
              method = "skip",
              args = list("type", "histogram"),
              label = "PC 2 & 3"
            )
          )
        ),
        list(
          type = "dropdown",
          x = 0.5,
          visible = FALSE,
          name = "Fit_toggle",
          xanchor = "left",
          buttons = list(
            list(
              method = "skip",
              args = list("type", "scatter"),
              label = "Cum. Predictivity"
            ),
            list(
              method = "skip",
              args = list("type", "scatter"),
              label = "Cum. Adequacy"
            ),
            list(
              method = "skip",
              args = list("type", "scatter"),
              label = "Scree Plot"
            ),
            list(
              method = "skip",
              args = list("type", "scatter"),
              label = "Variance Explained"
            ),
            list(
              method = "skip",
              args = list("type", "histogram"),
              label = "Summary Table"
            )
          )
        ),
        list(
          type = "dropdown",
          visible = FALSE,
          x = 0,
          y = 0,
          xanchor = "right",
          yanchor = "bottom",
          name = "Slider_toggle",
          direction = "up",
          buttons = axis_name_buttons
        )
      ),
      sliders = list(
        list(
          currentvalue = list(prefix = "Axis 1"),
          x = 0.0,
          y = -0.15,
          xanchor = "left",
          yanchor = "bottom",
          steps = slider_steps,
          visible = FALSE
        )
      )
    ) |>
    plotly::config(mathjax = "cdn")

  return(p_ly)
}


#' Get the quadrants each axis will fall into
#'
#' @param z.axes List containing the coordinates of the tickmarks per axis
#'
#' @return vector indicating quadrants
#' @noRd
get_quads_axes <- function(z.axes) {
  quads <- numeric(length(z.axes))
  for (i in 1:length(z.axes)) {
    max_entry <- which(z.axes[[i]][, 3] == max(z.axes[[i]][, 3]))
    m <- z.axes[[i]][max_entry, 2] / z.axes[[i]][max_entry, 1]
    if ((m > 0) && (z.axes[[i]][max_entry, 1] > 0)) {
      quads[i] <- 1
    }
    if (m > 0 && (z.axes[[i]][max_entry, 1] < 0)) {
      quads[i] <- 3
    }
    if (m < 0 && (z.axes[[i]][max_entry, 1] < 0)) {
      quads[i] <- 2
    }
    if (m < 0 && (z.axes[[i]][max_entry, 1] > 0)) {
      quads[i] <- 4
    }
  }
  return(quads)
}


#' Convenience function to shorten the TDA biplot axes
#'
#' @param z.axes List containing the coordinates of the tickmarks per axis
#' @param ellip coordinates of an ellipse
#'
#' @return trimmed z.axes
#' @noRd
shorten_axes <- function(z.axes, ellip) {
  p <- length(z.axes)
  gradient <- vapply(z.axes, function(x) x[1, 2] / x[1, 1], numeric(1))
  thetas <- atan(gradient)
  axes <- vector("list", p)

  for (i in seq_len(p)) {
    rotate <- RotationConstructor(thetas[i])
    back_rotate <- RotationConstructor(-thetas[i])

    # Rotate the calibrated axis so selection can happen on a horizontal line
    # while keeping the original "pretty" tick labels from z.axes[[i]][, 3].
    rotated_axis <- cbind(
      z.axes[[i]][, 1:2, drop = FALSE] %*% rotate,
      z.axes[[i]][, 3]
    )
    rotated_ellip <- ellip %*% rotate

    # The ellipse gives the approximate left/right trimming bounds once the
    # axis has been made horizontal.
    bounds <- rbind(
      c(min(rotated_ellip[, 1]), 0),
      c(max(rotated_ellip[, 1]), 0)
    )
    Z_ranges <- bounds %*% back_rotate
    Zhats <- obtain_zhat(Z_ranges, z.axes[[i]])
    zhat_range <- sort(Zhats)
    tick_vals <- rotated_axis[, 3]

    # Keep all existing calibrated ticks that fall inside the trimmed range,
    # plus the nearest tick just outside on each side so the axis starts and
    # ends on a tick mark.
    inside_idx <- which(
      tick_vals >= zhat_range[1] & tick_vals <= zhat_range[2]
    )
    lower_idx <- which(tick_vals < zhat_range[1])
    upper_idx <- which(tick_vals > zhat_range[2])

    keep_idx <- inside_idx
    if (length(lower_idx) > 0) {
      keep_idx <- c(keep_idx, lower_idx[which.max(tick_vals[lower_idx])])
    }
    if (length(upper_idx) > 0) {
      keep_idx <- c(keep_idx, upper_idx[which.min(tick_vals[upper_idx])])
    }

    keep_idx <- sort(unique(keep_idx))
    trimmed_axis <- rotated_axis[keep_idx, , drop = FALSE]
    # Rotate the trimmed coordinates back to the original biplot scaffold; the
    # tick labels are carried through unchanged.
    axes[[i]] <- cbind(
      trimmed_axis[, 1:2, drop = FALSE] %*% back_rotate,
      trimmed_axis[, 3]
    )
  }
  return(axes)
}


#' Linearly interpolate between tickmarks
#'
#' @param Z_ranges Z ranges for which tickmarks are desired
#' @param z.axis details on the current axis. i'th element in z.axes
#'
#' @return Vector containing tickmarks
#' @noRd
obtain_zhat <- function(Z_ranges, z.axis) {
  #simply going to linearly interpolate the two endpoints
  #can do so by projecting coorindates with their ticks on x-axis

  Z_hat1 <- (Z_ranges[1, 1] - z.axis[1, 1]) /
    (z.axis[nrow(z.axis), 1] - z.axis[1, 1])
  Z_hat1 <- Z_hat1 * (z.axis[nrow(z.axis), 3] - z.axis[1, 3]) + z.axis[1, 3]

  Z_hat2 <- (Z_ranges[2, 1] - z.axis[1, 1]) /
    (z.axis[nrow(z.axis), 1] - z.axis[1, 1])
  Z_hat2 <- Z_hat2 * (z.axis[nrow(z.axis), 3] - z.axis[1, 3]) + z.axis[1, 3]

  return(c(Z_hat1, Z_hat2))
}

#' Test if the current biplot is a correlation biplot
#'
#' @param x A PCA object from the biplotEZ package
#'
#' @return True or False
#' @noRd
is_correlation <- function(x) {
  basis <- x$e.vects
  new_x <- biplotEZ::biplot(x$raw.X, scaled = x$scaled, center = x$center) |>
    biplotEZ::PCA(e.vects = basis)
  #test in the ax.one.unit if they are the same
  diff <- sum(x$ax.one.unit - new_x$ax.one.unit)
  return(diff != 0)
}

#' Calculate the quality of fit for a PC biplot
#'
#' @param eigval eigenvalues returned from prcomp analysis
#' @param basis basis of display - pc pair
#'
#' @return Character vector
#' @noRd
fit_quality <- function(eigval, basis, dim_prefix = "PC") {
  if (is.null(eigval)) {
    return("")
  }
  fit.quality <- paste0(
    "Quality of display = ",
    round(
      ((eigval[basis[1]] + eigval[basis[2]]) / sum(eigval)) * 100,
      digits = 2
    ),
    "%",
    " = ",
    round((eigval[basis[1]] / sum(eigval)) * 100, digits = 2),
    "% (",
    dim_prefix,
    basis[1],
    ") + ",
    round((eigval[basis[2]] / sum(eigval)) * 100, digits = 2),
    "% (",
    dim_prefix,
    basis[2],
    ")"
  )
  return(fit.quality)
}

#' Compute regression-biplot fit components
#'
#' For non-orthogonal display coordinates, the per-dimension contributions are
#' defined by orthogonalizing the displayed coordinates in their given order.
#' This yields additive sums of squares while preserving the displayed
#' Dim 1 / Dim 2 ordering.
#'
#' @param X Data matrix used to fit the regression biplot.
#' @param Z Display-coordinate matrix.
#'
#' @return A list with total, overall, and per-dimension sums of squares.
#' @noRd
regression_fit_components <- function(X, Z) {
  X <- as.matrix(X)
  Z <- as.matrix(Z)

  if (
    is.null(dim(X)) ||
      is.null(dim(Z)) ||
      nrow(X) == 0L ||
      nrow(Z) == 0L ||
      nrow(X) != nrow(Z) ||
      ncol(Z) == 0L
  ) {
    return(list(total_ss = 0, overall_ss = 0, dim_ss = numeric(0)))
  }

  total_ss <- sum(X^2)
  dim_ss <- numeric(ncol(Z))

  if (!is.finite(total_ss) || total_ss <= 0) {
    return(list(total_ss = total_ss, overall_ss = 0, dim_ss = dim_ss))
  }

  q_cols <- NULL
  tol <- sqrt(.Machine$double.eps)

  for (j in seq_len(ncol(Z))) {
    zj <- Z[, j, drop = FALSE]

    if (!is.null(q_cols)) {
      zj <- zj - q_cols %*% crossprod(q_cols, zj)
    }

    norm_zj <- sqrt(sum(zj^2))
    if (!is.finite(norm_zj) || norm_zj <= tol) {
      next
    }

    qj <- zj / norm_zj
    dim_ss[j] <- sum((qj %*% crossprod(qj, X))^2)
    q_cols <- cbind(q_cols, qj)
  }

  list(
    total_ss = total_ss,
    overall_ss = sum(dim_ss),
    dim_ss = dim_ss
  )
}

#' Calculate the quality of fit for a regression biplot
#'
#' @param X Data matrix used to fit the regression biplot.
#' @param Z Display-coordinate matrix.
#' @param basis Basis labels corresponding to the columns of \code{Z}.
#' @param dim_prefix Basis label prefix, usually \code{"Dim"}.
#' @param digits Number of displayed decimal places.
#'
#' @return Character vector.
#' @noRd
regression_fit_quality <- function(
  X,
  Z,
  basis = seq_len(ncol(as.matrix(Z))),
  dim_prefix = "Dim",
  digits = 2
) {
  fit_comp <- regression_fit_components(X, Z)
  n_terms <- min(length(fit_comp$dim_ss), length(basis))

  if (
    n_terms == 0L ||
      !is.finite(fit_comp$total_ss) ||
      fit_comp$total_ss <= 0
  ) {
    return("")
  }

  dim_pct <- 100 * fit_comp$dim_ss[seq_len(n_terms)] / fit_comp$total_ss
  overall_pct <- 100 * fit_comp$overall_ss / fit_comp$total_ss

  labels <- vapply(
    seq_len(n_terms),
    function(i) {
      if (i == 1L) {
        return("R_1^2")
      }

      paste0(
        "R_{",
        i,
        "|",
        paste(seq_len(i - 1L), collapse = ","),
        "}^2"
      )
    },
    character(1)
  )

  pieces <- vapply(
    seq_len(n_terms),
    function(i) {
      paste0(
        round(dim_pct[i], digits),
        "% (",
        labels[i],
        ")"
      )
    },
    character(1)
  )

  paste0(
    "R^2_disp = ",
    round(overall_pct, digits),
    "% = ",
    paste(pieces, collapse = " + ")
  )
}

#' Build a MathJax quality-of-display label for regression biplots
#'
#' @param X Data matrix used to fit the regression biplot.
#' @param Z Display-coordinate matrix.
#' @param digits Number of displayed decimal places.
#'
#' @return A \code{plotly::TeX()} object or an empty string.
#' @noRd
regression_fit_quality_tex <- function(X, Z, digits = 2) {
  fit_comp <- regression_fit_components(X, Z)
  n_terms <- length(fit_comp$dim_ss)

  if (
    n_terms == 0L ||
      !is.finite(fit_comp$total_ss) ||
      fit_comp$total_ss <= 0
  ) {
    return("")
  }

  dim_pct <- 100 * fit_comp$dim_ss / fit_comp$total_ss
  overall_pct <- 100 * fit_comp$overall_ss / fit_comp$total_ss

  labels <- vapply(
    seq_len(n_terms),
    function(i) {
      if (i == 1L) {
        return("R_1^2")
      }

      paste0(
        "R_{",
        i,
        " \\\\mid ",
        paste(seq_len(i - 1L), collapse = ","),
        "}^2"
      )
    },
    character(1)
  )

  pieces <- vapply(
    seq_len(n_terms),
    function(i) {
      paste0(
        round(dim_pct[i], digits),
        "\\%\\,(",
        labels[i],
        ")"
      )
    },
    character(1)
  )

  plotly::TeX(
    paste0(
      "R^2_{disp}=",
      round(overall_pct, digits),
      "\\%= ",
      paste(pieces, collapse = " + ")
    )
  )
}

#' Obtain predicted values for PCA biplot
#'
#' @param x An object of class biplotEZ::PCA
#'
#' @return matrix n x p containing predicted values
#' @noRd
obtain_xhat <- function(x, z.axes = NULL) {
  if ("regress" %in% class(x) && !is.null(z.axes)) {
    # Regression biplot: interpolate predicted values from calibrated axes.
    # For each axis, rotate Z and the axis ticks so the axis is horizontal,
    # then linearly interpolate the tick labels at each observation's
    # projected x-coordinate.
    p <- x$p
    n <- x$n
    Xhat <- matrix(NA_real_, nrow = n, ncol = p)
    colnames(Xhat) <- colnames(x$X)

    for (i in seq_len(p)) {
      m <- (z.axes[[i]][2, 2] - z.axes[[i]][1, 2]) /
        (z.axes[[i]][2, 1] - z.axes[[i]][1, 1])
      theta <- atan(m)
      rot <- RotationConstructor(theta)

      rotZ <- x$Z %*% rot
      rot_ax <- z.axes[[i]][, 1:2, drop = FALSE] %*% rot

      Xhat[, i] <- stats::approx(
        x = rot_ax[, 1],
        y = z.axes[[i]][, 3],
        xout = rotZ[, 1],
        rule = 2
      )$y
    }
    return(Xhat)
  } else if (!is.null(x$Lmat)) {
    if (nrow(x$Lmat) == ncol(x$Lmat)) {
      Xhat <- x$Z %*% solve(x$Lmat)[x$e.vects, ]
    } else {
      Xhat <- x$X
    }
  } else {
    Xhat <- x$X
  }
  if (x$scaled) {
    Xhat <- scale(Xhat, center = FALSE, scale = 1 / x$sd)
  }
  if (x$center) {
    Xhat <- scale(Xhat, center = -1 * x$means, scale = FALSE)
  }
  return(Xhat)
}


#' Insert JS code for spline axes
#'
#' @param p_ly plotly graph
#'
#' @noRd
insert_spline_js <- function(p_ly, p) {
  p_ly <- p_ly |>
    htmlwidgets::onRender(
      "

     function(el,x,data) {
        console.log(el);
       var arr1 = new Array(data.p).fill(0);
       el.on('plotly_legendclick', function(dat){


          if(dat.data[dat.curveNumber].meta[0] === 'data'){
          return;
          }
          if(dat.data[dat.curveNumber].meta[0] === 'density'){
          return;
          }
          if(dat.data[dat.curveNumber].meta[0] === 'axis_pred'){
            return;
          }


          // REMOVE AXES

          var axis = dat.data[dat.curveNumber].legendgroup;
          var num = Number(axis.replace('Ax',''));


          var indeces =[];
          el.data.forEach(function(item,idx,arr){
              if(arr[idx].legendgroup === undefined){
              return;
              }
              if(arr[idx].legendgroup === axis){
                  indeces.push(idx);
              }
              if(arr[idx].customdata === undefined){
              return;
              }
              if(arr[idx].customdata[0] === axis){
                indeces.push(idx);
              }
          });


          var old_annotations = el.layout.annotations;
            old_annotations.forEach(function(item,idx,arr){
              if(arr[idx].customdata === num){
                old_annotations[idx].visible = !old_annotations[idx].visible;
              }
            });
          var new_annot = {annotations:old_annotations};

          hidden = arr1[num-1];
          var update = {'visible': ['legendonly',true][hidden]};
          hidden = [1,0][hidden];
          arr1[num-1] = hidden;
          var new_annot = {annotations:old_annotations};

          Plotly.restyle(el.id,update,indeces);

          Plotly.relayout(el.id,new_annot);
          return false;
        });

//---------------Click on the graph---------------------
        el.on('plotly_click', function(d) {

        // Click on the axes

        console.log(d);
        //el.layout.annotations.push(newAnnotation);
        var NewAnot1 = {
          x: d.points[0].x,
          y: d.points[0].y,
          text: '&#124;',
          showarrow: false,
          meta:'axis',
          xaxis:'x',
          yaxis:'y',
          visible: true,
          textangle: -Math.atan(d.points[0].customdata)*180/Math.PI,
          font: {
            size: 8
          },
          customdata: Number(d.points[0].data.legendgroup.replace('Ax',''))
        };
        var NewAnot2 = {
          x: d.points[0].x,
          y: d.points[0].y,
          text: (d.points[0].hovertext).toString(),
          showarrow: false,
          meta:'axis',
          xaxis:'x',
          yaxis:'y',
          visible: true,
          textangle: -Math.atan(d.points[0].customdata)*180/Math.PI,
          yshift: -12*Math.cos(Math.atan(d.points[0].customdata)),
          xshift: 12*Math.sin(Math.atan(d.points[0].customdata)),
          font: {
            size: 10
          },
          customdata: Number(d.points[0].data.legendgroup.replace('Ax',''))
        };



        el.layout.annotations.push(NewAnot1,NewAnot2);
        console.log(el.layout.annotations)
        Plotly.relayout(el.id,{annotations:el.layout.annotations});
        });

}

   ",
      data = list(p = p)
    )

  return(p_ly)
}

#' Get gradients at each point
#'
#' @param z z axes coordinates
#'
#' @noRd
get_gradients <- function(z) {
  p <- nrow(z)
  m <- (z[2:(p - 1) + 1, 2] - z[2:(p - 1) - 1, 2]) /
    (z[2:(p - 1) + 1, 1] - z[2:(p - 1) - 1, 1])
  m <- c(NA, m, NA)
  return(m)
}
