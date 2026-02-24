#' Plot object from the biplotEZ package in plotly
#'
#' @param x An object of class 'biplot' from the biplotEZ package
#'
#' @return A plotly graph containing the biplot
#' @export plot_bipl5
#'
#' @examples
#' library(biplotEZ)
#' x<-biplot(data = iris) |> PCA() |> plot_bipl5()
#' x<-biplot(iris[,1:4]) |> CVA(classes=iris[,5]) |> plot_bipl5()
#' x<-biplot(iris[,1:4]) |> PCO(dist.func = sqrtManhattan) |> plot_bipl5()
plot_bipl5 <- function(x) {
  if (x$dim.biplot != 2) {
    stop("plot_bipl5 only accepts biplots of two dimensions")
  }
  if (length(class(x)) < 2) {
    if (!is.null(x$PCOaxes)) {
      class(x) <- c(class(x), "PCO")
    }
  }
  UseMethod("plot_bipl5", x)
}


#' Plot a PCA biplot from the biplotEZ package
#'
#' @param x An object of class biplotEZ::biplot
#'
#' @return A plotly graph
#' @export
#' @method plot_bipl5 PCA
#' @S3method plot_bipl5 PCA
#'
#' @examples
#' library(biplotEZ)
#' x<-biplot(data = iris) |> PCA() |> plot_bipl5()
plot_bipl5.PCA <- function(x) {
  if (is.null(x$samples)) {
    x <- biplotEZ::samples(x)
  }
  #if(is.null(x$axes))
  #for now all the default settings of axes is supported. Might change later on
  x <- biplotEZ::axes(x)
  # Populate observation-level fit fields (e.g., sample.predictivity) for hover text.
  x <- biplotEZ::fit.measures(x)
  x$X <- scale(x$X, center = FALSE, scale = 1 / x$sd)
  x$X <- scale(x$X, -x$means, scale = FALSE)
  #next we need to obtain the coordinates of the other PC's
  #we first test if the current display is a correlation biplot:
  corr <- is_correlation(x)

  PC13 <- biplotEZ::biplot(x$raw.X, center = x$center, scaled = x$scaled) |>
    biplotEZ::PCA(e.vects = c(1, 3), correlation.biplot = corr) |>
    biplotEZ::axes() |>
    # Keep sample.predictivity available for the PC 1 & 3 hover payload.
    biplotEZ::fit.measures()
  PC23 <- biplotEZ::biplot(x$raw.X, center = x$center, scaled = x$scaled) |>
    biplotEZ::PCA(e.vects = c(2, 3), correlation.biplot = corr) |>
    biplotEZ::axes() |>
    # Keep sample.predictivity available for the PC 2 & 3 hover payload.
    biplotEZ::fit.measures()
  PC13$X <- x$X
  PC23$X <- x$X

  color <- x$samples$col
  scale <- x$scaled
  symbol <- pch_to_plotly(x$samples$pch)
  group <- x$group.aes
  if (length(levels(x$group.aes)) == 1) {
    group <- factor(rep("Data", x$n))
  }
  fit.quality <- fit_quality(x$eigenvalues, x$e.vects)

  #build scaffolding ->biplotEZ helper
  p_ly <- plot_scaffolding(
    fit.quality,
    x$e.vects,
    TRUE,
    TRUE,
    TRUE,
    TRUE,
    x_colnames = colnames(x$X)
  )

  payl_13 <- payload_new()
  payl_13$fit_qual <- fit_quality(PC13$eigenvalues, PC13$e.vects)
  payl_13 <- plot_scaffolding_payload(
    payl_13,
    dpquality = payl_13$fit_qual,
    basis = PC13$e.vects,
    PC_toggle = TRUE,
    ax_pred = TRUE,
    TDA = TRUE,
    vec_dis = TRUE
  )
  payl_23 <- payload_new()
  payl_23$fit_qual <- fit_quality(PC23$eigenvalues, PC23$e.vects)
  payl_23 <- plot_scaffolding_payload(
    payl_23,
    dpquality = payl_23$fit_qual,
    basis = PC23$e.vects,
    PC_toggle = TRUE,
    ax_pred = TRUE,
    TDA = TRUE,
    vec_dis = TRUE
  )

  #Insert any polygons to the plot -> EZ plotly layers

  if (!is.null(x$alpha.bags)) {
    p_ly <- insert_polygon_EZ(p_ly, x$alpha.bags, x$alpha.bag.aes)
    #p_ly<-insert_polygon_EZ_payload(payl_13)
  }
  if (!is.null(x$conc.ellipses)) {
    p_ly <- insert_polygon_EZ(
      p_ly,
      x$conc.ellipses,
      x$conc.ellipse.aes,
      "Con. Ellipses"
    )
  }

  Xhat <- obtain_xhat(x)
  Xhat_13 <- obtain_xhat(PC13)
  Xhat_23 <- obtain_xhat(PC23)

  z.axes <- biplotEZ::axes_coordinates(x)
  z.axes13 <- biplotEZ::axes_coordinates(PC13)
  z.axes23 <- biplotEZ::axes_coordinates(PC23)
  #insert Z coordinates ->PCAbiplot_Helper
  # Pass per-observation predictivity through so hovertext_generator can append it.
  obj <- list(
    Z = x$Z,
    group = group,
    n = x$n,
    x = as.matrix(x$X),
    XHat = Xhat,
    sample.predictivity = x$sample.predictivity
  )
  obj13 <- list(
    Z = PC13$Z,
    group = group,
    n = PC13$n,
    x = as.matrix(PC13$X),
    XHat = Xhat_13,
    sample.predictivity = PC13$sample.predictivity
  )
  obj23 <- list(
    Z = PC23$Z,
    group = group,
    n = PC23$n,
    x = as.matrix(PC23$X),
    XHat = Xhat_23,
    sample.predictivity = PC23$sample.predictivity
  )

  p_ly <- insert_Z_coo(p_ly, obj, symbol, color, TRUE)

  payl_13 <- insert_Z_coo_payload(
    payl_13,
    obj13,
    p_ly_pch = symbol,
    Col = color,
    visible = TRUE
  )
  payl_23 <- insert_Z_coo_payload(
    payl_23,
    obj23,
    p_ly_pch = symbol,
    Col = color,
    visible = TRUE
  )

  #insert class means if any
  if (x$class.means) {
    if (is.null(x$means.aes)) {
      x <- biplotEZ::means(x)
    }
    Mean_symbol <- pch_to_plotly(x$means.aes$pch)
    p_ly <- insert_class_means(p_ly, x$Zmeans, Mean_symbol, x$means.aes$col)
    payl_13 <- insert_class_means_payload(
      payl_13,
      x$Zmeans,
      Mean_symbol,
      x$means.aes$col
    )
  }

  #insert Linear Axes
  update <- insert_linear_axes(z.axes, x, p_ly)
  p_ly <- update[[1]]
  grads <- update[[2]]

  out1 <- insert_linear_axes_payload(payl_13, z.axes13, PC13)
  payl_13 <- out1$payload
  grads_13 <- out1$grads

  out2 <- insert_linear_axes_payload(payl_23, z.axes23, PC23)
  payl_23 <- out2$payload
  grads_23 <- out2$grads

  #Unit circle
  p_ly <- insert_unit_circle(p_ly, visible = FALSE)
  payl_13 <- insert_unit_circle_payload(payl_13, visible = FALSE)
  payl_23 <- insert_unit_circle_payload(payl_23, visible = FALSE)

  #insert vector representation

  temp <- list(V = x$Vr, x = x$X, p = x$p)
  temp13 <- list(V = PC13$Vr, x = PC13$X, p = PC13$p)
  temp23 <- list(V = PC23$Vr, x = PC23$X, p = PC23$p)
  p_ly <- insert_vector_annots(p_ly, temp, NULL, NULL)
  payl_13 <- insert_vector_annots_payload(payl_13, temp13)
  payl_23 <- insert_vector_annots_payload(payl_23, temp23)

  #insert Translated Density Axes
  temp <- add_TDA(z.axes, x, Z = x$Z, group = group, p_ly = p_ly, Col = color)
  p_ly <- temp$p_ly
  payl_13 <- add_TDA_payload(
    payload = payl_13,
    z.axes = z.axes13,
    x = PC13,
    Z = PC13$Z,
    group = group,
    Col = color
  )
  payl_23 <- add_TDA_payload(
    payload = payl_23,
    z.axes = z.axes23,
    x = PC23,
    Z = PC23$Z,
    group = group,
    Col = color
  )

  #initialialise a hollow payload for PC1&2
  payl_12 <- payload_new()
  payl_12$m <- temp$m
  payl_12$shift <- temp$shift

  #next we add the slider controls to the payloads
  payl_12 <- slider_control_payload(payl_12, n_inside = 17, n_outside = 4)
  payl_13 <- slider_control_payload(payl_13, n_inside = 17, n_outside = 4)
  payl_23 <- slider_control_payload(payl_23, n_inside = 17, n_outside = 4)

  #finally we add the fit measure table to the payloads
  payl_12 <- add_table_payload(payl_12, x = x)
  payl_13 <- add_table_payload(payl_13, x = PC13)
  payl_23 <- add_table_payload(payl_23, x = PC23)

  #create new payload specifically for for the fit measures
  fm_payl <- list()
  fm_payl["CumPred"] <- add_axis_pred_payload(fm_payl, x, EZ = TRUE)

  fm_payl["CumAd"] <- add_axis_adeq_payload(fm_payl, x, EZ = TRUE)
  fm_payl["VarExp"] <- add_prop_variance_payload(x)
  fm_payl["Scree"] <- add_scree_payload(x)

  p_ly <- insert_linear_js_v1(
    p_ly,
    p = x$p,
    cols = x$axes$tick.label.col,
    payload = list(
      "PC 1 & 2" = payl_12$payload,
      "PC 1 & 3" = payl_13$payload,
      "PC 2 & 3" = payl_23$payload
    ),
    fm_payload = fm_payl
  )

  return(p_ly)
}


#' Plot a CVA biplot from the biplotEZ package
#'
#' @param x An object of class biplotEZ::biplot
#'
#' @return A plotly graph
#' @export
#' @method plot_bipl5 CVA
#' @S3method plot_bipl5 CVA
#' @import biplotEZ
#'
#' @examples
#' library(biplotEZ)
#' x<-biplotEZ::biplot(iris[,1:4]) |> biplotEZ::CVA(classes=iris[,5]) |> plot_bipl5()
plot_bipl5.CVA <- function(x) {
  if (is.null(x$samples)) {
    x <- biplotEZ::samples(x)
  }
  #currently only the default aesthetics for the axes are supported
  x <- biplotEZ::axes(x)
  if (is.null(x$means.aes)) {
    x <- biplotEZ::means(x)
  }

  color <- x$samples$col
  scale <- x$scaled
  symbol <- pch_to_plotly(x$samples$pch)
  group <- x$group.aes
  if (length(levels(x$group.aes)) == 1) {
    group <- factor(rep("Data", x$n))
  }
  Z <- x$Z
  n <- x$n
  p <- x$p
  basis <- x$e.vects
  ax.aes <- x$axes

  #build scaffolding
  p_ly <- plot_scaffolding(
    "",
    basis,
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    x_colnames = colnames(x$X)
  )

  #Insert any polygons to the plot

  if (!is.null(x$alpha.bags)) {
    p_ly <- insert_polygon_EZ(p_ly, x$alpha.bags, x$alpha.bag.aes)
  }
  if (!is.null(x$conc.ellipses)) {
    p_ly <- insert_polygon_EZ(
      p_ly,
      x$conc.ellipses,
      x$conc.ellipse.aes,
      "Con. Ellipses"
    )
  }

  if (!is.null(x$Lmat)) {
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

  z.axes <- biplotEZ::axes_coordinates(x)

  #insert Z coordinates
  obj <- list(Z = Z, group = group, n = x$n, x = x$X)
  #p_ly<-insert_Z_coo(p_ly,obj,symbol,color,TRUE)

  num_groups <- length(levels(group))
  x <- fit.measures(x)
  fit <- round(x$within.class.sample.predictivity, 3)
  for (i in 1:num_groups) {
    p_ly <- p_ly |>
      add_trace(
        data = Z,
        x = Z[group == levels(group)[i], 1],
        y = Z[group == levels(group)[i], 2],
        name = levels(group)[i],
        type = "scatter",
        mode = "markers",
        hovertext = paste(
          rownames(x$X)[group == levels(group)[i]],
          "\n",
          "Predictivity:",
          fit[group == levels(group)[i]]
        ),
        hoverinfo = "text+name",
        customdata = (1:x$n)[group == levels(group)[i]],
        meta = "data",
        xaxis = "x",
        yaxis = "y",
        visible = TRUE,
        marker = list(symbol = symbol[i], color = color[i], opacity = 1),
        legendgroup = "data",
        legendgrouptitle = list(text = "<b>Data</b>")
      )
  }

  #insert class means if any
  if (x$class.means) {
    Mean_symbol <- pch_to_plotly(x$means.aes$pch)
    p_ly <- insert_class_means(p_ly, x$Zmeans, Mean_symbol, x$means.aes$col)
  }

  #insert Linear Axes
  update <- insert_linear_axes(z.axes, x, p_ly)
  p_ly <- update[[1]]
  grads <- update[[2]]

  #Unit circle
  p_ly <- p_ly |>
    add_trace(
      x = cos(seq(0, 2 * pi, length.out = 200)),
      y = sin(seq(0, 2 * pi, length.out = 200)),
      type = "scatter",
      mode = "lines",
      line = list(color = 'red', width = 1.2),
      name = "Unit Circle",
      showlegend = FALSE,
      meta = 'veccircle',
      xaxis = "x",
      yaxis = "y",
      hoverinfo = 'name',
      visible = FALSE
    )

  p_ly <- insert_linear_js_v1(
    p_ly,
    m = grads,
    p = p,
    cols = x$axes$tick.label.col,
    payload = NULL
  )

  return(p_ly)
}


#' Plot a PCO biplot from the biplotEZ package
#'
#' @param x An object of class biplotEZ::biplot
#'
#' @return A plotly graph
#' @export
#' @method plot_bipl5 PCO
#' @S3method plot_bipl5 PCO
#'
#' @examples
#' library(biplotEZ)
#' x<-biplot(iris[,1:4]) |> PCO(dist.func = sqrtManhattan) |> plot_bipl5()
plot_bipl5.PCO <- function(x) {
  if (is.null(x$samples)) {
    x <- biplotEZ::samples(x)
  }
  if (is.null(x$axes)) {
    x <- biplotEZ::axes(x)
  }
  color <- x$samples$col
  scale <- x$scaled
  symbol <- pch_to_plotly(x$samples$pch)
  group <- x$group.aes
  if (length(levels(x$group.aes)) == 1) {
    group <- factor(rep("Data", x$n))
  }
  Z <- x$Z
  n <- x$n
  p <- x$p
  basis <- x$e.vects
  ax.aes <- x$axes

  if (!is.null(x$Lmat)) {
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

  if (x$PCOaxes == "splines") {
    # z.axes <- spsUtil::quiet(lapply(1:p,
    #                                 biplotEZ:::biplot.spline.axis, Z, Xhat,
    #                                 means=x$means, sd=x$sd, n.int=ax.aes$ticks,
    #                                 spline.control=x$spline.control))
    z.axes <- biplotEZ::axes_coordinates(x)

    #build scaffolding
    p_ly <- plot_scaffolding(
      "",
      basis,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      x_colnames = colnames(x$X)
    )

    #insert Z coordinates
    obj <- list(Z = Z, group = group, n = n, x = x$X)
    p_ly <- insert_Z_coo(p_ly, obj, symbol, color, TRUE)

    radius <- max(abs(Z)) * 1.2
    theta <- seq(0, 2 * pi, length.out = 200)
    elipcoords <- cbind(radius * cos(theta), radius * sin(theta))

    z.axes <- check_inside_circle(z.axes, radius, NULL)

    for (i in 1:p) {
      AxName <- paste("<b>", colnames(x$X)[i], "</b>")
      endp <- z.axes[[i]][which.max(z.axes[[i]][, 3]), 1:2]
      pos <- "right"
      if (endp[1] < 0) {
        pos <- "left"
      }
      idx <- which(z.axes[[i]][, 4] == 1)

      full_m <- get_gradients(z.axes[[i]])
      m <- full_m[idx]
      if (any(is.na(m))) {
        idx <- idx[!is.na(m)]
        m <- m[!is.na(m)]
      }

      p_ly <- p_ly |>
        add_trace(
          x = z.axes[[i]][, 1],
          y = z.axes[[i]][, 2],
          type = "scatter",
          mode = "lines",
          line = list(color = 'grey', width = 1, simplify = FALSE),
          name = colnames(x$X)[i],
          legendgroup = paste("Ax", i, sep = ""),
          meta = 'axis',
          xaxis = "x",
          yaxis = "y",
          customdata = full_m,
          visible = TRUE,
          hovertext = round(z.axes[[i]][, 3], 1),
          hoverinfo = "text"
        ) |>

        add_annotations(
          x = z.axes[[i]][idx, 1],
          y = z.axes[[i]][idx, 2],
          text = as.character(z.axes[[i]][idx, 3]),
          showarrow = FALSE,
          textangle = -atan(m) * 180 / pi,
          visible = TRUE,
          yshift = -12 * cos(atan(m)),
          xshift = 12 * sin(atan(m)),
          meta = 'axis',
          xaxis = "x",
          yaxis = "y",
          customdata = i,
          font = list(size = 10)
        ) |>
        add_annotations(
          x = z.axes[[i]][idx, 1],
          y = z.axes[[i]][idx, 2],
          text = "&#124;",
          showarrow = FALSE,
          textangle = -atan(m) * 180 / pi,
          visible = TRUE,
          meta = 'axis',
          xaxis = "x",
          yaxis = "y",
          customdata = i,
          font = list(size = 8)
        ) |>
        add_trace(
          x = endp[1],
          y = endp[2],
          text = AxName,
          type = "scatter",
          mode = "text",
          textposition = pos,
          legendgroup = paste("Ax", i, sep = ""),
          showlegend = FALSE,
          textfont = list(size = 12),
          meta = 'axis',
          xaxis = "x",
          yaxis = "y",
          visible = TRUE
        )
    }

    p_ly <- p_ly |>
      add_trace(
        x = elipcoords[, 1],
        y = elipcoords[, 2],
        type = "scatter",
        mode = "lines",
        line = list(color = 'green', width = 0.6),
        name = "circle",
        showlegend = F,
        meta = 'circle',
        xaxis = "x",
        yaxis = "y",
        visible = TRUE,
        hoverinfo = "none"
      )
    p_ly <- insert_spline_js(p_ly, p)
  }

  if (x$PCOaxes == "regression") {
    z.axes <- biplotEZ::axes_coordinates(x)
    Xhat <- x$Z %*% solve(t(x$Z) %*% x$Z) %*% t(x$Z) %*% x$X
    if (x$scaled) {
      Xhat <- scale(Xhat, center = FALSE, scale = 1 / x$sd)
    }
    if (x$center) {
      Xhat <- scale(Xhat, center = -1 * x$means, scale = FALSE)
    }
    #build scaffolding
    p_ly <- plot_scaffolding(
      "",
      basis,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      x_colnames = colnames(x$X)
    )

    #insert Z coordinates
    obj <- list(Z = Z, group = group, n = n, x = x$X)
    p_ly <- insert_Z_coo(p_ly, obj, symbol, color, TRUE)

    update <- insert_linear_axes(z.axes, x, p_ly)
    p_ly <- update[[1]]
    grads <- update[[2]]

    p_ly <- insert_linear_js(
      p_ly,
      Xhat = Xhat,
      m = grads,
      p = p,
      cols = x$axes$tick.label.col
    )
  }
  return(p_ly)
}
