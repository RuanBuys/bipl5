

#' Initiate the payload scaffolding
#'
#' @param payload List containing data and layout attributes for a plotly graph
#' @param dpquality Vector containing details of display quality
#' @param basis Basis vectors of the plot
#' @param PC_toggle Indicator whether to show PC toggle dropdown list
#' @param ax_pred Indicator whether to show axis predictivity button
#' @param TDA Indicator whether to show TDA button
#' @param vec_dis Indicator whether to show vector display button
#'
#' @return updated payload
#' @noRd
plot_scaffolding_payload <- function(payload, dpquality, basis,
                                     PC_toggle = TRUE,
                                     ax_pred = TRUE, TDA = TRUE, vec_dis = TRUE) {

  Title <- "Overall quality and axis predictivities (cumulative)"

  layout <- list(
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
      scaleanchor = "x",
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
      side = "left",
      position = 0.65,
      showgrid = TRUE,
      domain = c(0.15, 0.85),
      layer = "below traces",
      title = Title
    ),

    hoverlabel = list(font = list(family = "Courier New, monospace")),

    updatemenus = list(
      list(
        y = 0.8,
        type = "buttons",
        x = 0,
        showactive = FALSE,
        active = c(0, 1),
        buttons = list(
          list(
            method = "skip",
            args = list("type", "scatter"),
            label = "Axis Predictivity",
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
          )
        )
      ),
      list(
        type = "dropdown",
        x = 0,
        visible = PC_toggle,
        buttons = list(
          list(method = "skip", args = list("type", "scatter"),   label = "PC 1 & 2"),
          list(method = "skip", args = list("type", "histogram"), label = "PC 1 & 3"),
          list(method = "skip", args = list("type", "histogram"), label = "PC 2 & 3")
        )
      )
    )
  )

  payload <- payload_add_layout(payload, layout)
  payload
}



#' Add the main observations to the payload
#'
#' @param payload List containing data and layout attributes for a plotly graph
#' @param x Object from biplotEZ package
#' @param p_ly_pch Plotting characters of the observations
#' @param Col Colors of the observations to be plotted
#' @param visible Whether points should be visible on graph. True by default
#'
#' @return
#' @noRd updated payload
insert_Z_coo_payload <- function(payload, x, p_ly_pch, Col, visible = TRUE) {

  groups <- levels(x$group)
  num_groups <- length(groups)

  new_traces <- vector("list", num_groups)

  for (i in seq_len(num_groups)) {
    g <- groups[i]
    sel <- x$group == g

    new_traces[[i]] <- list(
      x = x$Z[sel, 1],
      y = x$Z[sel, 2],
      name = g,
      type = "scatter",
      mode = "markers",
      hovertext = hovertext_generator(x, i,"<br />"),
      hoverinfo = "text+name",
      customdata = (seq_len(x$n))[sel],
      meta = list("data"),
      xaxis = "x",
      yaxis = "y",
      visible = visible,
      marker = list(
        symbol = p_ly_pch[i],
        color = Col[i],
        opacity = 1
      ),
      legendgroup = "data",
      legendgrouptitle = list(text = "<b>Data</b>")
    )
  }

  payload <- payload_add_traces(payload, new_traces)
  payload
}

#' Add class means to the payload
#'
#' @param payload List containing data and layout attributes for a plotly graph
#' @param Z Coordinates of the class means to be added
#' @param symbol Plotting symbol of each class mean
#' @param color color of each class mean observation
#'
#' @return updated payload
#' @noRd
insert_class_means_payload <- function(payload, Z, symbol, color) {
  stopifnot(is.matrix(Z) || is.data.frame(Z))
  Z <- as.matrix(Z)

  n <- nrow(Z)
  traces <- vector("list", n)

  for (i in seq_len(n)) {
    traces[[i]] <- list(
      x = Z[i, 1],
      y = Z[i, 2],
      name = rownames(Z)[i] %||% paste0("ClassMean_", i),
      type = "scatter",
      mode = "markers",
      hovertext = "Class Mean",
      hoverinfo = "text+name",
      customdata = i - 1,              # keep your 0-based id
      meta = list("ClassMean"),
      xaxis = "x",
      yaxis = "y",
      visible = TRUE,
      showlegend = FALSE,
      marker = list(symbol = symbol[i], color = color[i]),
      legendgroup = "ClassMean"
    )
  }

  payload_add_traces(payload, traces)
}


#' Insert polygons to the payload
#'
#' @param payload List containing data and layout attributes for a plotly graph
#' @param coors coordinates of the polygons to be added wrapped in a named list
#' @param aes Aestethics of the polygon
#' @param leg_group Character indicating type of polygon
#'
#' @return updated payload
#' @noRd
insert_polygon_EZ_payload <- function(payload, coors, aes, leg_group = "Alpha Bags") {
  stopifnot(is.list(coors), length(coors) > 0)

  leg_name <- names(coors) %||% paste0("Group_", seq_along(coors))
  traces <- vector("list", length(coors))

  for (i in seq_along(coors)) {
    xy <- coors[[i]]
    xy <- as.matrix(xy)

    if (leg_group != "Alpha Bags") {
      Elip <- cluster::ellipsoidhull(xy)
      xy <- cluster::predict.ellipsoid(Elip, n.out = 101)
      xy <- as.matrix(xy)
    }

    # close the polygon
    xy <- rbind(xy, xy[1, , drop = FALSE])

    traces[[i]] <- list(
      x = xy[, 1],
      y = xy[, 2],
      mode = "lines",
      type = "scatter",
      line = list(color = aes$col[i], width = aes$lwd[i]),
      fill = "toself",
      fillcolor = grDevices::adjustcolor(aes$col[i], aes$opacity[i]),
      legendrank = 2000,
      name = leg_name[i],
      legendgroup = leg_group,
      legendgrouptitle = list(text = paste0("<b>", leg_group, "</b>")),
      visible = TRUE,
      meta = list("polygon"),
      xaxis = "x",
      yaxis = "y"
    )
  }

  payload_add_traces(payload, traces)
}


#' Insert axis predictivity graph to the payload
#'
#' @param payload List containing data and layout attributes for a plotly graph
#' @param x Object from biplotEZ package
#' @param EZ Legacy, keep TRUE
#'
#' @return updated payload
#' @noRd
InsertAxisDeets_payload <- function(payload, x, EZ = FALSE) {
  if (EZ) {
    pred_deets <- axis_predictivities_EZ(x)
    ColNames <- c(colnames(x$X), "Weighted mean = Quality")
  } else {
    pred_deets <- axis_predictivities(x)
    ColNames <- c(colnames(x$x), "Weighted mean = Quality")
  }

  p <- x$p
  n <- nrow(pred_deets)

  traces <- vector("list", n)

  for (i in seq_len(n)) {
    linetype <- if (i == n) "solid" else "dashdot"
    lwidth   <- if (i == n) 3 else 2

    traces[[i]] <- list(
      x = seq_len(p),
      y = as.numeric(pred_deets[i, ]),
      type = "scatter",
      mode = "lines+markers",
      line = list(dash = linetype, width = lwidth),
      xaxis = "x",
      yaxis = "y",
      hoverinfo = "skip",
      showlegend = TRUE,
      name = ColNames[i],
      visible = FALSE,
      meta = list("axis_pred"),
      legendgroup = "AxPred",
      legendgrouptitle = list(text = "<b> Axis Predictivity <b>")
    )
  }

  payload_add_traces(payload, traces)
}


#' Add vector representation to the payload
#'
#' @param payload List containing data and layout attributes for a plotly graph
#' @param PC12 First two PC's
#' @param PC13 Legacy. Keep null
#' @param PC23 Legacy. Keep null
#'
#' @return updated payload
#' @noRd
insert_vector_annots_payload <- function(payload, PC12, PC13 = NULL, PC23 = NULL) {

  make_vec_annots <- function(PC, meta = NULL, visible = FALSE, text = NULL) {
    stopifnot(!is.null(PC$V), ncol(PC$V) >= 2)
    p <- PC$p %||% nrow(PC$V)

    if (is.null(text)) text <- colnames(PC$x) %||% paste0("Var", seq_len(p))

    anns <- vector("list", p)
    for (i in seq_len(p)) {
      anns[[i]] <- list(
        x = 0,
        y = 0,
        ax = PC$V[i, 1],
        ay = PC$V[i, 2],
        xref = "x",
        yref = "y",
        axref = "x",
        ayref = "y",
        text = text[i],
        showarrow = TRUE,
        arrowside = "start",
        visible = visible,
        meta = meta
      )
    }
    anns
  }

  anns <- list()

  # PC12: keep your meta tag vecload (used by your JS)
  anns <- c(anns, make_vec_annots(PC12, meta = list("vecload"), visible = FALSE))

  # PC13 / PC23: your original code didn’t tag meta; keep consistent (or tag if you want)
  if (!is.null(PC13)) anns <- c(anns, make_vec_annots(PC13, meta = NULL, visible = FALSE))
  if (!is.null(PC23)) anns <- c(anns, make_vec_annots(PC23, meta = NULL, visible = FALSE))

  payload_add_layout(payload, list(annotations = anns))
}


#' Insert TDA to the payload
#'
#' @param payload List containing data and layout attributes for a plotly graph
#' @param z.axes Output from biplotEZ - list containing tick coordinates
#' @param x Object from biplotEZ
#' @param Z Coordinates of the datapoints on the biplot
#' @param group Factor vector indicating class membership of each observation
#' @param Col Factor variable indicating colors of each class
#' @param inflate Numeric - inflate the length of the densities
#'
#' @return updated payload
#' @noRd
add_TDA_payload <- function(payload, z.axes, x, Z, group, Col, inflate = 1) {
  `%||%` <- function(a, b) if (is.null(a)) b else a

  # ensure group is a factor (for levels / match)
  if (!is.factor(group)) group <- factor(group)

  r1 <- range(x$Z[, 1])
  r2 <- range(x$Z[, 2])
  len <- sqrt((r1[1] - r1[2])^2 + (r2[1] - r2[2])^2)
  dist <- len / 8

  # ellipse over all the data (used to determine shifting)
  bigElip <- cluster::ellipsoidhull(Z)
  bigElipcoords <- cluster::predict.ellipsoid(bigElip, n.out = 101)
  elipcoords <- bigElipcoords

  # slopes + quadrants
  quads <- get_quads_axes(z.axes)
  m <- sapply(z.axes, function(ax) ax[1, 2] / ax[1, 1])
  p <- length(m)

  # shorten then shift axes + densities
  endpoints <- shorten_axes(z.axes, elipcoords)

  cols_for_move <- colnames(x$x %||% x$X) %||% paste0("V", seq_len(p))

  shift <- MoveLines(
    elip = bigElipcoords,
    m = m,
    quadrant = quads,
    d = dist,
    initial_ends = endpoints,
    swop = FALSE,
    cols = cols_for_move
  )

  DensCoors <- MoveDensities(
    Z = Z,
    m = m,
    endpoints = shift$ends,
    dist = shift$ShiftDist,
    dinflation = inflate,
    group = group,
    densityargs = NULL
  )

  # ---- build traces + annotations ----
  traces <- list()
  annotations <- list()

  titles <- c("<b>Axes</b>", rep("", p - 1))
  visible_axes <- FALSE

  var_names <- colnames(x$X) %||% paste0("Var", seq_len(p))

  for (i in seq_len(p)) {
    ends <- shift$ends[[i]]
    ends <- as.matrix(ends)

    # which end has max tick label
    index2 <- which.max(ends[, 3])

    # choose arrow glyph based on quadrant (matches your original)
    lab2 <- if (quads[i] %in% c(1, 4)) "&#11166;" else "&#11164;"

    # axis line trace
    lg_title <- if (i == 1) list(text = titles[i]) else NULL

    traces[[length(traces) + 1]] <- list(
      x = ends[, 1],
      y = ends[, 2],
      type = "scatter",
      mode = "lines",
      line = list(color = x$axes$col[i], width = 1, simplify = FALSE),
      name = var_names[i],
      legendgroup = paste0("ExpAx", i),
      legendgrouptitle = lg_title,
      meta = "ExpAx",
      xaxis = "x",
      yaxis = "y",
      customdata = ends[, 3],
      hoverinfo = "name",
      visible = visible_axes
    )

    # tick label + tick mark annotations (one per tick)
    ang_deg <- -atan(m[i]) * 180 / pi
    xs1 <-  12 * sin(atan(m[i]))
    ys1 <- -12 * cos(atan(m[i]))
    xs2 <-  22 * sin(atan(m[i]))
    ys2 <- -22 * cos(atan(m[i]))

    for (k in seq_len(nrow(ends))) {
      # numeric label
      annotations[[length(annotations) + 1]] <- list(
        x = ends[k, 1],
        y = ends[k, 2],
        text = as.character(ends[k, 3]),
        showarrow = FALSE,
        textangle = ang_deg,
        visible = visible_axes,
        yshift = ys1,
        xshift = xs1,
        meta = "ExpAx",
        xref = "x",
        yref = "y",
        customdata = i,
        font = list(size = 10, color = x$axes$tick.label.col[i])
      )

      # tick mark
      annotations[[length(annotations) + 1]] <- list(
        x = ends[k, 1],
        y = ends[k, 2],
        text = "&#124;",
        showarrow = FALSE,
        textangle = ang_deg,
        visible = visible_axes,
        meta = "ExpAx",
        xref = "x",
        yref = "y",
        customdata = i,
        font = list(size = 8, color = x$axes$tick.col[i])
      )
    }

    # axis name near center
    annotations[[length(annotations) + 1]] <- list(
      x = mean(ends[, 1]),
      y = mean(ends[, 2]),
      text = paste0("<b>", var_names[i], "</b>"),
      showarrow = FALSE,
      textangle = ang_deg,
      visible = visible_axes,
      yshift = ys2,
      xshift = xs2,
      meta = "ExpAx",
      xref = "x",
      yref = "y",
      customdata = i,
      font = list(size = 12, color = "gray")
    )

    # arrow glyph at the max tick end
    annotations[[length(annotations) + 1]] <- list(
      x = ends[index2, 1],
      y = ends[index2, 2],
      text = lab2,
      showarrow = FALSE,
      textangle = ang_deg,
      visible = visible_axes,
      meta = "ExpAx",
      xref = "x",
      yref = "y",
      customdata = i,
      font = list(size = 18, color = x$axes$tick.label.col[i])
    )
  }

  # density traces (per group + per axis)
  group_levels <- levels(group)

  for (gi in seq_along(DensCoors)) {
    Dens <- as.matrix(DensCoors[[gi]])
    gname <- unique(as.character(group))[gi]
    gidx <- match(gname, group_levels)
    if (is.na(gidx)) gidx <- gi

    # legend entry (hidden initially; you can turn on later)
    traces[[length(traces) + 1]] <- list(
      x = 0, y = 0,
      type = "scatter",
      mode = "lines",
      line = list(dash = "dot", color = Col[gidx], width = 0.95),
      legendgroup = gname,
      showlegend = TRUE,
      name = gname,
      meta = "density",
      xaxis = "x",
      yaxis = "y",
      hoverinfo = "skip",
      customdata = "legendentry",
      visible = FALSE
    )

    # per-axis densities
    for (j in seq_len(p)) {
      traces[[length(traces) + 1]] <- list(
        x = Dens[, 2*j - 1],
        y = Dens[, 2*j],
        type = "scatter",
        mode = "lines",
        line = list(dash = "dot", color = Col[gidx], width = 0.95),
        legendgroup = gname,
        showlegend = FALSE,
        name = gname,
        meta = "density",
        xaxis = "x",
        yaxis = "y",
        hoverinfo = "skip",
        customdata = paste0("ExpAx", j),
        visible = visible_axes
      )
    }
  }

  payload <- payload_add_traces(payload, traces)
  payload <- payload_add_layout(payload, list(annotations = annotations))

  list(payload = payload, m = m, shift = shift, DensCoors = DensCoors)
}


#' Insert linear axes to the payload
#'
#' @param payload List containing data and layout attributes for a plotly graph
#' @param z.axes Output from biplotEZ - list containing tick coordinates
#' @param x Object from biplotEZ
#'
#' @return updated payload
#' @noRd
insert_linear_axes_payload <- function(payload, z.axes, x) {
  `%||%` <- function(a, b) if (is.null(a)) b else a

  p <- x$p
  radius <- max(abs(x$Z)) * 1.2
  theta <- seq(0, 2*pi, length.out = 200)
  elipcoords <- cbind(radius*cos(theta), radius*sin(theta))

  z.axes <- check_inside_circle(z.axes, radius, NULL)

  var_names <- colnames(x$X) %||% paste0("Var", seq_len(p))
  titles <- c("<b>Axes</b>", rep("", p - 1))

  grads <- numeric(p)
  traces <- list()
  annotations <- list()

  for (i in seq_len(p)) {
    AxName <- paste0("<b>", var_names[i], "</b>")

    endp <- z.axes[[i]][which.max(z.axes[[i]][, 3]), 1:2]
    pos <- "right"

    m <- (z.axes[[i]][2,2] - z.axes[[i]][1,2]) / (z.axes[[i]][2,1] - z.axes[[i]][1,1])
    grads[i] <- m

    angle <- atan(m)
    if (endp[1] < 0) {
      pos <- "left"
      angle <- angle - pi
    }

    # endpoints on circle for the axis line
    x_line <- c(radius*cos(atan(m)), radius*cos(atan(m) - pi))
    y_line <- c(radius*sin(atan(m)), radius*sin(atan(m) - pi))

    mat <- cbind(x_line, y_line)
    zhats <- obtain_zhat(mat, z.axes[[i]])

    lg_title <- if (i == 1) list(text = titles[i]) else NULL

    # ---- axis line trace ----
    traces[[length(traces) + 1]] <- list(
      x = x_line,
      y = y_line,
      type = "scatter",
      mode = "lines",
      line = list(color = x$axes$col[i], width = 1, simplify = FALSE),
      name = var_names[i],
      legendgroup = paste0("Ax", i),
      legendgrouptitle = lg_title,
      meta = list("axis"),
      xaxis = "x",
      yaxis = "y",
      customdata = zhats,
      visible = TRUE,
      hoverinfo = "name"
    )

    # ---- tick annotations ----
    tick_xy <- as.matrix(z.axes[[i]][, 1:2, drop = FALSE])
    tick_txt <- as.character(z.axes[[i]][, 3])

    ang_deg <- -atan(m) * 180 / pi
    yshift <- -12 * cos(atan(m))
    xshift <-  12 * sin(atan(m))

    for (k in seq_len(nrow(tick_xy))) {
      # numeric labels
      annotations[[length(annotations) + 1]] <- list(
        x = tick_xy[k, 1],
        y = tick_xy[k, 2],
        text = tick_txt[k],
        showarrow = FALSE,
        textangle = ang_deg,
        visible = TRUE,
        yshift = yshift,
        xshift = xshift,
        meta = list("Ax"),
        xref = "x",
        yref = "y",
        customdata = i,
        font = list(size = 10, color = x$axes$tick.label.col[i])
      )

      # tick marks
      annotations[[length(annotations) + 1]] <- list(
        x = tick_xy[k, 1],
        y = tick_xy[k, 2],
        text = "&#124;",
        showarrow = FALSE,
        textangle = ang_deg,
        visible = TRUE,
        meta = list("Ax"),
        xref = "x",
        yref = "y",
        customdata = i,
        font = list(size = 8, color = x$axes$tick.col[i])
      )
    }

    # ---- axis name text trace ----
    traces[[length(traces) + 1]] <- list(
      x = radius * cos(angle),
      y = radius * sin(angle),
      text = AxName,
      type = "scatter",
      mode = "text",
      textposition = pos,
      legendgroup = paste0("Ax", i),
      showlegend = FALSE,
      textfont = list(size = 12, color = "gray"),
      meta = "axis",
      xaxis = "x",
      yaxis = "y",
      visible = TRUE
    )
  }

  # ---- Outer circle trace ----
  traces[[length(traces) + 1]] <- list(
    x = elipcoords[, 1],
    y = elipcoords[, 2],
    type = "scatter",
    mode = "lines",
    line = list(color = "green", width = 0.6),
    name = "OuterCircle",
    showlegend = FALSE,
    meta = list("OuterCircle"),
    xaxis = "x",
    yaxis = "y",
    visible = TRUE,
    hoverinfo = "none"
  )

  payload <- payload_add_traces(payload, traces)
  payload <- payload_add_layout(payload, list(annotations = annotations))

  list(payload = payload, grads = grads, radius = radius)
}

#' Add unit circle to biplot
#'
#' @param payload List containing data and layout attributes for a plotly graph
#' @param n Number of datapoints in the circle
#' @param visible Indicator, whether to display or not. Set to false
#' @param color Color of the circle
#' @param width Linewidth of circle
#'
#' @return updated payload
#' @noRd
insert_unit_circle_payload <- function(payload, n = 200, visible = FALSE,
                                       color = "red", width = 1.2) {
  theta <- seq(0, 2*pi, length.out = n)

  trace <- list(
    x = cos(theta),
    y = sin(theta),
    type = "scatter",
    mode = "lines",
    line = list(color = color, width = width),
    name = "Unit Circle",
    showlegend = FALSE,
    meta = list("veccircle"),   # array form plays nicely with metaTag()
    xaxis = "x",
    yaxis = "y",
    hoverinfo = "name",
    visible = visible
  )

  payload_add_traces(payload, list(trace))
}
