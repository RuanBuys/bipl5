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


#' Plot a PCO biplot from the biplotEZ package
#'
#' @param x An object of class biplotEZ::biplot
#'
#' @return A plotly graph
#' @export
#' @method plot_bipl5 PCO
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
    # Test these two z.axes with one another. Are they the same? If not, why??
    # SSpecifically, is the coordinates of the tick marks per axis the same?
    #

    z.axes <- spsUtil::quiet(lapply(
      1:p,
      biplotEZ:::biplot.spline.axis,
      Z,
      x$X,
      means = x$means,
      sd = x$sd,
      n.int = ax.aes$ticks,
      spline.control = x$spline.control
    ))
    #z.axes <- biplotEZ::axes_coordinates(x)

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
        )

      if (length(idx) > 0) {
        p_ly <- p_ly |>
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
          )
      }

      p_ly <- p_ly |>
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

  return(p_ly)
}
