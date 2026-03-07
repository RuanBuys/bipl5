build_plotly <- function(widget) {
  plotly::plotly_build(widget)
}

trace_meta_tags <- function(traces) {
  vapply(traces, function(tr) {
    meta <- tr$meta
    if (is.null(meta)) {
      return(NA_character_)
    }
    if (is.list(meta)) {
      return(paste(unlist(meta), collapse = "|"))
    }
    paste(meta, collapse = "|")
  }, character(1))
}

pca_ez <- function(e.vects = c(1, 2), data = iris) {
  biplotEZ::biplot(data) |>
    biplotEZ::PCA(e.vects = e.vects)
}

prepared_pca_ez <- function(e.vects = c(1, 2), data = iris) {
  x <- pca_ez(e.vects = e.vects, data = data)
  if (is.null(x$samples)) {
    x <- biplotEZ::samples(x)
  }
  x <- biplotEZ::axes(x)
  x <- biplotEZ::fit.measures(x)
  if (isTRUE(x$scaled)) {
    x$X <- scale(x$X, center = FALSE, scale = 1 / x$sd)
  }
  if (isTRUE(x$center)) {
    x$X <- scale(x$X, -x$means, scale = FALSE)
  }
  x
}

wrapped_pca <- function(e.vects = c(1, 2), data = iris) {
  pca_ez(e.vects = e.vects, data = data) |>
    wrap_bipl5()
}

five_group_classes <- function() {
  factor(rep(letters[1:5], each = 30))
}

cva_ez <- function(e.vects = c(1, 2), classes = five_group_classes()) {
  biplotEZ::biplot(iris[, 1:4]) |>
    biplotEZ::CVA(classes = classes, e.vects = e.vects)
}

prepared_cva_ez <- function(e.vects = c(1, 2), classes = five_group_classes()) {
  x <- cva_ez(e.vects = e.vects, classes = classes)
  if (is.null(x$samples)) {
    x <- biplotEZ::samples(x)
  }
  x <- biplotEZ::axes(x)
  x <- biplotEZ::fit.measures(x)
  if (is.null(x$means.aes)) {
    x <- biplotEZ::means(x)
  }
  if (isTRUE(x$scaled)) {
    x$X <- scale(x$X, center = FALSE, scale = 1 / x$sd)
  }
  if (isTRUE(x$center)) {
    x$X <- scale(x$X, -x$means, scale = FALSE)
  }
  x
}

wrapped_cva <- function(e.vects = c(1, 2), classes = five_group_classes()) {
  cva_ez(e.vects = e.vects, classes = classes) |>
    wrap_bipl5()
}

pco_ez <- function(axes = c("regression", "splines"), e.vects = c(1, 2)) {
  biplotEZ::biplot(iris[, 1:4]) |>
    biplotEZ::PCO(
      dist.func = biplotEZ::sqrtManhattan,
      axes = axes,
      e.vects = e.vects
    )
}

legacy_pca_mock <- function(data = as.matrix(iris[, 1:4])) {
  pr <- prcomp(data, center = TRUE, scale. = TRUE)
  list(
    PCA = list(v = pr$rotation, d = pr$sdev),
    p = ncol(data),
    n = nrow(data),
    x = as.matrix(data)
  )
}

legacy_vector_input <- function(data = as.matrix(iris[, 1:4])) {
  pr <- prcomp(data, center = TRUE, scale. = TRUE)
  v <- pr$rotation[, 1:2, drop = FALSE]
  m <- v[, 2] / v[, 1]
  list(
    DisplQuality = "Quality of display = 100%",
    basis = c(1, 2),
    x = as.matrix(data),
    Z = pr$x[, 1:2, drop = FALSE],
    p = ncol(data),
    n = nrow(data),
    mu = colMeans(data),
    stddev = apply(data, 2, stats::sd),
    group = factor(rep("Data", nrow(data))),
    m = m,
    quads = bipl5:::getquad(v, m),
    V = v,
    PCA = list(v = pr$rotation, d = pr$sdev)
  )
}

simple_polygon_aes <- function(n) {
  list(
    col = rep(c("red", "blue", "green"), length.out = n),
    lwd = rep(1, n),
    opacity = rep(0.2, n)
  )
}

simple_polygons <- function() {
  list(
    GroupA = rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1)),
    GroupB = rbind(c(0, 0), c(-1, 0), c(-1, -1), c(0, -1))
  )
}
