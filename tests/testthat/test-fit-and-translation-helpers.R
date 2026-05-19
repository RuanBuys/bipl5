test_that("fit quality helpers produce numeric summaries for EZ objects", {
  x <- prepared_pca_ez()

  axis_pred <- bipl5:::axis_predictivities_EZ(x)
  expect_equal(dim(axis_pred), c(x$p + 1, x$p))
  expect_identical(rownames(axis_pred)[x$p + 1], "Overall Quality")
  expect_true(all(diff(axis_pred[x$p + 1, ]) >= 0))

  adequacies <- bipl5:::axis_adequacies(x)
  expect_equal(dim(adequacies), c(x$p, x$p))
  expect_true(all(adequacies[, 1] <= adequacies[, x$p]))

  marginal_pred <- bipl5:::marginal_predictivities_EZ(x)
  expect_length(marginal_pred, x$p)
  expect_true(all(marginal_pred >= 0))
  expect_true(all(marginal_pred <= 1))

  fit_label <- bipl5:::fit_quality(x$eigenvalues, x$e.vects)
  expect_match(fit_label, "^Quality of display = ")
  expect_match(fit_label, "PC1")
  expect_match(fit_label, "PC2")
})

test_that("regression fit quality decomposes non-orthogonal display coordinates", {
  X <- matrix(c(
    1, 2,
    0, 1,
    1, 0
  ), ncol = 2, byrow = TRUE)
  Z <- cbind(c(1, 0, 1), c(1, 1, 2))

  fit_comp <- bipl5:::regression_fit_components(X, Z)
  proj <- Z %*% solve(crossprod(Z), t(Z))

  expect_equal(fit_comp$overall_ss, sum(fit_comp$dim_ss), tolerance = 1e-10)
  expect_equal(fit_comp$overall_ss, sum((proj %*% X)^2), tolerance = 1e-10)

  fit_label <- bipl5:::regression_fit_quality(X, Z)
  expect_match(fit_label, "^R\\^2_disp = ")
  expect_match(fit_label, "R_1\\^2")
  expect_match(fit_label, "R_\\{2\\|1\\}\\^2")

  fit_label_tex <- bipl5:::regression_fit_quality_tex(X, Z)
  expect_match(as.character(fit_label_tex), "R\\^2_\\{disp\\}")
  expect_match(as.character(fit_label_tex), "R_\\{2 \\\\\\\\mid 1\\}\\^2")
})

test_that("obtain_xhat reconstructs or falls back to existing X", {
  x <- prepared_pca_ez()
  xhat <- bipl5:::obtain_xhat(x)
  expect_equal(dim(xhat), dim(x$X))

  fallback <- list(
    Lmat = matrix(1, nrow = 2, ncol = 3),
    Z = matrix(1, nrow = 1, ncol = 2),
    e.vects = c(1, 2),
    X = matrix(1:6, nrow = 2),
    scaled = FALSE,
    center = FALSE
  )
  expect_identical(bipl5:::obtain_xhat(fallback), fallback$X)
})

test_that("is_correlation distinguishes correlation and covariance biplots", {
  base <- biplotEZ::biplot(iris) |> biplotEZ::PCA()
  corr <- biplotEZ::biplot(iris) |> biplotEZ::PCA(correlation.biplot = TRUE)

  expect_false(bipl5:::is_correlation(base))
  expect_true(bipl5:::is_correlation(corr))
})

test_that("tickmark helpers shorten and filter axes sensibly", {
  x <- prepared_pca_ez()
  radius <- max(abs(x$Z)) * 1.2
  theta <- seq(0, 2 * pi, length.out = 101)
  ellipse <- cbind(radius * cos(theta), radius * sin(theta))
  grads <- x$Vr[, 2] / x$Vr[, 1]

  ticks <- bipl5:::tickmarks(
    ellip = ellipse,
    gradient = grads,
    p = x$p,
    V = x$Vr,
    mu = x$means,
    stddev = x$sd
  )
  expect_length(ticks, x$p)
  expect_true(all(vapply(ticks, ncol, integer(1)) == 3))

  filtered <- bipl5:::check_inside_circle(ticks, r = radius, thetas = atan(grads))
  expect_length(filtered, length(ticks))
  expect_true(all(vapply(filtered, nrow, integer(1)) <= vapply(ticks, nrow, integer(1))))

  shortened <- bipl5:::shorten_axes(ticks, ellipse)
  expect_length(shortened, x$p)
  expect_true(all(vapply(shortened, ncol, integer(1)) == 3))
})

test_that("linear-axis ticks extend evenly before later trimming", {
  ticks <- list(
    matrix(
      c(
        -1, 0, -1,
         0, 0,  0,
         1, 0,  1
      ),
      ncol = 3,
      byrow = TRUE
    )
  )

  x <- list(Z = matrix(c(-2, 0, 2, 0), ncol = 2, byrow = TRUE))
  prepared <- bipl5:::ensure_outside_circle(ticks, x)

  expect_equal(prepared[[1]][, 3], c(-3, -2, -1, 0, 1, 2, 3))
  expect_true(any(prepared[[1]][, 1]^2 + prepared[[1]][, 2]^2 > (max(abs(x$Z)) * 1.2)^2))
})

test_that("pretty thinning keeps a smaller subset of existing tick labels", {
  ticks <- list(
    cbind(seq(-10, 10, by = 1), 0, seq(-10, 10, by = 1))
  )

  thinned <- bipl5:::keep_pretty_axis_ticks(ticks, n = 8)

  expect_lt(nrow(thinned[[1]]), nrow(ticks[[1]]))
  expect_true(all(thinned[[1]][, 3] %in% pretty(ticks[[1]][, 3], n = 8)))
})

test_that("shorten_axes trims to existing calibrated ticks", {
  x <- prepared_pca_ez()
  z_axes <- biplotEZ::axes_coordinates(x)
  theta <- seq(0, 2 * pi, length.out = 101)
  ellipse <- cbind(3 * cos(theta), 2 * sin(theta))

  shortened <- bipl5:::shorten_axes(z_axes, ellipse)

  for (i in seq_along(z_axes)) {
    expect_true(all(shortened[[i]][, 3] %in% z_axes[[i]][, 3]))

    original_idx <- match(shortened[[i]][, 3], z_axes[[i]][, 3])
    expect_false(anyNA(original_idx))
    expect_equal(
      shortened[[i]][, 1:2, drop = FALSE],
      z_axes[[i]][original_idx, 1:2, drop = FALSE],
      tolerance = 1e-10
    )
  }
})

test_that("translation helpers return shifted endpoints and densities", {
  x <- prepared_pca_ez()
  z_axes <- biplotEZ::axes_coordinates(x)
  theta <- seq(0, 2 * pi, length.out = 101)
  ellipse <- cbind(3 * cos(theta), 2 * sin(theta))
  quads <- bipl5:::get_quads_axes(z_axes)
  m <- vapply(z_axes, function(ax) ax[1, 2] / ax[1, 1], numeric(1))
  endpoints <- bipl5:::shorten_axes(z_axes, ellipse)

  moved <- bipl5:::MoveLines(
    elip = ellipse,
    m = m,
    quadrant = quads,
    d = 0.2,
    initial_ends = endpoints,
    swop = FALSE,
    cols = colnames(x$X)
  )
  expect_named(moved, c("ShiftDist", "ends", "Axes"))
  expect_length(moved$ShiftDist, length(m))
  expect_length(moved$ends, length(m))

  densities <- bipl5:::MoveDensities(
    Z = x$Z,
    m = m,
    endpoints = moved$ends,
    dist = moved$ShiftDist,
    dinflation = 1,
    group = x$group.aes
  )
  expect_length(densities, nlevels(x$group.aes))
  expect_true(all(vapply(densities, ncol, integer(1)) == 2 * length(m)))

  translated <- bipl5:::translate(
    elip = ellipse,
    quadrant = 1,
    other = matrix(c(0, 0, 0.2, 0.1), ncol = 2, byrow = TRUE),
    d = 0.1,
    endpoints = matrix(c(-0.5, 0, 0.5, 0), ncol = 2, byrow = TRUE),
    theta = 0,
    swop = FALSE
  )
  expect_named(translated, c("distance", "ends"))
  expect_equal(dim(translated$ends), c(2, 2))
})

test_that("dynamic density inflation keeps translated density heights bounded", {
  x <- prepared_pca_ez()
  z_axes <- biplotEZ::axes_coordinates(x)
  r1 <- range(x$Z[, 1])
  r2 <- range(x$Z[, 2])
  len <- sqrt((r1[1] - r1[2])^2 + (r2[1] - r2[2])^2)
  d <- len / 8
  ellipse <- cluster::predict.ellipsoid(cluster::ellipsoidhull(x$Z), n.out = 101)

  m <- vapply(z_axes, function(ax) ax[1, 2] / ax[1, 1], numeric(1))
  endpoints <- bipl5:::shorten_axes(z_axes, ellipse)
  moved <- bipl5:::MoveLines(
    elip = ellipse,
    m = m,
    quadrant = bipl5:::get_quads_axes(z_axes),
    d = d,
    initial_ends = endpoints,
    swop = FALSE,
    cols = colnames(x$X)
  )

  inflate <- bipl5:::compute_density_inflation(
    Z = x$Z,
    m = m,
    endpoints = moved$ends,
    group = x$group.aes,
    target_height = d
  )
  expect_length(inflate, length(m))
  expect_true(all(is.finite(inflate)))
  expect_true(all(inflate > 0))

  densities <- bipl5:::MoveDensities(
    Z = x$Z,
    m = m,
    endpoints = moved$ends,
    dist = moved$ShiftDist,
    dinflation = inflate,
    group = x$group.aes
  )

  for (i in seq_along(m)) {
    peak_height <- vapply(densities, function(curve) {
      rotated <- curve[, (2 * i - 1):(2 * i), drop = FALSE] %*%
        bipl5:::RotationConstructor(atan(m[i]))
      max(rotated[, 2] - moved$ShiftDist[i])
    }, numeric(1))

    expect_equal(max(peak_height), d, tolerance = 1e-8)
  }
})
