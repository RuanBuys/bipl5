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

test_that("legacy fit helpers accept prcomp-style mock objects", {
  legacy <- legacy_pca_mock()

  axis_pred <- bipl5:::axis_predictivities(legacy)
  expect_equal(dim(axis_pred), c(legacy$p + 1, legacy$p))
  expect_identical(colnames(axis_pred), paste("Rank", 1:legacy$p))
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
