# draw_panel() only runs when the plot is actually rendered, so exercise it on a
# null device rather than letting the default device write an Rplots.pdf
gg_build_grob <- function(p) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  ggplot2::ggplotGrob(p)
}

gg_axis_layer_data <- function(p) {
  built <- ggplot2::ggplot_build(p)
  idx <- vapply(
    p$layers,
    function(l) inherits(l$geom, "GeomCalibratedAxis"),
    logical(1)
  )
  expect_true(any(idx))
  built$data[[which(idx)[1]]]
}

gg_panel_range <- function(p) {
  built <- ggplot2::ggplot_build(p)
  pp <- built$layout$panel_params[[1]]
  list(x = pp$x.range, y = pp$y.range)
}

test_that("wrap_bipl5_gg validates its input", {
  expect_error(
    wrap_bipl5_gg(iris),
    "must be a 'biplot' object"
  )

  bad <- pca_ez()
  bad$dim.biplot <- 3
  expect_error(
    wrap_bipl5_gg(bad),
    "only accepts biplots of two dimensions"
  )

  expect_error(
    wrap_bipl5_gg(biplotEZ::biplot(iris[, 1:4])),
    "Apply PCA\\(\\), CVA\\(\\), regress\\(\\) or PCO\\(\\) first"
  )
})

test_that("wrap_bipl5_gg builds a ggplot with a calibrated axis layer", {
  p <- wrap_bipl5_gg(pca_ez())

  expect_s3_class(p, "ggplot")
  expect_true(any(vapply(
    p$layers,
    function(l) inherits(l$geom, "GeomCalibratedAxis"),
    logical(1)
  )))
  # a biplot is only readable when both display dimensions share a scale
  expect_identical(p$coordinates$ratio, 1)
  expect_equal(
    p$coordinates$aspect(list(x.range = c(0, 2), y.range = c(0, 1))),
    0.5
  )
  expect_silent(ggplot2::ggplot_build(p))
  # draw_panel() runs only on render, so make sure the grobs really build
  expect_s3_class(gg_build_grob(p), "gtable")
})

test_that("the recovered calibration reproduces the biplotEZ axis coordinates", {
  ez <- biplotEZ::axes(pca_ez())
  z.axes <- biplotEZ::axes_coordinates(ez)

  for (ax in z.axes) {
    ax <- as.matrix(ax)
    cal <- bipl5:::gg_axis_calibration(ax)

    expect_false(is.null(cal))
    expect_equal(cal$x0 + ax[, 3] * cal$dxdv, ax[, 1], tolerance = 1e-8)
    expect_equal(cal$y0 + ax[, 3] * cal$dydv, ax[, 2], tolerance = 1e-8)
    expect_equal(cal$offset, c(0, 0), tolerance = 1e-8)
    expect_gt(cal$vstep, 0)
  }
})

test_that("gg_axis_calibration rejects degenerate axes", {
  expect_null(bipl5:::gg_axis_calibration(matrix(0, nrow = 1, ncol = 3)))
  expect_null(bipl5:::gg_axis_calibration(matrix(0, nrow = 4, ncol = 2)))
  # constant marker values carry no calibration
  expect_null(bipl5:::gg_axis_calibration(cbind(1:4, 1:4, rep(2, 4))))
  # a zero-length axis is degenerate even when the values vary
  expect_null(bipl5:::gg_axis_calibration(cbind(rep(0, 4), rep(0, 4), 1:4)))
})

test_that("calibrated axes never widen the plotting window", {
  ez <- biplotEZ::axes(pca_ez())
  z.axes <- biplotEZ::axes_coordinates(ez)
  axis_extent <- max(abs(do.call(rbind, lapply(z.axes, function(a) a[, 1:2]))))

  p <- wrap_bipl5_gg(ez)
  ranges <- gg_panel_range(p)

  # markers reach well beyond the samples, yet the window follows the samples
  expect_gt(axis_extent, max(abs(ez$Z)))
  expect_lt(max(abs(unlist(ranges))), axis_extent)
  expect_equal(ranges$x, range(ez$Z[, 1]) + c(-1, 1) * 0.05 * diff(range(ez$Z[, 1])), tolerance = 1e-6)
  expect_equal(ranges$y, range(ez$Z[, 2]) + c(-1, 1) * 0.05 * diff(range(ez$Z[, 2])), tolerance = 1e-6)

  # the layer drops its positional aesthetics so that scale training skips it
  layer_data <- gg_axis_layer_data(p)
  expect_false("x" %in% names(layer_data))
  expect_false("y" %in% names(layer_data))
  expect_true(all(c("x_t", "y_t") %in% names(layer_data)))
})

test_that("drawn markers lie on the biplotEZ calibration grid", {
  ez <- biplotEZ::axes(pca_ez())
  z.axes <- biplotEZ::axes_coordinates(ez)

  p <- wrap_bipl5_gg(ez)
  ranges <- gg_panel_range(p)
  layer_data <- bipl5:::gg_fill_axis_defaults(gg_axis_layer_data(p))
  marks <- bipl5:::gg_axis_mark_data(
    split(layer_data, layer_data$group),
    ranges = ranges,
    axis_type = "line",
    tick_extend = TRUE
  )

  expect_gt(nrow(marks), 0)

  for (g in unique(marks$group)) {
    sub <- marks[marks$group == g, , drop = FALSE]
    ax <- as.matrix(z.axes[[as.integer(as.character(g))]])
    cal <- bipl5:::gg_axis_calibration(ax)
    values <- as.numeric(sub$label)

    # every drawn marker sits exactly where the biplotEZ calibration puts it
    expect_equal(sub$x_t, cal$x0 + values * cal$dxdv, tolerance = 1e-8)
    expect_equal(sub$y_t, cal$y0 + values * cal$dydv, tolerance = 1e-8)

    # and its value belongs to the marker grid biplotEZ chose
    offsets <- (values - ax[1, 3]) / cal$vstep
    expect_equal(offsets, round(offsets), tolerance = 1e-8)

    # markers are confined to the plotting window
    expect_true(all(sub$x_t >= ranges$x[1] - 1e-8 & sub$x_t <= ranges$x[2] + 1e-8))
    expect_true(all(sub$y_t >= ranges$y[1] - 1e-8 & sub$y_t <= ranges$y[2] + 1e-8))
  }
})

test_that("tick_extend = FALSE keeps only the supplied markers", {
  ez <- biplotEZ::axes(pca_ez())
  z.axes <- biplotEZ::axes_coordinates(ez)
  supplied <- lapply(z.axes, function(a) as.matrix(a)[, 3])

  p <- wrap_bipl5_gg(ez, tick_extend = FALSE)
  ranges <- gg_panel_range(p)
  layer_data <- bipl5:::gg_fill_axis_defaults(gg_axis_layer_data(p))
  marks <- bipl5:::gg_axis_mark_data(
    split(layer_data, layer_data$group),
    ranges = ranges,
    axis_type = "line",
    tick_extend = FALSE
  )

  for (g in unique(marks$group)) {
    sub <- marks[marks$group == g, , drop = FALSE]
    values <- as.numeric(sub$label)
    expect_true(all(values %in% round(supplied[[as.integer(as.character(g))]], 10)))
  }
})

test_that("marker extension falls back to the supplied markers when it cannot run", {
  ez <- biplotEZ::axes(pca_ez())
  z.axes <- biplotEZ::axes_coordinates(ez)
  # a calibration so fine that spanning the window would need a runaway number
  # of markers
  z.axes[[1]][, 3] <- z.axes[[1]][, 3] * 1e-6

  frame <- bipl5:::calibrated_axis_frame(ez, z.axes = z.axes)$data
  frame <- bipl5:::gg_fill_axis_defaults(
    transform(frame, x_t = frame$x, y_t = frame$y)
  )
  marks <- bipl5:::gg_axis_mark_data(
    split(frame, frame$group),
    ranges = list(x = c(-3, 3), y = c(-3, 3)),
    axis_type = "line",
    tick_extend = TRUE
  )

  expect_gt(sum(marks$group == 1), 0)

  # an axis that misses the window contributes nothing
  far <- bipl5:::gg_axis_mark_data(
    split(frame, frame$group),
    ranges = list(x = c(500, 501), y = c(500, 501)),
    axis_type = "line",
    tick_extend = TRUE
  )
  expect_null(far)
})

test_that("axis aesthetics follow the arguments and can defer to biplotEZ", {
  ez <- pca_ez() |> biplotEZ::axes(col = "steelblue", tick.label.col = "firebrick")

  default_data <- gg_axis_layer_data(wrap_bipl5_gg(ez))
  expect_identical(unique(default_data$axis_colour), "black")
  expect_identical(unique(default_data$text_colour), "black")

  ez_data <- gg_axis_layer_data(wrap_bipl5_gg(
    ez,
    axis_colour = "biplotEZ",
    text_colour = "biplotEZ"
  ))
  expect_identical(unique(ez_data$axis_colour), "steelblue")
  expect_identical(unique(ez_data$text_colour), "firebrick")

  # tick and label colours default to the axis colour
  custom <- gg_axis_layer_data(wrap_bipl5_gg(ez, axis_colour = "darkgreen"))
  resolved <- bipl5:::gg_fill_axis_defaults(custom)
  expect_identical(unique(resolved$tick_colour), "darkgreen")
  expect_identical(unique(resolved$label_colour), "darkgreen")
})

test_that("axis subsets and marker density follow the biplotEZ axes() call", {
  subset_p <- wrap_bipl5_gg(biplotEZ::axes(pca_ez(), which = c(1, 3)))
  subset_data <- gg_axis_layer_data(subset_p)
  expect_identical(
    sort(unique(as.character(subset_data$axis_label))),
    sort(colnames(iris)[c(1, 3)])
  )

  coarse <- biplotEZ::axes_coordinates(biplotEZ::axes(pca_ez(), ticks = 3))
  fine <- biplotEZ::axes_coordinates(biplotEZ::axes(pca_ez(), ticks = 10))
  step <- function(z) stats::median(diff(sort(as.matrix(z)[, 3])))
  expect_gt(step(coarse[[1]]), step(fine[[1]]))
})

test_that("wrap_bipl5_gg renders every supported biplot family", {
  cva <- wrap_bipl5_gg(cva_ez())
  expect_s3_class(cva, "ggplot")
  expect_silent(ggplot2::ggplot_build(cva))
  expect_match(cva$labels$x, "^CV1 \\([0-9.]+%\\)$")

  reg <- wrap_bipl5_gg(regress_ez())
  expect_s3_class(reg, "ggplot")
  expect_silent(ggplot2::ggplot_build(reg))
  expect_match(reg$labels$x, "^Dim1")

  pco <- wrap_bipl5_gg(pco_ez(axes = "regression"))
  expect_s3_class(pco, "ggplot")
  expect_silent(ggplot2::ggplot_build(pco))
})

test_that("spline PCO biplots are drawn as curved axes", {
  ez <- suppressMessages(pco_ez(axes = "splines"))
  p <- suppressMessages(wrap_bipl5_gg(ez))

  axis_layer <- p$layers[[which(vapply(
    p$layers,
    function(l) inherits(l$geom, "GeomCalibratedAxis"),
    logical(1)
  ))[1]]]
  expect_identical(axis_layer$geom_params$axis_type, "curve")
  expect_false(axis_layer$geom_params$tick_extend)

  layer_data <- gg_axis_layer_data(p)
  # curve vertices outnumber the labelled markers, and each vertex carries its
  # own orientation
  expect_gt(nrow(layer_data), sum(layer_data$tick))
  expect_gt(length(unique(round(layer_data$angle, 4))), 1)
  expect_silent(ggplot2::ggplot_build(p))
  expect_s3_class(gg_build_grob(p), "gtable")
})

test_that("group aesthetics, class means and polygons reach the plot", {
  grouped <- wrap_bipl5_gg(
    biplotEZ::biplot(iris[, 1:4]) |> biplotEZ::PCA(group.aes = iris[, 5]),
    legend_title = "Species"
  )
  expect_identical(grouped$labels$colour, "Species")

  ungrouped <- wrap_bipl5_gg(pca_ez())
  expect_null(ungrouped$labels$colour)

  cva <- wrap_bipl5_gg(cva_ez())
  means <- bipl5:::class_mean_frame(biplotEZ::means(cva_ez()), NULL)
  expect_equal(nrow(means), 5)
  expect_true(all(c("mean_colour", "mean_shape", "mean_size") %in% names(means)))

  bagged <- suppressMessages(
    biplotEZ::biplot(iris[, 1:4]) |>
      biplotEZ::PCA(group.aes = iris[, 5]) |>
      biplotEZ::alpha.bags(alpha = 0.9)
  )
  p <- wrap_bipl5_gg(bagged)
  expect_true(any(vapply(
    p$layers,
    function(l) inherits(l$geom, "GeomPolygon"),
    logical(1)
  )))
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("bags and ellipses are drawn exactly as biplotEZ stores them", {
  ez <- suppressMessages(
    biplotEZ::biplot(iris[, 1:4], scale = TRUE) |>
      biplotEZ::PCA(group.aes = iris[, 5]) |>
      biplotEZ::alpha.bags(alpha = 0.9) |>
      biplotEZ::ellipses(kappa = 2)
  )

  # biplotEZ renders both with graphics::polygon() on the stored boundary, so
  # the ggplot2 layer must not refit or resample them
  for (field in c("alpha.bags", "conc.ellipses")) {
    aes_field <- if (field == "alpha.bags") "alpha.bag.aes" else "conc.ellipse.aes"
    frame <- bipl5:::polygon_frame(ez[[field]], ez[[aes_field]])
    expect_equal(nrow(frame), sum(vapply(ez[[field]], nrow, integer(1))))

    for (i in seq_along(ez[[field]])) {
      sub <- frame[frame$poly_group == i, , drop = FALSE]
      expect_equal(sub$x, unname(ez[[field]][[i]][, 1]))
      expect_equal(sub$y, unname(ez[[field]][[i]][, 2]))
      expect_identical(sub$poly_colour[1], ez[[aes_field]]$col[i])
      expect_identical(sub$poly_alpha[1], ez[[aes_field]]$opacity[i])
    }
  }

  # both sets of polygons reach the plot, under distinct grouping
  p <- wrap_bipl5_gg(ez)
  poly_layer <- p$layers[[which(vapply(
    p$layers, function(l) inherits(l$geom, "GeomPolygon"), logical(1)
  ))[1]]]
  expect_length(unique(poly_layer$data$poly_group), 6L)
  expect_silent(ggplot2::ggplot_build(p))
  expect_s3_class(gg_build_grob(p), "gtable")
})

test_that("display quality is reported as the plot caption", {
  p <- wrap_bipl5_gg(pca_ez())
  expect_match(p$labels$caption, "^Quality of display")

  expect_null(wrap_bipl5_gg(pca_ez(), caption = "")$labels$caption)
  expect_identical(
    wrap_bipl5_gg(pca_ez(), caption = "custom")$labels$caption,
    "custom"
  )
})

test_that("geom_calibrated_axis handles vertical and translated axes", {
  vertical <- data.frame(
    group = 1L,
    axis_label = "V",
    x = 0,
    y = c(-1, 0, 1),
    value = c(-1, 0, 1),
    label = c("-1", "0", "1"),
    angle = pi / 2,
    tick = TRUE,
    cal_x0 = 0,
    cal_y0 = 0,
    cal_dxdv = 0,
    cal_dydv = 1,
    cal_vref = 0,
    cal_vstep = 1,
    off_x = 0,
    off_y = 0
  )
  translated <- transform(
    vertical,
    group = 2L,
    axis_label = "W",
    x = 1.5,
    cal_x0 = 1.5,
    off_x = 1.5
  )

  df <- rbind(vertical, translated)
  p <- ggplot2::ggplot() +
    ggplot2::geom_point(
      data = data.frame(x = c(-2, 2), y = c(-2, 2)),
      mapping = ggplot2::aes(x = x, y = y)
    ) +
    geom_calibrated_axis(
      data = df,
      mapping = ggplot2::aes(
        x = x, y = y, group = group, angle = angle, label = label,
        axis_label = axis_label, value = value, tick = tick,
        cal_x0 = cal_x0, cal_y0 = cal_y0, cal_dxdv = cal_dxdv,
        cal_dydv = cal_dydv, cal_vref = cal_vref, cal_vstep = cal_vstep,
        off_x = off_x, off_y = off_y
      ),
      inherit.aes = FALSE
    ) +
    ggplot2::coord_equal()

  expect_silent(ggplot2::ggplot_build(p))
  expect_s3_class(gg_build_grob(p), "gtable")

  line_data <- bipl5:::gg_axis_line_data(
    split(bipl5:::gg_fill_axis_defaults(transform(df, x_t = df$x, y_t = df$y)), df$group)
  )
  expect_true(all(line_data$.vertical))
  expect_equal(line_data$xintercept, c(0, 1.5))
})

test_that("gg_line_window_span bounds a line by the plotting window", {
  span <- bipl5:::gg_line_window_span(c(0, 0), c(1, 0), c(-2, 3), c(-1, 1))
  expect_equal(span, c(-2, 3))

  # a vertical line inside the window is bounded by the y range only
  span <- bipl5:::gg_line_window_span(c(0.5, 0), c(0, 1), c(-2, 3), c(-1, 4))
  expect_equal(span, c(-1, 4))

  # a vertical line outside the window misses it entirely
  expect_null(
    bipl5:::gg_line_window_span(c(9, 0), c(0, 1), c(-2, 3), c(-1, 4))
  )
})

test_that("marker values are formatted from the marker spacing", {
  expect_identical(
    bipl5:::gg_format_tick_values(c(0, 0.1, 0.2, 0.30000000000000004), step = 0.1),
    c("0", "0.1", "0.2", "0.3")
  )
  expect_identical(
    bipl5:::gg_format_tick_values(c(-10, 0, 10), step = 10),
    c("-10", "0", "10")
  )
  expect_identical(bipl5:::gg_format_tick_values(numeric(0)), character(0))
})

test_that("axis angles fold into the half-open interval (-pi/2, pi/2]", {
  folded <- bipl5:::gg_fold_angle(c(0, pi / 4, 3 * pi / 4, pi, -pi / 3))
  expect_true(all(folded > -pi / 2 - 1e-8 & folded <= pi / 2 + 1e-8))
  expect_equal(folded[1], 0)
  expect_equal(folded[2], pi / 4)
  expect_equal(folded[3], -pi / 4)
  expect_equal(folded[4], 0)
})

test_that("curve angles track increasing variable values", {
  px <- c(0, 1, 2, 3)
  py <- c(0, 0, 0, 0)
  expect_equal(bipl5:::gg_curve_angles(px, py, c(0, 1, 2, 3)), rep(0, 4))

  # values decreasing along the stored order flip the tangent
  flipped <- bipl5:::gg_curve_angles(px, py, c(3, 2, 1, 0))
  expect_equal(cos(flipped), rep(-1, 4))
  expect_equal(sin(flipped), rep(0, 4), tolerance = 1e-12)
})
