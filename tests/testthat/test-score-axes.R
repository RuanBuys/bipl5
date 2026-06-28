data_trace_hover <- function(bp, nm = names(bp$meta$pc_info)[1]) {
  traces <- bp[[nm]]$mdsDisplay$trace_data
  data_traces <- Filter(function(tr) "data" %in% unlist(tr$meta), traces)
  data_traces[[1]]$hovertext
}

test_that("score_axes adds an Error column to every mdsDisplay", {
  bp <- init_biplot(iris[, 1:4], scale = TRUE) |>
    scale_mds(type = "pca")
  bp <- append_mdsDisplay(bp, eigenvectors = c(1, 3))

  scored <- score_axes(bp)

  expect_s3_class(scored, "bipl5_biplot")
  expect_true(isTRUE(scored$meta$reading_errors))
  for (nm in names(scored$meta$pc_info)) {
    expect_match(data_trace_hover(scored, nm)[1], "Error")
  }
})

test_that("the default build does not compute reading errors", {
  bp <- init_biplot(iris[, 1:4], scale = TRUE) |>
    scale_mds(type = "pca")

  expect_false(grepl("Error", data_trace_hover(bp)[1]))
  expect_null(bp$meta$reading_errors)
})

test_that("scoring preserves the existing Actual and Pred columns", {
  bp <- init_biplot(iris[, 1:4], scale = TRUE) |>
    scale_mds(type = "pca")
  scored <- score_axes(bp)

  strip_error_column <- function(hover) {
    lines <- strsplit(hover, "<br />", fixed = TRUE)[[1]]
    lines <- vapply(lines, function(l) {
      if (grepl("^\\|", l)) sub("[^|]*\\|$", "", l) else l
    }, character(1))
    paste(lines, collapse = "<br />")
  }

  base_hover <- data_trace_hover(bp)
  scored_hover <- data_trace_hover(scored)
  expect_identical(strip_error_column(scored_hover[1]), base_hover[1])
})

test_that("the reading error matches the Alves formula", {
  bp <- init_biplot(iris[, 1:4], scale = TRUE) |>
    scale_mds(type = "pca")
  scored <- score_axes(bp)

  ez <- scored$meta$x
  nm <- names(scored$meta$pc_info)[1]
  Z <- scored[[nm]]$Data$sample_coordinates
  z_axes <- scored[[nm]]$Data$axes_coordinates
  pred <- direct_reading_values(Z, as.matrix(ez$X), z_axes)

  expected <- 100 * abs(as.matrix(ez$X)[1, ] - pred[1, ]) / ez$sd
  hover <- data_trace_hover(scored)[1]
  reported <- as.numeric(sub("%.*", "", regmatches(
    hover,
    gregexpr("[0-9.]+%", hover)
  )[[1]]))

  expect_equal(reported, round(unname(expected), 2), tolerance = 1e-6)
})

test_that("unscaled biplots use a scaling constant of one", {
  scored <- init_biplot(iris[, 1:4], scale = FALSE) |>
    scale_mds(type = "pca") |>
    score_axes()

  ez <- scored$meta$x
  nm <- names(scored$meta$pc_info)[1]
  Z <- scored[[nm]]$Data$sample_coordinates
  z_axes <- scored[[nm]]$Data$axes_coordinates
  pred <- direct_reading_values(Z, as.matrix(ez$X), z_axes)

  expected <- 100 * abs(as.matrix(ez$X)[1, ] - pred[1, ])
  hover <- data_trace_hover(scored)[1]
  reported <- as.numeric(sub("%.*", "", regmatches(
    hover,
    gregexpr("[0-9.]+%", hover)
  )[[1]]))

  expect_equal(reported, round(unname(expected), 2), tolerance = 1e-6)
})

test_that("score_axes works for CVA and PCO biplots", {
  cva <- init_biplot(iris[, 1:4]) |>
    scale_mds(type = "cva", classes = five_group_classes()) |>
    score_axes()
  expect_match(data_trace_hover(cva)[1], "Error")

  pco <- init_biplot(iris[, 1:4]) |>
    scale_mds(type = "pco") |>
    score_axes()
  expect_match(data_trace_hover(pco)[1], "Error")
})

test_that("score_axes leaves spline biplots unchanged with a warning", {
  spline_bp <- init_biplot(iris[, 1:4]) |>
    scale_mds(type = "pco", axes = "splines")

  expect_warning(scored <- score_axes(spline_bp), "spline")
  expect_false(grepl("Error", data_trace_hover(scored)[1]))
})

test_that("score_axes rejects objects that are not bipl5_biplots", {
  expect_error(score_axes(list()), "bipl5_biplot")
})
