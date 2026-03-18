trace_has_meta <- function(tr, key) {
  meta <- tr$meta
  if (is.null(meta)) {
    return(FALSE)
  }
  if (is.character(meta)) {
    return(key %in% meta)
  }
  key %in% unlist(meta, use.names = FALSE)
}

sample_traces_for <- function(bp, mds_name = "mdsDisplay_12") {
  traces <- bp[[mds_name]]$mdsDisplay$trace_data
  traces[vapply(traces, trace_has_meta, logical(1), key = "data")]
}

sample_legend_traces_for <- function(bp, mds_name = "mdsDisplay_12", kind = NULL) {
  traces <- bp[[mds_name]]$mdsDisplay$trace_data
  out <- traces[vapply(traces, trace_has_meta, logical(1), key = "sample-legend")]
  if (is.null(kind)) {
    return(out)
  }
  out[vapply(out, trace_has_meta, logical(1), key = kind)]
}

density_traces_for <- function(bp, mds_name = "mdsDisplay_12") {
  traces <- bp[[mds_name]]$mdsDisplay$trace_data
  traces[vapply(traces, trace_has_meta, logical(1), key = "density")]
}

test_that("format_samples splits PCA sample traces by a stored bare column name", {
  bp <- init_biplot(iris) |>
    scale_mds(type = "pca", eigenvectors = c(1, 2))

  original_samples <- sample_traces_for(bp)
  expect_length(original_samples, 1)
  expect_identical(original_samples[[1]]$legendgrouptitle$text, "<b>Data</b>")

  formatted <- format_samples(
    bp,
    stratify = "col",
    by = Species,
    col = c("red", "blue", "green")
  )

  sample_traces <- sample_traces_for(formatted)

  expect_length(sample_traces, 3)
  expect_identical(
    vapply(sample_traces, `[[`, character(1), "name"),
    levels(iris$Species)
  )
  expect_true(all(vapply(sample_traces, function(tr) identical(tr$legendgroup, "data"), logical(1))))
  expect_identical(sample_traces[[1]]$legendgrouptitle$text, "<b>Species</b>")
  expect_identical(unname(formatted$meta$color), c("red", "blue", "green"))
  expect_identical(levels(formatted$meta$group), levels(iris$Species))
  expect_identical(levels(formatted$meta$x$group.aes), levels(iris$Species))
  expect_identical(formatted$meta$sample_format$color$legend_title, "Species")
  expect_null(formatted$meta$sample_format$symbol)
  expect_length(sample_legend_traces_for(formatted), 0)
})

test_that("format_samples still accepts a supplied grouping vector", {
  bp <- init_biplot(iris) |>
    scale_mds(type = "pca", eigenvectors = c(1, 2))

  formatted <- format_samples(
    bp,
    stratify = "col",
    by = iris$Species,
    col = c("red", "blue", "green")
  )

  sample_traces <- sample_traces_for(formatted)

  expect_identical(levels(formatted$meta$group), levels(iris$Species))
  expect_length(sample_traces, 3)
  expect_identical(sample_traces[[1]]$legendgrouptitle$text, "<b>Data</b>")
})

test_that("format_samples still accepts a quoted stored column name", {
  bp <- init_biplot(iris) |>
    scale_mds(type = "pca", eigenvectors = c(1, 2))

  formatted <- format_samples(
    bp,
    stratify = "col",
    by = "Species",
    col = c("red", "blue", "green")
  )

  sample_traces <- sample_traces_for(formatted)

  expect_identical(levels(formatted$meta$group), levels(iris$Species))
  expect_identical(sample_traces[[1]]$legendgrouptitle$text, "<b>Species</b>")
})

test_that("format_samples keeps a unified legend when symbols reuse the current grouping", {
  bp <- init_biplot(iris) |>
    scale_mds(type = "pca", eigenvectors = c(1, 2)) |>
    format_samples(
      stratify = "col",
      by = Species,
      col = c("red", "blue", "green")
    )

  reformatted <- format_samples(
    bp,
    stratify = "symbol",
    pch = c(16, 17, 15)
  )

  sample_traces <- sample_traces_for(reformatted)

  expect_length(sample_traces, 3)
  expect_length(sample_legend_traces_for(reformatted), 0)
  expect_identical(sample_traces[[1]]$legendgrouptitle$text, "<b>Species</b>")
  expect_identical(
    vapply(sample_traces, function(tr) tr$marker$color, character(1)),
    c("red", "blue", "green")
  )
  expect_identical(
    vapply(sample_traces, function(tr) tr$marker$symbol, character(1)),
    pch_to_plotly(c(16L, 17L, 15L))
  )
  expect_identical(unname(reformatted$meta$color), c("red", "blue", "green"))
})

test_that("format_samples metadata is reused by append_mdsDisplay", {
  bp <- init_biplot(iris) |>
    scale_mds(type = "pca", eigenvectors = c(1, 2)) |>
    format_samples(
      stratify = "col",
      by = Species,
      col = c("red", "blue", "green")
    )

  extended <- append_mdsDisplay(bp, c(1, 3))
  sample_traces <- sample_traces_for(extended, "mdsDisplay_13")

  expect_length(sample_traces, 3)
  expect_identical(
    vapply(sample_traces, `[[`, character(1), "name"),
    levels(iris$Species)
  )
  expect_identical(sample_traces[[1]]$legendgrouptitle$text, "<b>Species</b>")
})

test_that("format_samples updates all existing mdsDisplays", {
  bp <- init_biplot(iris) |>
    scale_mds(type = "pca", eigenvectors = c(1, 2)) |>
    append_mdsDisplay(c(1, 3))

  formatted <- format_samples(
    bp,
    stratify = "col",
    by = Species,
    col = c("red", "blue", "green")
  )

  for (nm in names(formatted$meta$pc_info)) {
    sample_traces <- sample_traces_for(formatted, nm)

    expect_length(sample_traces, 3)
    expect_identical(
      vapply(sample_traces, `[[`, character(1), "name"),
      levels(iris$Species)
    )
    expect_identical(sample_traces[[1]]$legendgrouptitle$text, "<b>Species</b>")
  }
})

test_that("format_samples preserves fitted CVA classes while reformatting samples", {
  bp <- init_biplot(iris) |>
    scale_mds(
      type = "cva",
      classes = five_group_classes(),
      eigenvectors = c(1, 2)
    )

  model_group <- bp$meta$x$group.aes

  formatted <- format_samples(
    bp,
    stratify = "col",
    by = Species,
    col = c("red", "blue", "green")
  )

  sample_traces <- sample_traces_for(formatted)
  mean_traces <- formatted$mdsDisplay_12$mdsDisplay$trace_data[
    vapply(
      formatted$mdsDisplay_12$mdsDisplay$trace_data,
      trace_has_meta,
      logical(1),
      key = "ClassMean"
    )
  ]

  expect_length(sample_traces, 3)
  expect_length(mean_traces, length(levels(model_group)))
  expect_identical(formatted$meta$x$group.aes, model_group)
  expect_identical(levels(formatted$meta$group), levels(iris$Species))
})

test_that("color stratification rebuilds translated-axis densities by color group", {
  bp <- init_biplot(iris) |>
    scale_mds(type = "pca", eigenvectors = c(1, 2))

  original_density <- density_traces_for(bp)
  expect_length(original_density, 5)
  expect_identical(unique(vapply(original_density, `[[`, character(1), "legendgroup")), "Data")

  formatted <- format_samples(
    bp,
    stratify = "col",
    by = Species,
    col = c("red", "blue", "green")
  )

  density_traces <- density_traces_for(formatted)

  expect_length(density_traces, 15)
  expect_identical(
    unique(vapply(density_traces, `[[`, character(1), "legendgroup")),
    levels(iris$Species)
  )
})

test_that("symbol-only stratification leaves translated-axis densities unchanged", {
  bp <- init_biplot(iris) |>
    scale_mds(type = "pca", eigenvectors = c(1, 2)) |>
    format_samples(
      stratify = "symbol",
      by = Species,
      pch = c(16, 17, 15)
    )

  density_traces <- density_traces_for(bp)

  expect_length(density_traces, 5)
  expect_identical(unique(vapply(density_traces, `[[`, character(1), "legendgroup")), "Data")
})

test_that("adding color after symbol stratification rebuilds translated-axis densities", {
  bp <- init_biplot(iris) |>
    scale_mds(type = "pca", eigenvectors = c(1, 2)) |>
    format_samples(
      stratify = "symbol",
      by = Species,
      pch = c(16, 17, 15)
    ) |>
    format_samples(
      stratify = "col",
      by = Species,
      col = c("red", "blue", "green")
    )

  density_traces <- density_traces_for(bp)

  expect_length(density_traces, 15)
  expect_identical(
    unique(vapply(density_traces, `[[`, character(1), "legendgroup")),
    levels(iris$Species)
  )
})

test_that("format_samples builds dual legend sections for different stratifying variables", {
  iris2 <- iris
  iris2$Band <- factor(
    rep(paste0("class", 1:4), length.out = nrow(iris2)),
    levels = paste0("class", 1:4)
  )

  formatted <- init_biplot(iris2) |>
    scale_mds(type = "pca", eigenvectors = c(1, 2)) |>
    format_samples(
      stratify = "col",
      by = Species,
      col = c("tomato", "steelblue", "darkgreen")
    ) |>
    format_samples(
      stratify = "symbol",
      by = Band,
      pch = c(12, 13, 14, 15)
    )

  sample_traces <- sample_traces_for(formatted)
  color_legends <- sample_legend_traces_for(formatted, kind = "color")
  symbol_legends <- sample_legend_traces_for(formatted, kind = "symbol")

  expect_length(sample_traces, 12)
  expect_true(all(vapply(sample_traces, trace_has_meta, logical(1), key = "sample-combo")))
  expect_true(all(vapply(sample_traces, function(tr) identical(tr$showlegend, FALSE), logical(1))))

  expect_length(color_legends, 3)
  expect_length(symbol_legends, 4)
  expect_identical(
    vapply(color_legends, `[[`, character(1), "name"),
    levels(iris2$Species)
  )
  expect_identical(
    vapply(symbol_legends, `[[`, character(1), "name"),
    levels(iris2$Band)
  )
  expect_identical(unique(vapply(color_legends, `[[`, character(1), "legendgroup")), "sample-legend-color")
  expect_identical(unique(vapply(symbol_legends, `[[`, character(1), "legendgroup")), "sample-legend-symbol")
  expect_identical(color_legends[[1]]$legendgrouptitle$text, "<b>Species</b>")
  expect_identical(symbol_legends[[1]]$legendgrouptitle$text, "<b>Band</b>")
  expect_true(all(vapply(symbol_legends, function(tr) identical(tr$marker$color, "black"), logical(1))))
  expect_identical(
    vapply(symbol_legends, function(tr) tr$marker$symbol, character(1)),
    pch_to_plotly(c(12L, 13L, 14L, 15L))
  )

  expect_identical(formatted$meta$sample_format$order, c("color", "symbol"))
  expect_identical(formatted$meta$sample_format$color$legend_title, "Species")
  expect_identical(formatted$meta$sample_format$symbol$legend_title, "Band")
  expect_identical(levels(formatted$meta$group), levels(iris2$Species))

  density_traces <- density_traces_for(formatted)
  expect_length(density_traces, 15)
  expect_identical(
    unique(vapply(density_traces, `[[`, character(1), "legendgroup")),
    levels(iris2$Species)
  )
})

test_that("dual sample formatting is reused by append_mdsDisplay", {
  iris2 <- iris
  iris2$Band <- factor(
    rep(paste0("class", 1:4), length.out = nrow(iris2)),
    levels = paste0("class", 1:4)
  )

  formatted <- init_biplot(iris2) |>
    scale_mds(type = "pca", eigenvectors = c(1, 2)) |>
    format_samples(
      stratify = "col",
      by = Species,
      col = c("tomato", "steelblue", "darkgreen")
    ) |>
    format_samples(
      stratify = "symbol",
      by = Band,
      pch = c(12, 13, 14, 15)
    ) |>
    append_mdsDisplay(c(1, 3))

  for (nm in names(formatted$meta$pc_info)) {
    sample_traces <- sample_traces_for(formatted, nm)
    color_legends <- sample_legend_traces_for(formatted, nm, kind = "color")
    symbol_legends <- sample_legend_traces_for(formatted, nm, kind = "symbol")
    density_traces <- density_traces_for(formatted, nm)

    expect_length(sample_traces, 12)
    expect_length(color_legends, 3)
    expect_length(symbol_legends, 4)
    expect_length(density_traces, 15)
  }
})

test_that("format_samples errors cleanly when a column name is unavailable", {
  bp <- init_biplot(iris[, 1:4]) |>
    scale_mds(type = "pca", eigenvectors = c(1, 2))

  expect_error(
    format_samples(bp, stratify = "col", by = Species),
    "dataset passed to init_biplot"
  )
})
