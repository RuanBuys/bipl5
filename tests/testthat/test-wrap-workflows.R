test_that("wrap_bipl5 validates dimensions and builds PCA mdsDisplay registries", {
  bad <- pca_ez()
  bad$dim.biplot <- 3
  expect_error(wrap_bipl5(bad), "only accepts biplots of two dimensions")

  bp <- wrapped_pca()
  expect_s3_class(bp, "bipl5_biplot")
  expect_s3_class(bp, "pca")
  expect_identical(names(bp$meta$pc_info), c("mdsDisplay_12", "mdsDisplay_13", "mdsDisplay_23"))
  expect_named(bp$fit_measures, c("CumPred", "CumAd", "VarExp", "Scree", "fit_table_12", "fit_table_13", "fit_table_23"))
  expect_equal(dim(bp$mdsDisplay_12$Data$sample_coordinates), c(150, 2))
})

test_that("wrap_bipl5.PCA preserves nonstandard user pair order", {
  bp <- biplotEZ::biplot(as.data.frame(state.x77)) |>
    biplotEZ::PCA(e.vects = c(4, 5)) |>
    wrap_bipl5()

  expect_identical(names(bp$meta$pc_info), c("mdsDisplay_45", "mdsDisplay_12", "mdsDisplay_13"))
  expect_identical(bp$meta$pc_info[[1]]$label, "PC 4 & 5")
})

test_that("wrap_bipl5.CVA builds CV registries and omits fit measures", {
  bp <- wrapped_cva()
  expect_s3_class(bp, "bipl5_biplot")
  expect_s3_class(bp, "cva")
  expect_null(bp$fit_measures)
  expect_identical(names(bp$meta$pc_info), c("mdsDisplay_12", "mdsDisplay_13", "mdsDisplay_23"))
  expect_identical(bp$meta$pc_info[[1]]$label, "CV 1 & 2")

  built <- build_plotly(plot(bp))
  expect_gt(length(built$x$data), 0)
})

test_that("plot.bipl5_biplot exposes mdsDisplays and fit measures to JS hooks", {
  bp <- wrapped_pca()
  widget <- plot(bp)
  hook_data <- widget$jsHooks$render[[1]]$data

  expect_identical(unname(hook_data$initialPCKey), "PC 1 & 2")
  expect_named(hook_data$mdsDisplays, c("PC 1 & 2", "PC 1 & 3", "PC 2 & 3"))
  expect_true(is.null(hook_data$mdsDisplays[["PC 1 & 2"]]$trace_data))
  expect_false(is.null(hook_data$mdsDisplays[["PC 1 & 3"]]$trace_data))
  expect_named(hook_data$fm_mdsDisplay, c("CumPred", "CumAd", "VarExp", "Scree"))

  built <- build_plotly(widget)
  expect_gt(length(built$x$data), 0)
  expect_gt(length(built$x$layout$annotations), 0)
})

test_that("subset_biplot preserves order and matching fit tables", {
  bp <- wrapped_pca()
  subsetted <- bipl5:::subset_biplot(bp, c("mdsDisplay_23", "mdsDisplay_12"))

  expect_identical(names(subsetted$meta$pc_info), c("mdsDisplay_23", "mdsDisplay_12"))
  expect_named(subsetted$fit_measures, c("CumPred", "CumAd", "VarExp", "Scree", "fit_table_23", "fit_table_12"))

  widget <- plot(subsetted)
  expect_identical(unname(widget$jsHooks$render[[1]]$data$initialPCKey), "PC 2 & 3")
  expect_error(
    bipl5:::subset_biplot(bp, "mdsDisplay_99"),
    "Unknown mdsDisplay\\(s\\): mdsDisplay_99"
  )
})

test_that("extract supports mdsDisplay subsets, two-level access, and path traversal", {
  bp <- wrapped_pca()

  only_12 <- extract(bp, mdsDisplay_12)
  expect_s3_class(only_12, "bipl5_biplot")
  expect_identical(names(only_12$meta$pc_info), "mdsDisplay_12")

  data_branch <- extract(bp, from = mdsDisplay_12, what = Data)
  expect_s3_class(data_branch, "bipl5_data")

  fit_label <- extract(bp, from = mdsDisplay_12, what = fit_qual)
  expect_match(fit_label, "Quality of display = ")

  fit_graph <- extract(bp, fit_measures, CumPred)
  expect_s3_class(fit_graph, "bipl5_fit")

  fit_graph_path <- extract(bp, fit_measures$CumPred)
  expect_s3_class(fit_graph_path, "bipl5_fit")

  samples <- extract(bp, mdsDisplay_12$Data$sample_coordinates)
  expect_equal(dim(samples), c(150, 2))

  expect_error(
    extract(bp, mdsDisplay_12$Data$missing_field),
    "Field 'missing_field' not found"
  )
  expect_error(
    extract(bp),
    "Provide either \\(from, what\\) or a single \\$ expression"
  )
})

test_that("plot.bipl5_fit reconstructs ggplot fit graphs from stored traces", {
  bp <- wrapped_pca()

  cum_pred_plot <- plot(extract(bp, fit_measures, CumPred))
  expect_s3_class(cum_pred_plot, "ggplot")
  expect_identical(
    cum_pred_plot$labels$title,
    paste(
      "Figure 1: Cumulative quality and axis predictivities",
      "across the subspace.",
      sep = "\n"
    )
  )
  expect_identical(cum_pred_plot$labels$colour, "Axis Predictivity")
  expect_identical(cum_pred_plot$labels$x, "Dimension of subspace")
  expect_identical(
    cum_pred_plot$labels$y,
    "Overall quality and axis predictivities (cumulative)"
  )
  expect_silent(ggplot2::ggplot_build(cum_pred_plot))

  cum_ad_plot <- plot(extract(bp, fit_measures, CumAd))
  expect_identical(
    cum_ad_plot$labels$title,
    paste(
      "Figure 2: Cumulative adequacy across dimensions",
      "of the subspace.",
      sep = "\n"
    )
  )
  expect_identical(cum_ad_plot$labels$colour, "Axis Adequacy")
  expect_identical(cum_ad_plot$labels$y, "Cumulative adequacy")
  expect_silent(ggplot2::ggplot_build(cum_ad_plot))

  var_exp_plot <- plot(extract(bp, fit_measures, VarExp))
  expect_identical(
    var_exp_plot$labels$title,
    paste(
      "Figure 4: Cumulative proportion of total variation",
      "explained across subspace dimensions.",
      sep = "\n"
    )
  )
  expect_identical(var_exp_plot$labels$fill, "Variance Contribution")
  expect_identical(
    var_exp_plot$labels$y,
    "Proportion of total variation (cumulative)"
  )
  expect_silent(ggplot2::ggplot_build(var_exp_plot))

  scree_plot <- plot(extract(bp, fit_measures, Scree))
  expect_identical(
    scree_plot$labels$title,
    paste(
      "Figure 3: Scree profile of eigenvalues across",
      "subspace dimensions.",
      sep = "\n"
    )
  )
  expect_identical(scree_plot$labels$y, "Scree profile (eigenvalues)")
  expect_silent(ggplot2::ggplot_build(scree_plot))
})

test_that("remove_mdsDisplay validates names and preserves at least one mdsDisplay", {
  bp <- wrapped_pca()
  dropped <- remove_mdsDisplay(bp, mdsDisplay_13)

  expect_identical(names(dropped$meta$pc_info), c("mdsDisplay_12", "mdsDisplay_23"))
  expect_false("fit_table_13" %in% names(dropped$fit_measures))

  expect_error(remove_mdsDisplay(bp, mdsDisplay_99), "is not a valid mdsDisplay name")

  only_12 <- extract(bp, mdsDisplay_12)
  expect_error(
    remove_mdsDisplay(only_12, mdsDisplay_12),
    "Cannot remove the last remaining mdsDisplay"
  )

  broken <- bp
  broken$mdsDisplay_13 <- NULL
  expect_error(
    remove_mdsDisplay(broken, mdsDisplay_13),
    "does not exist in this object"
  )
})

test_that("append_mdsDisplay validates input and extends PCA and CVA registries", {
  bp <- wrapped_pca()
  extended <- append_mdsDisplay(bp, c(4, 1))

  expect_identical(tail(names(extended$meta$pc_info), 1), "mdsDisplay_14")
  expect_identical(extended$meta$pc_info$mdsDisplay_14$label, "PC 1 & 4")
  expect_true("fit_table_14" %in% names(extended$fit_measures))

  expect_error(append_mdsDisplay(bp, 1), "numeric vector of length 2")
  expect_error(append_mdsDisplay(bp, c(2, 2)), "two different PC indices")
  expect_error(append_mdsDisplay(bp, c(1, 9)), "must be between 1 and")
  expect_error(append_mdsDisplay(bp, c(1, 2)), "already exists in this object")

  cva_bp <- wrapped_cva()
  cva_extended <- append_mdsDisplay(cva_bp, c(1, 4))
  expect_identical(tail(names(cva_extended$meta$pc_info), 1), "mdsDisplay_14")
  expect_null(cva_extended$fit_measures)
  expect_identical(cva_extended$meta$pc_info$mdsDisplay_14$label, "CV 1 & 4")
})

test_that("wrap_bipl5 on a PCO object produces a bipl5_biplot with pco class", {
  pco <- pco_ez()
  bp <- wrap_bipl5(pco)
  expect_s3_class(bp, "bipl5_biplot")
  expect_true("pco" %in% class(bp))
  expect_null(bp$fit_measures)
  expect_error(append_mdsDisplay(bp, c(1, 3)), "not supported")
  expect_error(remove_mdsDisplay(bp, mdsDisplay_12), "not supported")
})

test_that("wrap_bipl5 on a regression object exposes display quality", {
  reg <- regress_ez(non_orthogonal = TRUE)
  bp <- wrap_bipl5(reg)

  expect_s3_class(bp, "bipl5_biplot")
  expect_true("reg" %in% class(bp))
  expect_null(bp$fit_measures)
  expect_identical(names(bp$meta$pc_info), "mdsDisplay_12")
  expect_match(bp$mdsDisplay_12$fit_qual, "^R\\^2_disp = ")
  expect_match(bp$mdsDisplay_12$fit_qual, "R_1\\^2")
  expect_match(bp$mdsDisplay_12$fit_qual, "R_\\{2\\|1\\}\\^2")
  expect_identical(bp$meta$fit.quality, bp$mdsDisplay_12$fit_qual)
  expect_match(as.character(bp$meta$fit.quality.plotly), "R\\^2_\\{disp\\}")
  expect_identical(bp$mdsDisplay_12$mdsDisplay$layout$xaxis$title, bp$mdsDisplay_12$fit_qual)

  widget <- plot(bp)
  expect_match(as.character(widget$x$layoutAttrs[[1]]$xaxis$title), "R\\^2_\\{disp\\}")
  expect_true(any(vapply(
    widget$dependencies,
    function(dep) identical(dep$name, "mathjax") && identical(dep$script, "cdn.js"),
    logical(1)
  )))
})
