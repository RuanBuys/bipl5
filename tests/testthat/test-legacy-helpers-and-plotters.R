test_that("plot scaffolding and dependency helpers return configured widgets", {
  p <- bipl5:::plot_scaffolding(
    dpquality = "Quality",
    basis = c(1, 2),
    PC_toggle = TRUE,
    ax_pred = TRUE,
    TDA = TRUE,
    vec_dis = TRUE,
    x_colnames = colnames(iris)[1:4]
  )

  layout <- p$x$layoutAttrs[[1]]
  expect_s3_class(p, "plotly")
  expect_length(layout$updatemenus, 4)
  expect_identical(layout$updatemenus[[2]]$name, "PC_toggle")
  expect_identical(layout$updatemenus[[3]]$name, "Fit_toggle")
  expect_identical(layout$updatemenus[[4]]$name, "Slider_toggle")

  dep <- bipl5:::bipl5_dependency()
  expect_s3_class(dep, "html_dependency")
  expect_identical(dep$name, "bipl5-plotly")
  expect_true("bipl5_plotly.js" %in% dep$script)

  widget <- bipl5:::insert_linear_js_v1(
    plotly::plot_ly(),
    p = 4,
    cols = c("red", "blue"),
    payload = list("PC 1 & 2" = list(config = list())),
    fm_payload = list(Scree = list()),
    initial_pc_key = "PC 1 & 2"
  )
  expect_true(any(vapply(widget$dependencies, function(x) identical(x$name, "bipl5-plotly"), logical(1))))
  expect_identical(widget$jsHooks$render[[1]]$data$initialPCKey, "PC 1 & 2")
  expect_identical(widget$jsHooks$render[[1]]$data$p, 4)
})

test_that("legacy plotly layer helpers build traces and annotations", {
  x <- biplotEZ::means(prepared_pca_ez())
  group <- x$group.aes
  symbol <- bipl5:::pch_to_plotly(x$samples$pch)
  color <- x$samples$col
  Xhat <- bipl5:::obtain_xhat(x)
  obj <- list(
    Z = x$Z,
    group = group,
    n = x$n,
    x = as.matrix(x$X),
    XHat = Xhat,
    sample.predictivity = x$sample.predictivity
  )

  p <- plotly::plot_ly()
  p <- bipl5:::insert_Z_coo(p, obj, symbol, color, TRUE)
  p <- bipl5:::insert_unit_circle(p, visible = TRUE)
  zmeans <- do.call(rbind, lapply(levels(group), function(g) {
    colMeans(x$Z[group == g, , drop = FALSE])
  }))
  rownames(zmeans) <- levels(group)
  p <- bipl5:::insert_class_means(
    p,
    zmeans,
    bipl5:::pch_to_plotly(x$means.aes$pch),
    x$means.aes$col
  )
  p <- bipl5:::insert_polygon_EZ(
    p,
    coors = simple_polygons(),
    aes = simple_polygon_aes(2)
  )
  p <- bipl5:::insert_polygon_EZ(
    p,
    coors = simple_polygons(),
    aes = simple_polygon_aes(2),
    leg_group = "Con. Ellipses"
  )
  metas <- trace_meta_tags(p$x$attrs)
  expect_true(any(metas == "data"))
  expect_true(any(metas == "veccircle"))
  expect_true(any(metas == "ClassMean"))
  expect_true(any(metas == "polygon"))
})

test_that("legacy hovertext, axes, fit tables, and JS helpers remain callable", {
  legacy <- legacy_pca_mock()
  vec_in <- legacy_vector_input()

  hover_obj <- list(
    x = vec_in$x,
    XHat = vec_in$x,
    group = vec_in$group,
    n = vec_in$n,
    sample.predictivity = rep(0.5, vec_in$n)
  )
  hover <- bipl5:::hovertext_generator(hover_obj, i = 1, linebreak = "<br>")
  expect_length(hover, vec_in$n)
  expect_match(hover[[1]], "Sample predictivity:")

  p <- bipl5:::plot_scaffolding("Quality", c(1, 2), x_colnames = colnames(vec_in$x))
  p <- bipl5:::InsertAxisDeets(p, legacy)
  fit_tables <- bipl5:::InsertFitMeasures(p, legacy)
  expect_length(fit_tables, 3)
  expect_equal(dim(fit_tables[[2]]), c(3, legacy$p))
  expect_equal(dim(fit_tables[[3]]), c(3, legacy$p))

  z_axes <- biplotEZ::axes_coordinates(prepared_pca_ez())
  p2 <- plotly::plot_ly()
  p2 <- bipl5:::insert_vector_annots(p2, list(V = vec_in$V, x = vec_in$x, p = vec_in$p), NULL, NULL)
  p2 <- bipl5:::insert_linear_js(p2, Xhat = vec_in$x, p = vec_in$p, m = vec_in$m, cols = colnames(vec_in$x))
  p2 <- bipl5:::insert_spline_js(p2, p = vec_in$p)
  expect_true(!is.null(p2$jsHooks$render))

  linear <- bipl5:::insert_linear_axes(z_axes, prepared_pca_ez(), plotly::plot_ly())
  expect_length(linear, 2)
  expect_s3_class(linear[[1]], "plotly")

  tda <- bipl5:::add_TDA(
    z.axes = z_axes,
    x = prepared_pca_ez(),
    p_ly = plotly::plot_ly(),
    Z = prepared_pca_ez()$Z,
    group = prepared_pca_ez()$group.aes,
    Col = prepared_pca_ez()$samples$col
  )
  expect_named(tda, c("p_ly", "m", "shift"))
})

test_that("legacy vector helpers accept handcrafted PCA input", {
  vec_in <- legacy_vector_input()

  p <- bipl5:::make_biplot_EZ(vec_in)
  expect_length(p, 3)
  expect_s3_class(p[[1]], "plotly")

  avb <- bipl5:::add_vector_biplot(
    p_ly = plotly::plot_ly(),
    x = vec_in,
    symbol = "circle",
    color = "red",
    visible = TRUE
  )
  expect_length(avb, 5)
  expect_s3_class(avb[[1]], "plotly")
  expect_equal(dim(avb[[2]]), c(vec_in$p, 2))
  expect_equal(dim(avb[[3]]), c(vec_in$n, vec_in$p))
})

test_that("plot_bipl5 methods cover PCA, CVA, and PCO variants", {
  pca_plot <- plot_bipl5(pca_ez())
  expect_s3_class(pca_plot, "plotly")
  expect_gt(length(build_plotly(pca_plot)$x$data), 0)

  cva_plot <- plot_bipl5(cva_ez(classes = factor(rep(letters[1:3], each = 50))))
  expect_s3_class(cva_plot, "plotly")
  expect_gt(length(build_plotly(cva_plot)$x$data), 0)

  pco_reg <- plot_bipl5(pco_ez(axes = "regression"))
  expect_s3_class(pco_reg, "plotly")
  expect_gt(length(build_plotly(pco_reg)$x$data), 0)

  pco_spline <- plot_bipl5(pco_ez(axes = "splines"))
  expect_s3_class(pco_spline, "plotly")
  expect_gt(length(build_plotly(pco_spline)$x$data), 0)
})

test_that("plot_bipl5 generic validates dimensions and augments PCO class", {
  bad <- pca_ez()
  bad$dim.biplot <- 3
  expect_error(plot_bipl5(bad), "only accepts biplots of two dimensions")

  pco <- pco_ez()
  class(pco) <- "biplot"
  expect_s3_class(plot_bipl5(pco), "plotly")
})
