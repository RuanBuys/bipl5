test_that("mdsDisplay scaffolding configures layout buttons and dropdowns", {
  mdsDisplay <- bipl5:::plot_scaffolding_mdsDisplay(
    bipl5:::mdsDisplay_new(),
    dpquality = "Quality",
    basis = c(1, 2),
    PC_toggle = FALSE,
    ax_pred = FALSE,
    TDA = TRUE,
    vec_dis = FALSE
  )

  expect_identical(mdsDisplay$layout$xaxis$title, "Quality")
  expect_false(mdsDisplay$layout$updatemenus[[2]]$visible)
  expect_false(mdsDisplay$layout$updatemenus[[1]]$buttons[[1]]$visible)
  expect_false(mdsDisplay$layout$updatemenus[[1]]$buttons[[3]]$visible)
})

test_that("observation, class mean, polygon, and vector mdsDisplay layers have stable tags", {
  x <- biplotEZ::means(prepared_pca_ez())
  group <- x$group.aes
  color <- x$samples$col
  symbol <- bipl5:::pch_to_plotly(x$samples$pch)
  Xhat <- bipl5:::obtain_xhat(x)

  obj <- list(
    Z = x$Z,
    group = group,
    n = x$n,
    x = as.matrix(x$X),
    XHat = Xhat,
    sample.predictivity = x$sample.predictivity
  )

  mdsDisplay <- bipl5:::mdsDisplay_new()
  mdsDisplay <- bipl5:::insert_Z_coo_mdsDisplay(mdsDisplay, obj, symbol, color)
  expect_length(mdsDisplay$trace_data, nlevels(group))
  expect_true(all(trace_meta_tags(mdsDisplay$trace_data) == "data"))

  zmeans <- do.call(rbind, lapply(levels(group), function(g) {
    colMeans(x$Z[group == g, , drop = FALSE])
  }))
  rownames(zmeans) <- levels(group)
  mdsDisplay <- bipl5:::insert_class_means_mdsDisplay(
    mdsDisplay,
    Z = zmeans,
    symbol = bipl5:::pch_to_plotly(x$means.aes$pch),
    color = x$means.aes$col
  )
  class_mean_traces <- Filter(function(tr) identical(unlist(tr$meta), "ClassMean"), mdsDisplay$trace_data)
  expect_length(class_mean_traces, nrow(zmeans))
  expect_true(is.list(class_mean_traces[[1]]$x))
  expect_true(is.list(class_mean_traces[[1]]$y))

  mdsDisplay <- bipl5:::insert_polygon_EZ_mdsDisplay(
    mdsDisplay,
    coors = simple_polygons(),
    aes = simple_polygon_aes(2)
  )
  expect_true(any(trace_meta_tags(mdsDisplay$trace_data) == "polygon"))

  ellipses <- bipl5:::insert_polygon_EZ_mdsDisplay(
    bipl5:::mdsDisplay_new(),
    coors = simple_polygons(),
    aes = simple_polygon_aes(2),
    leg_group = "Con. Ellipses"
  )
  expect_true(any(trace_meta_tags(ellipses$trace_data) == "polygon"))

  mdsDisplay <- bipl5:::insert_vector_annots_mdsDisplay(
    mdsDisplay,
    list(V = x$Vr, x = x$X, p = x$p, Z = x$Z)
  )
  alpha <- max(sqrt(x$Z[, 1]^2 + x$Z[, 2]^2))
  expect_length(mdsDisplay$layout$annotations, x$p)
  expect_identical(unlist(mdsDisplay$layout$annotations[[1]]$meta), "vecload")
  expect_equal(mdsDisplay$layout$annotations[[1]]$ax, alpha * x$Vr[1, 1])
  expect_equal(mdsDisplay$layout$annotations[[1]]$ay, alpha * x$Vr[1, 2])

  extra_ann <- bipl5:::insert_vector_annots_mdsDisplay(
    bipl5:::mdsDisplay_new(),
    list(V = x$Vr, x = x$X, p = x$p, Z = x$Z),
    PC13 = list(V = x$Vr, x = x$X, p = x$p, Z = x$Z),
    PC23 = list(V = x$Vr, x = x$X, p = x$p, Z = x$Z)
  )
  expect_length(extra_ann$layout$annotations, 3 * x$p)
})

test_that("fit panel mdsDisplay builders return plotly-ready trace lists", {
  x <- prepared_pca_ez()

  adeq <- bipl5:::add_axis_adeq_mdsDisplay(list(), x)[[1]]
  expect_length(adeq, x$p)
  expect_true(all(vapply(adeq, function(tr) identical(unlist(tr$meta), c("FitPanel", "Cum. Adequacy")), logical(1))))

  pred <- bipl5:::add_axis_pred_mdsDisplay(list(), x)[[1]]
  expect_length(pred, x$p + 1)
  expect_true(any(vapply(pred, function(tr) tr$line$dash == "solid", logical(1))))

  var_exp <- bipl5:::add_prop_variance_mdsDisplay(x)[[1]]
  expect_length(var_exp, x$p + 1)
  expect_identical(unlist(var_exp[[length(var_exp)]]$meta), c("FitPanel", "Variance Explained"))

  scree <- bipl5:::add_scree_mdsDisplay(x)[[1]]
  expect_length(scree, 1)
  expect_identical(unlist(scree[[1]]$meta), c("FitPanel", "Scree Plot"))

  short_scree <- bipl5:::add_scree_mdsDisplay(list(eigenvalues = 1:3, p = 4))[[1]]
  expect_identical(short_scree[[1]]$x, 1:3)
})

test_that("near-zero axis ticks are replaced with exact zero", {
  z_axes <- list(
    cbind(1:3, 4:6, c(-1e-17, 0.25, 1e-18)),
    cbind(1:2, 3:4, c(-0.5, 1e-13))
  )

  cleaned <- bipl5:::zero_to_near_zero(z_axes)

  expect_identical(cleaned[[1]][1, 3], 0)
  expect_identical(cleaned[[1]][2, 3], 0.25)
  expect_identical(cleaned[[1]][3, 3], 0)
  expect_identical(cleaned[[2]][1, 3], -0.5)
  expect_identical(cleaned[[2]][2, 3], 0)
})

test_that("linear axes, TDA, fit table, and slider builders enrich mdsDisplays", {
  x <- prepared_pca_ez()
  z_axes <- biplotEZ::axes_coordinates(x)

  linear <- bipl5:::insert_linear_axes_mdsDisplay(bipl5:::mdsDisplay_new(), z_axes, x)
  expect_named(linear, c("mdsDisplay", "grads", "radius"))
  expect_length(linear$grads, x$p)
  expect_true(any(trace_meta_tags(linear$mdsDisplay$trace_data) == "OuterCircle"))
  expect_true(any(vapply(linear$mdsDisplay$layout$annotations, function(ann) identical(unlist(ann$meta), "Ax"), logical(1))))

  tda <- bipl5:::add_TDA_mdsDisplay(
    linear$mdsDisplay,
    z_axes,
    x,
    Z = x$Z,
    group = x$group.aes,
    Col = x$samples$col
  )
  expect_named(tda, c("mdsDisplay", "m", "shift"))
  expect_length(tda$m, x$p)
  metas <- trace_meta_tags(tda$mdsDisplay$trace_data)
  expect_true(any(metas == "ExpAx"))
  expect_true(any(metas == "density"))

  slider <- bipl5:::slider_control_mdsDisplay(tda, n_inside = 17, n_outside = 4)
  expect_length(slider$mdsDisplay$config$slider_info$slider_pos, x$p)
  expect_gt(slider$mdsDisplay$config$slider_info$step_size, 0)

  table_mdsDisplay <- bipl5:::add_table_mdsDisplay(list(mdsDisplay = list()), x)
  expect_length(table_mdsDisplay$mdsDisplay$fit_table, 1)
  expect_identical(
    unlist(table_mdsDisplay$mdsDisplay$fit_table[[1]]$meta),
    c("FitPanel", "Summary Table")
  )
})

test_that("build_one_mdsDisplay composes a complete PCA and CVA mdsDisplay", {
  x <- biplotEZ::means(prepared_pca_ez())
  x_ref <- x
  x_ref$alpha.bags <- simple_polygons()
  x_ref$alpha.bag.aes <- simple_polygon_aes(2)
  x_ref$conc.ellipses <- simple_polygons()
  x_ref$conc.ellipse.aes <- simple_polygon_aes(2)

  group <- x$group.aes
  color <- x$samples$col
  symbol <- bipl5:::pch_to_plotly(x$samples$pch)

  payl <- bipl5:::build_one_mdsDisplay(
    ez_obj = x,
    group = group,
    color = color,
    symbol = symbol,
    x_ref = x_ref,
    include_polygons = TRUE
  )
  expect_s3_class(payl, "bipl5_mdsDisplay")
  expect_s3_class(payl$Data, "bipl5_data")
  expect_true(any(trace_meta_tags(payl$mdsDisplay$trace_data) == "polygon"))
  expect_true(any(vapply(
    payl$mdsDisplay$layout$annotations,
    function(ann) identical(unlist(ann$meta), "vecload"),
    logical(1)
  )))

  cva <- prepared_cva_ez()
  cva_no_means <- cva
  cva_no_means$Zmeans <- NULL
  cva_ref <- cva
  cva_ref$means.aes <- NULL

  cva_payl <- bipl5:::build_one_mdsDisplay(
    ez_obj = cva_no_means,
    group = cva$group.aes,
    color = cva$samples$col,
    symbol = bipl5:::pch_to_plotly(cva$samples$pch),
    x_ref = cva_ref,
    include_polygons = FALSE,
    dim_prefix = "CV",
    ax_pred = FALSE,
    vec_dis = FALSE
  )
  cva_metas <- trace_meta_tags(cva_payl$mdsDisplay$trace_data)
  expect_true(any(cva_metas == "ClassMean"))
  expect_false(any(vapply(
    cva_payl$mdsDisplay$layout$annotations,
    function(ann) identical(unlist(ann$meta), "vecload"),
    logical(1)
  )))
})
