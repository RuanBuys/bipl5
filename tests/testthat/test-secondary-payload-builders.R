test_that("payload scaffolding configures layout buttons and dropdowns", {
  payload <- bipl5:::plot_scaffolding_payload(
    bipl5:::payload_new(),
    dpquality = "Quality",
    basis = c(1, 2),
    PC_toggle = FALSE,
    ax_pred = FALSE,
    TDA = TRUE,
    vec_dis = FALSE
  )

  expect_identical(payload$layout$xaxis$title, "Quality")
  expect_false(payload$layout$updatemenus[[2]]$visible)
  expect_false(payload$layout$updatemenus[[1]]$buttons[[1]]$visible)
  expect_false(payload$layout$updatemenus[[1]]$buttons[[3]]$visible)
})

test_that("observation, class mean, polygon, and vector payload layers have stable tags", {
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

  payload <- bipl5:::payload_new()
  payload <- bipl5:::insert_Z_coo_payload(payload, obj, symbol, color)
  expect_length(payload$trace_data, nlevels(group))
  expect_true(all(trace_meta_tags(payload$trace_data) == "data"))

  zmeans <- do.call(rbind, lapply(levels(group), function(g) {
    colMeans(x$Z[group == g, , drop = FALSE])
  }))
  rownames(zmeans) <- levels(group)
  payload <- bipl5:::insert_class_means_payload(
    payload,
    Z = zmeans,
    symbol = bipl5:::pch_to_plotly(x$means.aes$pch),
    color = x$means.aes$col
  )
  class_mean_traces <- Filter(function(tr) identical(unlist(tr$meta), "ClassMean"), payload$trace_data)
  expect_length(class_mean_traces, nrow(zmeans))
  expect_true(is.list(class_mean_traces[[1]]$x))
  expect_true(is.list(class_mean_traces[[1]]$y))

  payload <- bipl5:::insert_polygon_EZ_payload(
    payload,
    coors = simple_polygons(),
    aes = simple_polygon_aes(2)
  )
  expect_true(any(trace_meta_tags(payload$trace_data) == "polygon"))

  ellipses <- bipl5:::insert_polygon_EZ_payload(
    bipl5:::payload_new(),
    coors = simple_polygons(),
    aes = simple_polygon_aes(2),
    leg_group = "Con. Ellipses"
  )
  expect_true(any(trace_meta_tags(ellipses$trace_data) == "polygon"))

  payload <- bipl5:::insert_vector_annots_payload(
    payload,
    list(V = x$Vr, x = x$X, p = x$p)
  )
  expect_length(payload$layout$annotations, x$p)
  expect_identical(unlist(payload$layout$annotations[[1]]$meta), "vecload")

  extra_ann <- bipl5:::insert_vector_annots_payload(
    bipl5:::payload_new(),
    list(V = x$Vr, x = x$X, p = x$p),
    PC13 = list(V = x$Vr, x = x$X, p = x$p),
    PC23 = list(V = x$Vr, x = x$X, p = x$p)
  )
  expect_length(extra_ann$layout$annotations, 3 * x$p)
})

test_that("fit panel payload builders return plotly-ready trace lists", {
  x <- prepared_pca_ez()

  adeq <- bipl5:::add_axis_adeq_payload(list(), x)[[1]]
  expect_length(adeq, x$p)
  expect_true(all(vapply(adeq, function(tr) identical(unlist(tr$meta), c("FitPanel", "Cum. Adequacy")), logical(1))))

  pred <- bipl5:::add_axis_pred_payload(list(), x)[[1]]
  expect_length(pred, x$p + 1)
  expect_true(any(vapply(pred, function(tr) tr$line$dash == "solid", logical(1))))

  var_exp <- bipl5:::add_prop_variance_payload(x)[[1]]
  expect_length(var_exp, x$p + 1)
  expect_identical(unlist(var_exp[[length(var_exp)]]$meta), c("FitPanel", "Variance Explained"))

  scree <- bipl5:::add_scree_payload(x)[[1]]
  expect_length(scree, 1)
  expect_identical(unlist(scree[[1]]$meta), c("FitPanel", "Scree Plot"))

  short_scree <- bipl5:::add_scree_payload(list(eigenvalues = 1:3, p = 4))[[1]]
  expect_identical(short_scree[[1]]$x, 1:3)
})

test_that("linear axes, TDA, fit table, and slider builders enrich payloads", {
  x <- prepared_pca_ez()
  z_axes <- biplotEZ::axes_coordinates(x)

  linear <- bipl5:::insert_linear_axes_payload(bipl5:::payload_new(), z_axes, x)
  expect_named(linear, c("payload", "grads", "radius"))
  expect_length(linear$grads, x$p)
  expect_true(any(trace_meta_tags(linear$payload$trace_data) == "OuterCircle"))
  expect_true(any(vapply(linear$payload$layout$annotations, function(ann) identical(unlist(ann$meta), "Ax"), logical(1))))

  unit <- bipl5:::insert_unit_circle_payload(linear$payload, visible = TRUE)
  expect_true(any(trace_meta_tags(unit$trace_data) == "veccircle"))

  tda <- bipl5:::add_TDA_payload(unit, z_axes, x, Z = x$Z, group = x$group.aes, Col = x$samples$col)
  expect_named(tda, c("payload", "m", "shift"))
  expect_length(tda$m, x$p)
  metas <- trace_meta_tags(tda$payload$trace_data)
  expect_true(any(metas == "ExpAx"))
  expect_true(any(metas == "density"))

  slider <- bipl5:::slider_control_payload(tda, n_inside = 17, n_outside = 4)
  expect_length(slider$payload$config$slider_info$slider_pos, x$p)
  expect_gt(slider$payload$config$slider_info$step_size, 0)

  table_payload <- bipl5:::add_table_payload(list(payload = list()), x)
  expect_length(table_payload$payload$fit_table, 1)
  expect_identical(
    unlist(table_payload$payload$fit_table[[1]]$meta),
    c("FitPanel", "Summary Table")
  )
})

test_that("build_one_payload composes a complete PCA and CVA payload", {
  x <- biplotEZ::means(prepared_pca_ez())
  x_ref <- x
  x_ref$alpha.bags <- simple_polygons()
  x_ref$alpha.bag.aes <- simple_polygon_aes(2)
  x_ref$conc.ellipses <- simple_polygons()
  x_ref$conc.ellipse.aes <- simple_polygon_aes(2)

  group <- x$group.aes
  color <- x$samples$col
  symbol <- bipl5:::pch_to_plotly(x$samples$pch)

  payl <- bipl5:::build_one_payload(
    ez_obj = x,
    group = group,
    color = color,
    symbol = symbol,
    x_ref = x_ref,
    include_polygons = TRUE
  )
  expect_s3_class(payl, "bipl5_payload")
  expect_s3_class(payl$Data, "bipl5_data")
  expect_true(any(trace_meta_tags(payl$payload$trace_data) == "polygon"))
  expect_true(any(trace_meta_tags(payl$payload$trace_data) == "veccircle"))

  cva <- prepared_cva_ez()
  cva_no_means <- cva
  cva_no_means$Zmeans <- NULL
  cva_ref <- cva
  cva_ref$means.aes <- NULL

  cva_payl <- bipl5:::build_one_payload(
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
  cva_metas <- trace_meta_tags(cva_payl$payload$trace_data)
  expect_false(any(cva_metas == "veccircle"))
  expect_true(any(cva_metas == "ClassMean"))
})
