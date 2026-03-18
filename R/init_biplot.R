#' Create a bipl5 specification object
#'
#' `init_biplot()` stores the raw data and preprocessing options needed to
#' construct a biplot later with [scale_mds()]. It does not perform any
#' ordination itself. When `data` is a data frame containing both numeric and
#' non-numeric columns, only the numeric columns are used for the biplot
#' calculation, while the full data frame is retained for later formatting
#' steps such as [format_samples()].
#'
#' @param data A matrix or data frame. If a data frame contains non-numeric
#'   columns, they are stored but excluded from the ordination input.
#' @param center Logical; should numeric variables be centered before analysis?
#' @param scale Logical; should numeric variables be scaled before analysis?
#'
#' @return An object of class `bipl5_spec`.
#' @export
init_biplot <- function(data, center = TRUE, scale = FALSE) {
  new_bipl5_spec(data = data, center = center, scale = scale)
}


#' Scale a biplot specification into a bipl5_biplot
#'
#' `scale_mds()` turns a `bipl5_spec` created by [init_biplot()] into a fully
#' formed `bipl5_biplot` by dispatching to one of the underlying
#' [biplotEZ::PCA()], [biplotEZ::CVA()], [biplotEZ::PCO()], or
#' [biplotEZ::regress()] methods and then compiling only the requested
#' `mdsDisplay`. Any additional displays can be added later with
#' [append_mdsDisplay()].
#'
#' The `type` argument chooses the underlying biplot method. Additional
#' arguments are method-specific and should be supplied via `...`.
#'
#' Supported aliases in `...`:
#'
#' - Common: `classes`, `group_aes` / `group.aes`, `title` / `Title`
#' - PCA: `dimensions` / `dim.biplot`, `eigenvectors` / `e.vects`,
#'   `show_class_means` / `show.class.means` / `show_group_means` /
#'   `show.group.means`, `correlation_biplot` / `correlation.biplot`
#' - CVA: `classes`, `dimensions` / `dim.biplot`, `eigenvectors` / `e.vects`,
#'   `weighted_cva` / `weightedCVA`, `show_class_means` / `show.class.means` /
#'   `show_group_means` / `show.group.means`, `low_dim` / `low.dim`
#' - PCO: `Dmat` / `dist_mat`, `dist_func` / `dist.func`,
#'   `dist_func_cat` / `dist.func.cat`, `dimensions` / `dim.biplot`,
#'   `eigenvectors` / `e.vects`, `show_class_means` / `show.class.means` /
#'   `show_group_means` / `show.group.means`, `axes`
#' - `regress`: `Z` / `z`, `show_group_means` / `show.group.means` /
#'   `show_class_means` / `show.class.means`, `axes`
#'
#' For `type = "pco"`, any remaining named arguments in `...` are forwarded to
#' the chosen distance function.
#'
#' @param x A `bipl5_spec` created by [init_biplot()].
#' @param type The biplot method to construct. One of `"pca"`, `"cva"`,
#'   `"pco"`, `"regress"`, or `"regression"`.
#' @param ... Additional named arguments for the chosen method.
#'
#' @return A fully formed `bipl5_biplot`.
#' @export
scale_mds <- function(x, type = c("pca", "cva", "pco", "regress"), ...) {
  UseMethod("scale_mds")
}

#' @rdname scale_mds
#' @export
scale_mds.bipl5_spec <- function(x, type = c("pca", "cva", "pco", "regress"), ...) {
  type <- normalize_mds_type(type)
  dots <- list(...)
  validate_scale_mds_dots(dots)

  base <- build_base_biplot_from_spec(x, dots)
  bp <- base$bp
  dots <- base$dots

  built <- switch(
    type,
    pca = scale_mds_build_pca(bp, dots),
    cva = scale_mds_build_cva(bp, dots, classes = base$common$classes),
    pco = scale_mds_build_pco(bp, dots),
    regress = scale_mds_build_regress(bp, dots)
  )

  out <- switch(
    type,
    pca = scale_mds_compile_pca_biplot(built$ez_obj),
    cva = scale_mds_compile_cva_biplot(built$ez_obj),
    pco = scale_mds_compile_pco_biplot(built$ez_obj),
    regress = scale_mds_compile_regress_biplot(built$ez_obj)
  )
  out$meta$spec <- x
  out$meta$scale_mds <- list(
    type = type,
    common = base$common,
    args = built$args
  )
  out
}

#' @noRd
scale_mds.default <- function(x, type = c("pca", "cva", "pco", "regress"), ...) {
  stop(
    "scale_mds() expects a bipl5_spec created by init_biplot().",
    call. = FALSE
  )
}


#' Create a bipl5_spec object
#'
#' @param data A matrix or data frame.
#' @param center Logical scalar.
#' @param scale Logical scalar.
#'
#' @return An object of class `bipl5_spec`
#' @noRd
new_bipl5_spec <- function(data, center = TRUE, scale = FALSE) {
  if (!is.matrix(data) && !is.data.frame(data)) {
    stop("data must be a matrix or data frame.", call. = FALSE)
  }
  if (!is.logical(center) || length(center) != 1 || is.na(center)) {
    stop("center must be either TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(scale) || length(scale) != 1 || is.na(scale)) {
    stop("scale must be either TRUE or FALSE.", call. = FALSE)
  }

  processed <- init_biplot_prepare_data(data)

  obj <- list(
    data = processed$data,
    analysis_data = processed$analysis_data,
    numeric_columns = processed$numeric_columns,
    center = center,
    scale = scale
  )
  class(obj) <- "bipl5_spec"
  obj
}


#' @noRd
init_biplot_prepare_data <- function(data) {
  if (is.matrix(data)) {
    if (!is.numeric(data)) {
      stop("matrix inputs to init_biplot() must be numeric.", call. = FALSE)
    }

    data_df <- as.data.frame(data)
    return(list(
      data = data_df,
      analysis_data = data_df,
      numeric_columns = colnames(data_df)
    ))
  }

  data_df <- as.data.frame(data)
  numeric_cols <- vapply(data_df, is.numeric, logical(1))

  if (!any(numeric_cols)) {
    stop(
      "init_biplot() requires at least one numeric column for the biplot calculation.",
      call. = FALSE
    )
  }

  list(
    data = data_df,
    analysis_data = data_df[, numeric_cols, drop = FALSE],
    numeric_columns = names(data_df)[numeric_cols]
  )
}


#' @noRd
normalize_mds_type <- function(type) {
  if (length(type) == 0 || is.na(type[1])) {
    stop("scale_mds() requires a non-empty 'type'.", call. = FALSE)
  }
  type <- tolower(type[1])
  switch(
    type,
    pca = "pca",
    cva = "cva",
    pco = "pco",
    reg = "regress",
    regress = "regress",
    regression = "regress",
    stop(
      "Unsupported type '",
      type,
      "'. Use one of: 'pca', 'cva', 'pco', 'regress'.",
      call. = FALSE
    )
  )
}

#' @noRd
validate_scale_mds_dots <- function(dots) {
  if (length(dots) == 0) {
    return(invisible(NULL))
  }

  nms <- names(dots)
  if (is.null(nms) || any(!nzchar(nms))) {
    stop(
      "All additional arguments to scale_mds() must be named.",
      call. = FALSE
    )
  }

  invisible(NULL)
}

#' @noRd
compact_nulls <- function(x) {
  x[!vapply(x, is.null, logical(1))]
}

#' @noRd
scale_mds_pull_arg <- function(dots, aliases, default = NULL) {
  hits <- names(dots)[names(dots) %in% aliases]
  if (length(hits) > 1) {
    stop(
      "Please supply only one of: ",
      paste(aliases, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  if (length(hits) == 0) {
    return(default)
  }
  dots[[hits[[1]]]]
}

#' @noRd
scale_mds_drop_aliases <- function(dots, aliases) {
  if (length(dots) == 0) {
    return(dots)
  }
  keep <- !(names(dots) %in% aliases)
  dots[keep]
}

#' @noRd
scale_mds_check_unused <- function(dots, type) {
  if (length(dots) == 0) {
    return(invisible(NULL))
  }

  stop(
    "Unsupported arguments for scale_mds(type = '",
    type,
    "'): ",
    paste(names(dots), collapse = ", "),
    call. = FALSE
  )
}

#' @noRd
build_base_biplot_from_spec <- function(spec, dots) {
  common_aliases <- c("classes", "group_aes", "group.aes", "title", "Title")
  classes <- scale_mds_pull_arg(dots, "classes")
  group_aes <- scale_mds_pull_arg(dots, c("group_aes", "group.aes"))
  title <- scale_mds_pull_arg(dots, c("title", "Title"))

  dots <- scale_mds_drop_aliases(dots, common_aliases)

  bp <- do.call(
    biplotEZ::biplot,
    compact_nulls(list(
      data = spec$analysis_data,
      classes = classes,
      group.aes = group_aes,
      center = spec$center,
      scaled = spec$scale,
      Title = title
    ))
  )

  list(
    bp = bp,
    dots = dots,
    common = compact_nulls(list(
      classes = classes,
      group_aes = group_aes,
      title = title
    ))
  )
}

#' @noRd
scale_mds_build_pca <- function(bp, dots) {
  show_aliases <- c(
    "show_class_means",
    "show.class.means",
    "show_group_means",
    "show.group.means"
  )
  arg_aliases <- c(
    "dimensions", "dim.biplot",
    "eigenvectors", "e.vects",
    "group_aes", "group.aes",
    show_aliases,
    "correlation_biplot", "correlation.biplot"
  )

  dimensions <- scale_mds_pull_arg(dots, c("dimensions", "dim.biplot"))
  eigenvectors <- scale_mds_pull_arg(dots, c("eigenvectors", "e.vects"))
  group_aes <- scale_mds_pull_arg(dots, c("group_aes", "group.aes"))
  show_class_means <- scale_mds_pull_arg(dots, show_aliases)
  correlation_biplot <- scale_mds_pull_arg(
    dots,
    c("correlation_biplot", "correlation.biplot")
  )

  dots <- scale_mds_drop_aliases(dots, arg_aliases)
  scale_mds_check_unused(dots, "pca")

  ez_obj <- do.call(
    biplotEZ::PCA,
    compact_nulls(list(
      bp = bp,
      dim.biplot = dimensions,
      e.vects = eigenvectors,
      group.aes = group_aes,
      show.class.means = show_class_means,
      correlation.biplot = correlation_biplot
    ))
  )

  list(
    ez_obj = ez_obj,
    args = compact_nulls(list(
      dimensions = dimensions,
      eigenvectors = eigenvectors,
      group_aes = group_aes,
      show_class_means = show_class_means,
      correlation_biplot = correlation_biplot
    ))
  )
}

#' @noRd
scale_mds_build_cva <- function(bp, dots, classes = NULL) {
  if (is.null(classes)) {
    stop(
      "scale_mds(type = 'cva') requires 'classes'.",
      call. = FALSE
    )
  }

  show_aliases <- c(
    "show_class_means",
    "show.class.means",
    "show_group_means",
    "show.group.means"
  )
  arg_aliases <- c(
    "dimensions", "dim.biplot",
    "eigenvectors", "e.vects",
    "weighted_cva", "weightedCVA",
    show_aliases,
    "low_dim", "low.dim"
  )

  dimensions <- scale_mds_pull_arg(dots, c("dimensions", "dim.biplot"))
  eigenvectors <- scale_mds_pull_arg(dots, c("eigenvectors", "e.vects"))
  weighted_cva <- scale_mds_pull_arg(dots, c("weighted_cva", "weightedCVA"))
  show_class_means <- scale_mds_pull_arg(dots, show_aliases)
  low_dim <- scale_mds_pull_arg(dots, c("low_dim", "low.dim"))

  dots <- scale_mds_drop_aliases(dots, arg_aliases)
  scale_mds_check_unused(dots, "cva")

  ez_obj <- do.call(
    biplotEZ::CVA,
    compact_nulls(list(
      bp = bp,
      classes = classes,
      dim.biplot = dimensions,
      e.vects = eigenvectors,
      weightedCVA = weighted_cva,
      show.class.means = show_class_means,
      low.dim = low_dim
    ))
  )

  list(
    ez_obj = ez_obj,
    args = compact_nulls(list(
      classes = classes,
      dimensions = dimensions,
      eigenvectors = eigenvectors,
      weighted_cva = weighted_cva,
      show_class_means = show_class_means,
      low_dim = low_dim
    ))
  )
}

#' @noRd
scale_mds_build_pco <- function(bp, dots) {
  show_aliases <- c(
    "show_class_means",
    "show.class.means",
    "show_group_means",
    "show.group.means"
  )
  arg_aliases <- c(
    "Dmat", "dist_mat",
    "dist_func", "dist.func",
    "dist_func_cat", "dist.func.cat",
    "dimensions", "dim.biplot",
    "eigenvectors", "e.vects",
    "group_aes", "group.aes",
    show_aliases,
    "axes"
  )

  Dmat <- scale_mds_pull_arg(dots, c("Dmat", "dist_mat"))
  dist_func <- scale_mds_pull_arg(dots, c("dist_func", "dist.func"))
  dist_func_cat <- scale_mds_pull_arg(dots, c("dist_func_cat", "dist.func.cat"))
  dimensions <- scale_mds_pull_arg(dots, c("dimensions", "dim.biplot"))
  eigenvectors <- scale_mds_pull_arg(dots, c("eigenvectors", "e.vects"))
  group_aes <- scale_mds_pull_arg(dots, c("group_aes", "group.aes"))
  show_class_means <- scale_mds_pull_arg(dots, show_aliases)
  axes <- scale_mds_pull_arg(dots, "axes")

  dots <- scale_mds_drop_aliases(dots, arg_aliases)

  pco_args <- compact_nulls(list(
    bp = bp,
    Dmat = Dmat,
    dist.func = dist_func,
    dist.func.cat = dist_func_cat,
    dim.biplot = dimensions,
    e.vects = eigenvectors,
    group.aes = group_aes,
    show.class.means = show_class_means,
    axes = axes
  ))
  pco_args <- c(pco_args, dots)

  ez_obj <- do.call(biplotEZ::PCO, pco_args)

  list(
    ez_obj = ez_obj,
    args = compact_nulls(list(
      Dmat = Dmat,
      dist_func = dist_func,
      dist_func_cat = dist_func_cat,
      dimensions = dimensions,
      eigenvectors = eigenvectors,
      group_aes = group_aes,
      show_class_means = show_class_means,
      axes = axes,
      dist_args = if (length(dots) > 0) dots
    ))
  )
}

#' @noRd
scale_mds_build_regress <- function(bp, dots) {
  show_aliases <- c(
    "show_group_means",
    "show.group.means",
    "show_class_means",
    "show.class.means"
  )
  arg_aliases <- c(
    "Z", "z",
    "group_aes", "group.aes",
    show_aliases,
    "axes"
  )

  Z <- scale_mds_pull_arg(dots, c("Z", "z"))
  if (is.null(Z)) {
    stop(
      "scale_mds(type = 'regress') requires 'Z'.",
      call. = FALSE
    )
  }

  group_aes <- scale_mds_pull_arg(dots, c("group_aes", "group.aes"))
  show_group_means <- scale_mds_pull_arg(dots, show_aliases)
  axes <- scale_mds_pull_arg(dots, "axes")

  dots <- scale_mds_drop_aliases(dots, arg_aliases)
  scale_mds_check_unused(dots, "regress")

  ez_obj <- do.call(
    biplotEZ::regress,
    compact_nulls(list(
      bp = bp,
      Z = Z,
      group.aes = group_aes,
      show.group.means = show_group_means,
      axes = axes
    ))
  )

  list(
    ez_obj = ez_obj,
    args = compact_nulls(list(
      Z = Z,
      group_aes = group_aes,
      show_group_means = show_group_means,
      axes = axes
    ))
  )
}

#' @noRd
scale_mds_restore_raw_x <- function(x) {
  if (isTRUE(x$scaled)) {
    x$X <- scale(x$X, center = FALSE, scale = 1 / x$sd)
  }
  if (isTRUE(x$center)) {
    x$X <- scale(x$X, -x$means, scale = FALSE)
  }
  x
}

#' @noRd
scale_mds_extract_display_aes <- function(x) {
  color <- x$samples$col
  symbol <- pch_to_plotly(x$samples$pch)
  group <- x$group.aes
  if (length(levels(x$group.aes)) == 1) {
    group <- factor(rep("Data", x$n))
  }

  list(
    color = color,
    symbol = symbol,
    group = group
  )
}

#' @noRd
scale_mds_single_fit_table <- function(ez_obj) {
  tmp <- list(mdsDisplay = list())
  tmp <- add_table_mdsDisplay(tmp, x = ez_obj)
  tmp$mdsDisplay$fit_table
}

#' @noRd
scale_mds_single_pc_info <- function(pcs, prefix = "PC") {
  info <- list()
  info[[mdsDisplay_name(pcs)]] <- list(
    pcs = pcs,
    label = pair_label(pcs, prefix = prefix),
    ft_name = ft_name(pcs)
  )
  info
}

#' @noRd
scale_mds_new_single_biplot <- function(
  ez_obj,
  mdsDisplay,
  fit_measures,
  pcs,
  dim_prefix,
  biplot_type,
  fit_quality,
  fit_quality_plotly = NULL,
  spline = NULL
) {
  aes <- scale_mds_extract_display_aes(ez_obj)
  pname <- mdsDisplay_name(pcs)

  meta <- compact_nulls(list(
    x = ez_obj,
    color = aes$color,
    symbol = aes$symbol,
    group = aes$group,
    fit.quality = fit_quality,
    fit.quality.plotly = fit_quality_plotly,
    pc_info = scale_mds_single_pc_info(pcs, prefix = dim_prefix),
    dim_prefix = dim_prefix,
    spline = spline
  ))

  mdsDisplays <- list()
  mdsDisplays[[pname]] <- mdsDisplay
  new_bipl5_biplot(mdsDisplays, fit_measures, meta, biplot_type = biplot_type)
}

#' @noRd
scale_mds_compile_pca_biplot <- function(x) {
  if (is.null(x$samples)) {
    x <- biplotEZ::samples(x)
  }
  x <- biplotEZ::axes(x)
  x <- biplotEZ::fit.measures(x)
  x <- scale_mds_restore_raw_x(x)

  aes <- scale_mds_extract_display_aes(x)
  pcs <- as.numeric(sort(x$e.vects))
  pname <- mdsDisplay_name(pcs)

  payl <- build_one_mdsDisplay(
    ez_obj = x,
    group = aes$group,
    color = aes$color,
    symbol = aes$symbol,
    x_ref = x,
    include_polygons = TRUE
  )

  fit_tables <- list()
  fit_tables[[ft_name(pcs)]] <- scale_mds_single_fit_table(x)

  fm_payl <- list()
  fm_payl["CumPred"] <- add_axis_pred_mdsDisplay(fm_payl, x)
  fm_payl["CumAd"] <- add_axis_adeq_mdsDisplay(fm_payl, x)
  fm_payl["VarExp"] <- add_prop_variance_mdsDisplay(x)
  fm_payl["Scree"] <- add_scree_mdsDisplay(x)

  fit_measures <- new_bipl5_fitmeasures(
    CumPred = fm_payl$CumPred,
    CumAd = fm_payl$CumAd,
    VarExp = fm_payl$VarExp,
    Scree = fm_payl$Scree,
    fit_tables = fit_tables
  )

  scale_mds_new_single_biplot(
    ez_obj = x,
    mdsDisplay = payl,
    fit_measures = fit_measures,
    pcs = pcs,
    dim_prefix = "PC",
    biplot_type = "pca",
    fit_quality = fit_quality(x$eigenvalues, x$e.vects)
  )
}

#' @noRd
scale_mds_compile_cva_biplot <- function(x) {
  if (is.null(x$samples)) {
    x <- biplotEZ::samples(x)
  }
  x <- biplotEZ::axes(x)
  x <- biplotEZ::fit.measures(x)
  if (is.null(x$means.aes)) {
    x <- biplotEZ::means(x)
  }
  x <- scale_mds_restore_raw_x(x)

  aes <- scale_mds_extract_display_aes(x)
  pcs <- as.numeric(sort(x$e.vects))

  payl <- build_one_mdsDisplay(
    ez_obj = x,
    group = aes$group,
    color = aes$color,
    symbol = aes$symbol,
    x_ref = x,
    include_polygons = TRUE,
    dim_prefix = "CV",
    ax_pred = FALSE,
    vec_dis = FALSE
  )

  scale_mds_new_single_biplot(
    ez_obj = x,
    mdsDisplay = payl,
    fit_measures = NULL,
    pcs = pcs,
    dim_prefix = "CV",
    biplot_type = "cva",
    fit_quality = fit_quality(x$eigenvalues, x$e.vects, dim_prefix = "CV")
  )
}

#' @noRd
scale_mds_compile_regress_biplot <- function(x) {
  if (is.null(x$samples)) {
    x <- biplotEZ::samples(x)
  }
  x <- biplotEZ::axes(x)

  z.axes <- biplotEZ::axes_coordinates(x)
  pcs <- c(1, 2)
  fit_qual <- regression_fit_quality(
    X = x$X,
    Z = x$Z,
    basis = pcs,
    dim_prefix = "Dim"
  )
  fit_qual_plotly <- regression_fit_quality_tex(
    X = x$X,
    Z = x$Z
  )

  x <- scale_mds_restore_raw_x(x)
  aes <- scale_mds_extract_display_aes(x)

  payl <- build_one_mdsDisplay(
    ez_obj = x,
    group = aes$group,
    color = aes$color,
    symbol = aes$symbol,
    x_ref = x,
    include_polygons = TRUE,
    dim_prefix = "Dim",
    ax_pred = FALSE,
    vec_dis = FALSE,
    z.axes = z.axes,
    fit_qual = fit_qual
  )

  scale_mds_new_single_biplot(
    ez_obj = x,
    mdsDisplay = payl,
    fit_measures = NULL,
    pcs = pcs,
    dim_prefix = "Dim",
    biplot_type = "reg",
    fit_quality = fit_qual,
    fit_quality_plotly = fit_qual_plotly
  )
}

#' @noRd
scale_mds_compile_pco_biplot <- function(x) {
  if (is.null(x$samples)) {
    x <- biplotEZ::samples(x)
  }
  if (is.null(x$axes)) {
    x <- biplotEZ::axes(x)
  }

  temp <- x$raw.X
  x$raw.X <- x$X
  z.axes <- biplotEZ::axes_coordinates(x)
  x$raw.X <- temp

  x <- scale_mds_restore_raw_x(x)
  aes <- scale_mds_extract_display_aes(x)

  pcs <- c(1, 2)
  is_spline <- identical(x$PCOaxes, "splines")

  if (is_spline) {
    payl <- build_spline_mdsDisplay(
      ez_obj = x,
      group = aes$group,
      color = aes$color,
      symbol = aes$symbol,
      z.axes = z.axes
    )
  } else {
    payl <- build_one_mdsDisplay(
      ez_obj = x,
      group = aes$group,
      color = aes$color,
      symbol = aes$symbol,
      x_ref = x,
      include_polygons = TRUE,
      dim_prefix = "Dim",
      ax_pred = FALSE,
      vec_dis = FALSE,
      z.axes = z.axes
    )
  }

  scale_mds_new_single_biplot(
    ez_obj = x,
    mdsDisplay = payl,
    fit_measures = NULL,
    pcs = pcs,
    dim_prefix = "Dim",
    biplot_type = "pco",
    fit_quality = "",
    spline = is_spline
  )
}
