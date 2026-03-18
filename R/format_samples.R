#' Format sample aesthetics on a bipl5_biplot
#'
#' `format_samples()` rebuilds the sample-trace block inside each
#' `mdsDisplay` so that observations are grouped by `by` and rendered with one
#' trace per visual class. This means the visible trace structure, legend
#' labels, and stored sample-format metadata all stay aligned.
#'
#' The function is intended for sample formatting only. It does not refit the
#' underlying ordination model. In particular, for CVA biplots the fitted CVA
#' classes are preserved and only the sample traces are reformatted.
#'
#' A first call to `format_samples()` creates one sample legend section for the
#' requested aesthetic. For example, `format_samples(stratify = "col", by =
#' Species)` will colour the observations by `Species` and create a legend
#' section headed `Species` with one entry per class.
#'
#' A second call can be used to add a second, independent sample
#' stratification. If the second call uses the same grouping variable as the
#' first call, both aesthetics are applied to the same set of classes and the
#' legend remains unified. If the second call uses a different grouping
#' variable, `format_samples()` creates a second legend section and internally
#' splits the observation layer into all observed combinations of the two
#' grouping variables.
#'
#' For example, the sequence
#'
#' `init_biplot(iris2) |> scale_mds("pca") |> format_samples(stratify = "col",
#' by = Species) |> format_samples(stratify = "symbol", by = Band)`
#'
#' will produce two sample legend sections:
#' - `Species` for the colour grouping
#' - `Band` for the symbol grouping
#'
#' The visible observation traces are then split by `Species x Band`, but these
#' combination traces are hidden from the legend. Instead, `format_samples()`
#' inserts legend-only sample entries so the legend remains easy to read.
#'
#' If translated axes are available in the `mdsDisplay`, a colour
#' stratification also rebuilds the kernel-density traces on the translated
#' axes so that those densities reflect the colour classes. Symbol-only
#' stratification does not change the translated-axis densities. This means:
#' - `format_samples(stratify = "col", ...)` recalculates translated-axis
#'   densities by the colour grouping
#' - `format_samples(stratify = "symbol", ...)` leaves the existing translated
#'   densities unchanged
#'
#' The legend toggles operate across the full dual stratification:
#' - clicking a colour legend entry hides or shows all observations belonging to
#'   that colour class, across every symbol class
#' - clicking a symbol legend entry hides or shows all observations belonging to
#'   that symbol class, across every colour class
#'
#' The formatting is applied to every `mdsDisplay` currently stored in the
#' object. If additional displays are later added with [append_mdsDisplay()],
#' the stored sample-format state is reused so the new displays inherit the same
#' sample legend structure.
#'
#' @param x A `bipl5_biplot` object.
#' @param stratify Which aesthetic to change: `"col"` for marker colour or
#'   `"symbol"` for marker symbol.
#' @param by Optional grouping variable for the sample traces. This can be:
#'   - a bare column name stored in the dataset supplied to [init_biplot()]
#'   - a single character column name stored in the object, or
#'   - a vector/factor of length `n`, one value per observation.
#'
#'   When `NULL`, the current sample grouping in `x$meta$group` is reused.
#' @param col Optional vector of colours. When `stratify = "col"`, this must
#'   have one value per visual class defined by `by`. If omitted, a default
#'   palette is used.
#' @param pch Optional vector of plotting symbols. When `stratify = "symbol"`,
#'   this must have one value per visual class defined by `by`. Numeric base-R
#'   pch codes are converted internally to plotly symbols; character plotly
#'   symbol names are also accepted.
#'
#' @return A modified `bipl5_biplot`.
#'
#' @details
#' `format_samples()` supports two complementary workflows.
#'
#' `Single stratification`
#'
#' A single call to `format_samples()` rebuilds the sample layer so that one
#' trace is created per class in `by`. This updates the marker appearance, the
#' legend entries, and the stored sample-format metadata consistently.
#'
#' `Second stratification`
#'
#' A second call to `format_samples()` can be used to add a second sample
#' aesthetic. This is most useful when colour and plotting symbol represent
#' different variables.
#'
#' If the second call uses the same grouping structure as the first, the result
#' is still one legend section with one entry per class, but each class now
#' carries both a colour and a plotting symbol.
#'
#' If the second call uses a different grouping structure, the object stores two
#' independent sample legend sections. Internally, the observation layer is
#' rebuilt as one hidden trace per observed combination of the two grouping
#' variables. The visible legend then shows one section for each stratifying
#' variable.
#'
#' `Translated-axis densities`
#'
#' When translated axes are present, the kernel-density traces on those axes are
#' tied to the current colour grouping. Applying `format_samples(stratify =
#' "col", ...)` rebuilds the translated-axis density traces so they match the
#' colour classes. Applying `format_samples(stratify = "symbol", ...)` does not
#' rebuild those densities.
#'
#' So:
#' - a first colour stratification updates both the sample layer and the
#'   translated-axis densities
#' - a later symbol stratification leaves those densities as they are
#' - if a symbol stratification is applied first and a colour stratification is
#'   added later, the translated-axis densities are rebuilt when the colour
#'   stratification is added
#'
#' `Legend click behaviour`
#'
#' When two different stratifications are active, the legend entries behave like
#' filters:
#' - clicking a class in the first legend section toggles all observations in
#'   that class, regardless of their membership in the second stratification
#' - clicking a class in the second legend section toggles all observations in
#'   that class, regardless of their membership in the first stratification
#'
#' So if colours represent `Species` and symbols represent `Band`, clicking
#' `setosa` hides all `setosa` observations, while clicking `class1` hides all
#' `class1` observations across every species.
#'
#' `Non-standard evaluation`
#'
#' If `by` is supplied as a bare column name, `format_samples()` looks for that
#' column in the dataset stored by [init_biplot()]. If `by` is supplied as a
#' character string, it is interpreted as the name of a stored column. If `by`
#' is supplied as a vector, it must have one value per observation; in that case
#' the legend title defaults to `"Data"` because there is no stored column name
#' to display.
#'
#' `CVA note`
#'
#' `format_samples()` does not change the fitted CVA model. It only reformats
#' the sample traces. The grouping used to fit the CVA model should therefore be
#' specified in [scale_mds()], not in `format_samples()`.
#'
#' @examples
#' bp <- init_biplot(iris) |>
#'   scale_mds(type = "pca", eigenvectors = c(1, 2))
#'
#' bp_species <- format_samples(
#'   bp,
#'   stratify = "col",
#'   by = Species,
#'   col = c("tomato", "steelblue", "darkgreen")
#' )
#'
#' sample_idx <- vapply(
#'   bp_species$mdsDisplay_12$mdsDisplay$trace_data,
#'   function(tr) "data" %in% unlist(tr$meta),
#'   logical(1)
#' )
#'
#' vapply(
#'   bp_species$mdsDisplay_12$mdsDisplay$trace_data[sample_idx],
#'   `[[`,
#'   character(1),
#'   "name"
#' )
#'
#' bp_symbol <- format_samples(
#'   bp,
#'   stratify = "symbol",
#'   by = Species,
#'   pch = c(16, 17, 15)
#' )
#'
#' iris2 <- iris
#' iris2$Band <- factor(
#'   rep(c("class1", "class2", "class3", "class4"), length.out = nrow(iris2))
#' )
#'
#' bp_dual <- init_biplot(iris2) |>
#'   scale_mds(type = "pca", eigenvectors = c(1, 2)) |>
#'   format_samples(
#'     stratify = "col",
#'     by = Species,
#'     col = c("tomato", "steelblue", "darkgreen")
#'   ) |>
#'   format_samples(
#'     stratify = "symbol",
#'     by = Band,
#'     pch = c(12, 13, 14, 15)
#'   )
#'
#' # When plotted, the legend now has one section for Species and one for Band.
#' # Clicking a Species entry hides that species across all Band classes.
#' # Clicking a Band entry hides that Band class across all Species classes.
#' if (interactive()) {
#'   plot(bp_dual)
#' }
#'
#' bp_species_13 <- append_mdsDisplay(bp_species, c(1, 3))
#' @export
format_samples <- function(
  x,
  stratify = c("col", "symbol"),
  by = NULL,
  col = NULL,
  pch = NULL
) {
  if (!inherits(x, "bipl5_biplot")) {
    stop("'x' must inherit from 'bipl5_biplot'.", call. = FALSE)
  }

  by_missing <- missing(by)
  by_expr <- substitute(by)
  by_env <- parent.frame()
  stratify <- match.arg(stratify)
  original_data <- format_samples_get_data(x)
  n <- format_samples_n_obs(x, original_data)
  state <- format_samples_get_state(x, n)

  group_info <- format_samples_resolve_grouping(
    x = x,
    state = state,
    stratify = stratify,
    by_expr = by_expr,
    by_env = by_env,
    by_missing = by_missing,
    data = original_data,
    n = n
  )

  aes_info <- format_samples_resolve_target_aes(
    state = state,
    stratify = stratify,
    group = group_info$group,
    col = col,
    pch = pch
  )
  state <- format_samples_update_state(
    state = state,
    stratify = stratify,
    group_info = group_info,
    aes_info = aes_info
  )

  update_means <- format_samples_should_update_means(x, state)
  mds_names <- names(x$meta$pc_info)

  for (mds_name in mds_names) {
    x[[mds_name]] <- format_samples_rebuild_mdsDisplay(
      mds = x[[mds_name]],
      update_means = update_means,
      state = state,
      ez_obj = x$meta$x,
      rebuild_tda = identical(stratify, "col")
    )
  }

  x <- format_samples_update_meta(x = x, state = state)

  x
}


#' Retrieve the original data frame from a bipl5_biplot
#' @noRd
format_samples_get_data <- function(x) {
  if (!is.null(x$meta$spec$data)) {
    return(as.data.frame(x$meta$spec$data))
  }
  if (!is.null(x$meta$x$raw.X)) {
    return(as.data.frame(x$meta$x$raw.X))
  }
  if (!is.null(x$meta$x$X)) {
    return(as.data.frame(x$meta$x$X))
  }
  stop("Cannot retrieve data from the bipl5_biplot object.", call. = FALSE)
}

#' @noRd
format_samples_n_obs <- function(x, data) {
  if (!is.null(x$meta$x$n)) {
    return(as.integer(x$meta$x$n))
  }
  nrow(data)
}

#' @noRd
format_samples_get_state <- function(x, n) {
  current <- x$meta$sample_format
  defaults <- list(
    color = format_samples_default_color(x),
    symbol = format_samples_default_symbol(x),
    pch_numeric = format_samples_default_pch(x)
  )

  if (is.list(current) && identical(current$version, 2L)) {
    current$defaults <- defaults
    if (is.null(current$order)) {
      current$order <- character(0)
    }
    if (is.null(current$color)) {
      current$color <- NULL
    }
    if (is.null(current$symbol)) {
      current$symbol <- NULL
    }
    return(current)
  }

  state <- list(
    version = 2L,
    order = character(0),
    color = NULL,
    symbol = NULL,
    defaults = defaults
  )

  if (is.list(current) && !is.null(current$stratify)) {
    kind <- switch(
      current$stratify,
      col = "color",
      symbol = "symbol",
      NULL
    )
    if (!is.null(kind)) {
      state[[kind]] <- format_samples_make_legacy_spec(
        x = x,
        state = state,
        kind = kind,
        n = n
      )
      state$order <- kind
    }
  }

  state
}

#' @noRd
format_samples_make_legacy_spec <- function(x, state, kind, n) {
  group <- format_samples_current_group(x, n)
  legend_title <- format_samples_legacy_legend_title(x)
  source <- if (identical(legend_title, "Data")) "existing grouping" else legend_title

  if (identical(kind, "color")) {
    values <- x$meta$color
    if (is.null(values) || length(values) != nlevels(group)) {
      values <- rep(state$defaults$color, length.out = nlevels(group))
    }

    return(list(
      group = group,
      levels = levels(group),
      values = unname(values),
      legend_title = legend_title,
      source = source
    ))
  }

  values <- x$meta$symbol
  if (is.null(values) || length(values) != nlevels(group)) {
    values <- rep(state$defaults$symbol, length.out = nlevels(group))
  }

  pch_numeric <- x$meta$x$samples$pch
  if (is.null(pch_numeric) || length(pch_numeric) != nlevels(group)) {
    pch_numeric <- rep(state$defaults$pch_numeric, length.out = nlevels(group))
  }

  list(
    group = group,
    levels = levels(group),
    values = unname(values),
    pch_numeric = as.integer(unname(pch_numeric)),
    legend_title = legend_title,
    source = source
  )
}

#' @noRd
format_samples_default_color <- function(x) {
  color <- x$meta$color
  if (is.null(color) || length(color) == 0) {
    return(colorpal(1))
  }
  color[[1]]
}

#' @noRd
format_samples_default_symbol <- function(x) {
  symbol <- x$meta$symbol
  if (is.null(symbol) && !is.null(x$meta$x$samples$plotly_symbol)) {
    symbol <- x$meta$x$samples$plotly_symbol
  }
  if (is.null(symbol) || length(symbol) == 0) {
    return("circle")
  }
  symbol[[1]]
}

#' @noRd
format_samples_default_pch <- function(x) {
  pch_numeric <- x$meta$x$samples$pch
  if (is.null(pch_numeric) || length(pch_numeric) == 0) {
    return(19L)
  }
  as.integer(pch_numeric[[1]])
}

#' @noRd
format_samples_state_spec <- function(state, kind) {
  state[[kind]]
}

#' @noRd
format_samples_primary_kind <- function(state) {
  active <- state$order[state$order %in% c("color", "symbol")]
  active <- active[!vapply(active, function(kind) is.null(state[[kind]]), logical(1))]
  if (length(active) == 0) {
    return(NULL)
  }
  active[[1]]
}

#' @noRd
format_samples_primary_spec <- function(state) {
  kind <- format_samples_primary_kind(state)
  if (is.null(kind)) {
    return(NULL)
  }
  state[[kind]]
}

#' @noRd
format_samples_current_group <- function(x, n) {
  group <- x$meta$group
  if (is.null(group) || length(group) != n) {
    return(factor(rep("Data", n), levels = "Data"))
  }
  if (is.factor(group)) {
    return(droplevels(group))
  }
  group_chr <- as.character(group)
  factor(group_chr, levels = unique(group_chr))
}

#' @noRd
format_samples_legacy_legend_title <- function(x) {
  title <- x$meta$sample_format$legend_title
  if (is.null(title) || !is.character(title) || length(title) != 1 || !nzchar(title)) {
    return("Data")
  }
  title
}

#' Resolve the grouping used to split the sample traces
#' @noRd
format_samples_resolve_grouping <- function(
  x,
  state,
  stratify,
  by_expr,
  by_env,
  by_missing,
  data,
  n
) {
  current_spec <- format_samples_state_spec(
    state,
    if (identical(stratify, "col")) "color" else "symbol"
  )
  fallback_spec <- if (!is.null(current_spec)) {
    current_spec
  } else {
    format_samples_primary_spec(state)
  }

  if (isTRUE(by_missing) || identical(by_expr, quote(NULL))) {
    group_raw <- if (!is.null(fallback_spec)) fallback_spec$group else format_samples_current_group(x, n)
    source <- "existing grouping"
    legend_title <- if (!is.null(fallback_spec)) fallback_spec$legend_title else format_samples_legacy_legend_title(x)
  } else {
    evaluated <- tryCatch(
      eval(by_expr, envir = data, enclos = by_env),
      error = function(e) e
    )

    if (inherits(evaluated, "error")) {
      by_label <- format_samples_expr_label(by_expr)
      stop(
        "Column '",
        by_label,
        "' was not found in the data stored by this object. ",
        "Supply the grouping variable directly as a vector if it is not part ",
        "of the dataset passed to init_biplot().",
        call. = FALSE
      )
    }

    if (is.character(evaluated) && length(evaluated) == 1) {
      if (!evaluated %in% names(data)) {
        stop(
          "Column '",
          evaluated,
          "' was not found in the data stored by this object. ",
          "Supply the grouping variable directly as a vector if it is not part ",
          "of the dataset passed to init_biplot().",
          call. = FALSE
        )
      }
      group_raw <- data[[evaluated]]
      source <- evaluated
      legend_title <- evaluated
    } else if (length(evaluated) == n) {
      group_raw <- evaluated
      source <- if (
        is.symbol(by_expr) &&
        format_samples_expr_label(by_expr) %in% names(data)
      ) {
        format_samples_expr_label(by_expr)
      } else {
        "supplied vector"
      }
      legend_title <- if (identical(source, "supplied vector")) {
        "Data"
      } else {
        source
      }
    } else {
      stop(
        "'by' must resolve to a stored column name or a vector of length ",
        n,
        ".",
        call. = FALSE
      )
    }
  }

  if (anyNA(group_raw)) {
    stop("Missing values are not supported in 'by'.", call. = FALSE)
  }

  if (is.factor(group_raw)) {
    group <- droplevels(group_raw)
  } else {
    group_chr <- as.character(group_raw)
    group <- factor(group_chr, levels = unique(group_chr))
  }

  list(
    group = group,
    levels = levels(group),
    source = source,
    legend_title = legend_title
  )
}

#' @noRd
format_samples_expr_label <- function(expr) {
  paste(deparse(expr, width.cutoff = 500L), collapse = "")
}

#' @noRd
format_samples_resolve_target_aes <- function(state, stratify, group, col, pch) {
  k <- nlevels(group)

  if (identical(stratify, "col")) {
    return(list(
      values = unname(format_samples_resolve_colors(col, k))
    ))
  }

  symbol_info <- format_samples_resolve_symbols(pch, k)
  list(
    values = unname(symbol_info$symbol),
    pch_numeric = as.integer(unname(symbol_info$pch_numeric))
  )
}

#' @noRd
format_samples_update_state <- function(state, stratify, group_info, aes_info) {
  kind <- if (identical(stratify, "col")) "color" else "symbol"

  spec <- list(
    group = group_info$group,
    levels = group_info$levels,
    values = aes_info$values,
    legend_title = group_info$legend_title,
    source = group_info$source
  )

  if (identical(kind, "symbol")) {
    spec$pch_numeric <- aes_info$pch_numeric
  }

  state[[kind]] <- spec
  if (!(kind %in% state$order)) {
    state$order <- c(state$order, kind)
  }
  state
}

#' @noRd
format_samples_expand_single <- function(x, k, fallback) {
  if (is.null(x) || length(x) == 0) {
    return(rep(fallback, length.out = k))
  }
  if (length(x) == k) {
    return(x)
  }
  rep(x[[1]], length.out = k)
}

#' @noRd
format_samples_resolve_colors <- function(col, k) {
  if (is.null(col)) {
    if (k <= 16) {
      return(colorpal(k))
    }
    return(grDevices::hcl.colors(k, palette = "Dark 3"))
  }

  if (length(col) != k) {
    stop(
      "Expected ",
      k,
      " colours, got ",
      length(col),
      ".",
      call. = FALSE
    )
  }

  col
}

#' @noRd
format_samples_resolve_symbols <- function(pch, k) {
  if (is.null(pch)) {
    stop("'pch' is required when stratify = 'symbol'.", call. = FALSE)
  }

  if (length(pch) != k) {
    stop(
      "Expected ",
      k,
      " plotting symbols, got ",
      length(pch),
      ".",
      call. = FALSE
    )
  }

  if (is.numeric(pch)) {
    pch_numeric <- as.integer(pch)
    symbols <- pch_to_plotly(pch_numeric)
    if (any(!nzchar(symbols))) {
      stop("One or more numeric pch values are not supported.", call. = FALSE)
    }
  } else {
    invalid <- validate_symbol(as.character(pch))
    if (!is.null(invalid)) {
      stop(
        "Invalid plotly symbols: ",
        paste(invalid, collapse = ", "),
        call. = FALSE
      )
    }
    symbols <- as.character(pch)
    pch_numeric <- format_samples_plotly_to_pch(symbols)
    pch_numeric[is.na(pch_numeric)] <- 19L
  }

  list(
    symbol = symbols,
    pch_numeric = pch_numeric
  )
}

#' @noRd
format_samples_plotly_to_pch <- function(symbols) {
  codes <- 0:25
  mapped <- pch_to_plotly(codes)
  reverse_map <- stats::setNames(codes, mapped)
  as.integer(unname(reverse_map[as.character(symbols)]))
}

#' Rebuild the sample trace block for one mdsDisplay
#' @noRd
format_samples_rebuild_mdsDisplay <- function(
  mds,
  update_means,
  state,
  ez_obj = NULL,
  rebuild_tda = FALSE
) {
  traces <- mds$mdsDisplay$trace_data
  sample_idx <- which(
    vapply(traces, format_samples_has_meta, logical(1), key = "data")
  )
  sample_legend_idx <- which(
    vapply(traces, format_samples_has_meta, logical(1), key = "sample-legend")
  )

  if (length(sample_idx) == 0) {
    return(mds)
  }

  sample_points <- format_samples_collect_sample_points(
    traces = traces[sample_idx]
  )

  rebuilt_samples <- format_samples_build_sample_traces(
    points = sample_points,
    state = state,
    template = traces[[sample_idx[[1]]]]
  )

  mean_idx <- integer(0)
  rebuilt_means <- list()
  if (isTRUE(update_means)) {
    mean_spec <- format_samples_mean_spec(state, nrow(sample_points))
    mean_idx <- which(
      vapply(traces, format_samples_has_meta, logical(1), key = "ClassMean")
    )
    rebuilt_means <- format_samples_build_mean_traces(
      points = sample_points,
      mean_spec = mean_spec,
      template = if (length(mean_idx) > 0) traces[[mean_idx[[1]]]] else NULL
    )
  }

  mds$mdsDisplay$trace_data <- format_samples_replace_trace_blocks(
    traces = traces,
    sample_idx = c(sample_idx, sample_legend_idx),
    rebuilt_samples = rebuilt_samples,
    mean_idx = mean_idx,
    rebuilt_means = rebuilt_means
  )

  if (isTRUE(rebuild_tda)) {
    mds <- format_samples_rebuild_tda_layer(
      mds = mds,
      state = state,
      ez_obj = ez_obj
    )
  }

  mds
}

#' @noRd
format_samples_rebuild_tda_layer <- function(mds, state, ez_obj) {
  color_spec <- state$color
  if (is.null(color_spec) || is.null(ez_obj)) {
    return(mds)
  }

  z.axes <- mds$Data$axes_coordinates
  Z <- mds$Data$sample_coordinates
  if (is.null(z.axes) || is.null(Z)) {
    return(mds)
  }

  traces <- mds$mdsDisplay$trace_data
  tda_idx <- which(
    vapply(traces, format_samples_has_meta, logical(1), key = "ExpAx") |
      vapply(traces, format_samples_has_meta, logical(1), key = "density")
  )
  if (length(tda_idx) == 0) {
    return(mds)
  }

  mds$mdsDisplay$trace_data <- traces[-tda_idx]

  annotations <- mds$mdsDisplay$layout$annotations
  if (!is.null(annotations)) {
    keep_ann <- !vapply(
      annotations,
      function(ann) format_samples_has_meta(ann, "ExpAx"),
      logical(1)
    )
    mds$mdsDisplay$layout$annotations <- annotations[keep_ann]
  }

  tda_x <- ez_obj
  tda_x$Z <- as.matrix(Z)

  tda_out <- add_TDA_mdsDisplay(
    mdsDisplay = mds$mdsDisplay,
    z.axes = z.axes,
    x = tda_x,
    Z = as.matrix(Z),
    group = color_spec$group,
    Col = color_spec$values
  )

  mds$mdsDisplay <- tda_out$mdsDisplay
  mds$m <- tda_out$m
  mds$shift <- tda_out$shift
  mds$Data$translated_axes_coordinates <- tda_out$shift

  slider_bundle <- list(
    mdsDisplay = mds$mdsDisplay,
    m = tda_out$m,
    shift = tda_out$shift
  )
  slider_bundle <- slider_control_mdsDisplay(
    slider_bundle,
    n_inside = 17,
    n_outside = 4
  )
  mds$mdsDisplay$config$slider_info <- slider_bundle$mdsDisplay$config$slider_info

  mds
}

#' @noRd
format_samples_has_meta <- function(trace, key) {
  meta <- trace$meta
  if (is.null(meta)) {
    return(FALSE)
  }
  if (is.character(meta)) {
    return(identical(meta, key))
  }
  key %in% unlist(meta, use.names = FALSE)
}

#' Collect point-level sample information from the existing traces
#' @noRd
format_samples_collect_sample_points <- function(traces) {
  point_data <- lapply(traces, function(tr) {
    obs_idx <- as.integer(unlist(tr$customdata, use.names = FALSE))
    hovertext <- as.character(unlist(tr$hovertext, use.names = FALSE))
    x <- as.numeric(unlist(tr$x, use.names = FALSE))
    y <- as.numeric(unlist(tr$y, use.names = FALSE))

    if (length(hovertext) == 1 && length(obs_idx) > 1) {
      hovertext <- rep(hovertext, length(obs_idx))
    }

    data.frame(
      obs_idx = obs_idx,
      x = x,
      y = y,
      hovertext = hovertext,
      stringsAsFactors = FALSE
    )
  })

  points <- do.call(rbind, point_data)
  points <- points[order(points$obs_idx), , drop = FALSE]
  rownames(points) <- NULL
  points
}

#' @noRd
format_samples_build_sample_traces <- function(
  points,
  state,
  template
) {
  n <- if (nrow(points) == 0) 0L else max(points$obs_idx)
  color_spec <- format_samples_effective_spec(state, "color", n)
  symbol_spec <- format_samples_effective_spec(state, "symbol", n)

  points$color_group <- as.character(color_spec$group[points$obs_idx])
  points$symbol_group <- as.character(symbol_spec$group[points$obs_idx])

  if (format_samples_has_dual_stratification(state, n)) {
    section_order <- format_samples_section_order(state)
    out <- list()
    for (kind in section_order) {
      spec <- if (identical(kind, "color")) color_spec else symbol_spec
      out <- c(
        out,
        format_samples_build_sample_legend_traces(
          kind = kind,
          spec = spec,
          state = state,
          template = template
        )
      )
    }
    return(c(
      out,
      format_samples_build_combo_sample_traces(
        points = points,
        color_spec = color_spec,
        symbol_spec = symbol_spec,
        template = template
      )
    ))
  }

  unified_spec <- format_samples_unified_spec(state, n)
  format_samples_build_unified_sample_traces(
    points = points,
    unified_spec = unified_spec,
    template = template
  )
}

#' @noRd
format_samples_make_legend_title <- function(title = "Data") {
  if (is.null(title) || !is.character(title) || length(title) != 1 || !nzchar(title)) {
    title <- "Data"
  }
  list(text = paste0("<b>", title, "</b>"))
}

#' @noRd
format_samples_section_order <- function(state) {
  active <- state$order[state$order %in% c("color", "symbol")]
  active[!vapply(active, function(kind) is.null(state[[kind]]), logical(1))]
}

#' @noRd
format_samples_effective_spec <- function(state, kind, n) {
  spec <- state[[kind]]
  if (!is.null(spec)) {
    return(spec)
  }

  default_group <- factor(rep("Data", n), levels = "Data")
  if (identical(kind, "color")) {
    return(list(
      group = default_group,
      levels = "Data",
      values = state$defaults$color,
      legend_title = "Data",
      source = "default"
    ))
  }

  list(
    group = default_group,
    levels = "Data",
    values = state$defaults$symbol,
    pch_numeric = state$defaults$pch_numeric,
    legend_title = "Data",
    source = "default"
  )
}

#' @noRd
format_samples_groups_identical <- function(x, y) {
  if (is.null(x) || is.null(y) || length(x) != length(y)) {
    return(FALSE)
  }
  identical(as.character(x), as.character(y))
}

#' @noRd
format_samples_has_dual_stratification <- function(state, n) {
  color_spec <- state$color
  symbol_spec <- state$symbol
  if (is.null(color_spec) || is.null(symbol_spec)) {
    return(FALSE)
  }
  !format_samples_groups_identical(color_spec$group, symbol_spec$group)
}

#' @noRd
format_samples_unified_spec <- function(state, n) {
  primary_kind <- format_samples_primary_kind(state)
  if (is.null(primary_kind)) {
    primary_kind <- "color"
  }

  primary_spec <- format_samples_effective_spec(state, primary_kind, n)
  group <- primary_spec$group
  k <- nlevels(group)

  color_spec <- state$color
  symbol_spec <- state$symbol

  colors <- if (!is.null(color_spec) && format_samples_groups_identical(color_spec$group, group)) {
    color_spec$values
  } else {
    rep(state$defaults$color, length.out = k)
  }

  if (!is.null(symbol_spec) && format_samples_groups_identical(symbol_spec$group, group)) {
    symbols <- symbol_spec$values
    pch_numeric <- symbol_spec$pch_numeric
  } else {
    symbols <- rep(state$defaults$symbol, length.out = k)
    pch_numeric <- rep(state$defaults$pch_numeric, length.out = k)
  }

  list(
    group = group,
    levels = levels(group),
    colors = unname(colors),
    symbols = unname(symbols),
    pch_numeric = as.integer(unname(pch_numeric)),
    legend_title = primary_spec$legend_title
  )
}

#' @noRd
format_samples_build_unified_sample_traces <- function(points, unified_spec, template) {
  template_marker <- template$marker
  if (is.null(template_marker)) {
    template_marker <- list(opacity = 1)
  }

  lapply(seq_along(unified_spec$levels), function(i) {
    lev <- unified_spec$levels[[i]]
    sel <- as.character(unified_spec$group[points$obs_idx]) == lev
    marker <- template_marker
    marker$color <- unified_spec$colors[[i]]
    marker$symbol <- unified_spec$symbols[[i]]

    trace <- list(
      x = points$x[sel],
      y = points$y[sel],
      name = lev,
      type = format_samples_or(template$type, "scatter"),
      mode = format_samples_or(template$mode, "markers"),
      hovertext = points$hovertext[sel],
      hoverinfo = format_samples_or(template$hoverinfo, "text+name"),
      customdata = points$obs_idx[sel],
      meta = list("data", paste0("group:", lev)),
      xaxis = format_samples_or(template$xaxis, "x"),
      yaxis = format_samples_or(template$yaxis, "y"),
      visible = format_samples_or(template$visible, TRUE),
      showlegend = TRUE,
      marker = marker,
      legendgroup = "data",
      legendgrouptitle = format_samples_make_legend_title(unified_spec$legend_title)
    )

    trace
  })
}

#' @noRd
format_samples_build_combo_sample_traces <- function(points, color_spec, symbol_spec, template) {
  template_marker <- template$marker
  if (is.null(template_marker)) {
    template_marker <- list(opacity = 1)
  }

  traces <- list()
  for (i in seq_along(color_spec$levels)) {
    color_level <- color_spec$levels[[i]]
    for (j in seq_along(symbol_spec$levels)) {
      symbol_level <- symbol_spec$levels[[j]]
      sel <- points$color_group == color_level & points$symbol_group == symbol_level
      if (!any(sel)) {
        next
      }

      marker <- template_marker
      marker$color <- color_spec$values[[i]]
      marker$symbol <- symbol_spec$values[[j]]

      traces[[length(traces) + 1]] <- list(
        x = points$x[sel],
        y = points$y[sel],
        name = paste(color_level, symbol_level, sep = " | "),
        type = format_samples_or(template$type, "scatter"),
        mode = format_samples_or(template$mode, "markers"),
        hovertext = points$hovertext[sel],
        hoverinfo = format_samples_or(template$hoverinfo, "text+name"),
        customdata = points$obs_idx[sel],
        meta = list(
          "data",
          "sample-combo",
          paste0("color:", color_level),
          paste0("symbol:", symbol_level)
        ),
        xaxis = format_samples_or(template$xaxis, "x"),
        yaxis = format_samples_or(template$yaxis, "y"),
        visible = format_samples_or(template$visible, TRUE),
        showlegend = FALSE,
        marker = marker,
        legendgroup = "data"
      )
    }
  }

  traces
}

#' @noRd
format_samples_build_sample_legend_traces <- function(kind, spec, state, template) {
  marker_symbol <- if (identical(kind, "color")) {
    rep(state$defaults$symbol, length.out = length(spec$levels))
  } else {
    spec$values
  }
  marker_color <- if (identical(kind, "color")) {
    spec$values
  } else {
    rep("black", length.out = length(spec$levels))
  }

  lapply(seq_along(spec$levels), function(i) {
    lev <- spec$levels[[i]]
    list(
      x = list(NA_real_),
      y = list(NA_real_),
      name = lev,
      type = format_samples_or(template$type, "scatter"),
      mode = format_samples_or(template$mode, "markers"),
      hoverinfo = "skip",
      showlegend = TRUE,
      visible = TRUE,
      marker = list(
        color = marker_color[[i]],
        symbol = marker_symbol[[i]],
        opacity = 1
      ),
      meta = list(
        "sample-legend",
        kind,
        paste0(kind, ":", lev)
      ),
      xaxis = format_samples_or(template$xaxis, "x"),
      yaxis = format_samples_or(template$yaxis, "y"),
      legendgroup = paste0("sample-legend-", kind),
      legendgrouptitle = if (identical(i, 1L)) {
        format_samples_make_legend_title(spec$legend_title)
      } else {
        NULL
      }
    )
  })
}

#' @noRd
format_samples_mean_spec <- function(state, n) {
  if (format_samples_has_dual_stratification(state, n) && !is.null(state$color)) {
    return(list(
      group = state$color$group,
      levels = state$color$levels,
      colors = state$color$values,
      symbols = rep(state$defaults$symbol, length.out = length(state$color$levels))
    ))
  }

  unified_spec <- format_samples_unified_spec(state, n)
  list(
    group = unified_spec$group,
    levels = unified_spec$levels,
    colors = unified_spec$colors,
    symbols = unified_spec$symbols
  )
}

#' @noRd
format_samples_build_mean_traces <- function(
  points,
  mean_spec,
  template = NULL
) {
  template_marker <- if (!is.null(template)) template$marker else NULL
  if (is.null(template_marker)) {
    template_marker <- list(size = 10)
  }

  group_values <- as.character(mean_spec$group[points$obs_idx])

  lapply(seq_along(mean_spec$levels), function(i) {
    lev <- mean_spec$levels[[i]]
    sel <- group_values == lev
    marker <- template_marker
    marker$color <- mean_spec$colors[[i]]
    marker$symbol <- mean_spec$symbols[[i]]

    list(
      x = list(mean(points$x[sel])),
      y = list(mean(points$y[sel])),
      name = lev,
      type = if (!is.null(template$type)) template$type else "scatter",
      mode = if (!is.null(template$mode)) template$mode else "markers",
      hovertext = "Class Mean",
      hoverinfo = if (!is.null(template$hoverinfo)) template$hoverinfo else "text+name",
      customdata = i - 1,
      meta = list("ClassMean"),
      xaxis = if (!is.null(template$xaxis)) template$xaxis else "x",
      yaxis = if (!is.null(template$yaxis)) template$yaxis else "y",
      visible = if (!is.null(template$visible)) template$visible else TRUE,
      showlegend = FALSE,
      marker = marker,
      legendgroup = "ClassMean"
    )
  })
}

#' @noRd
format_samples_replace_trace_blocks <- function(
  traces,
  sample_idx,
  rebuilt_samples,
  mean_idx,
  rebuilt_means
) {
  out <- list()
  sample_first <- if (length(sample_idx) > 0) min(sample_idx) else Inf
  mean_first <- if (length(mean_idx) > 0) min(mean_idx) else Inf

  for (i in seq_along(traces)) {
    if (i == sample_first) {
      out <- c(out, rebuilt_samples)
    }
    if (i == mean_first) {
      out <- c(out, rebuilt_means)
    }
    if (i %in% sample_idx || i %in% mean_idx) {
      next
    }
    out[[length(out) + 1]] <- traces[[i]]
  }

  out
}

#' @noRd
format_samples_should_update_means <- function(x, state) {
  isTRUE(x$meta$x$class.means) && !("cva" %in% class(x))
}

#' Update the stored metadata so future displays inherit the new sample format
#' @noRd
format_samples_compat_meta <- function(state, x, n) {
  primary_kind <- format_samples_primary_kind(state)
  if (is.null(primary_kind)) {
    group <- format_samples_current_group(x, n)
    k <- nlevels(group)
    return(list(
      group = group,
      color = rep(state$defaults$color, length.out = k),
      symbol = rep(state$defaults$symbol, length.out = k),
      pch_numeric = rep(state$defaults$pch_numeric, length.out = k),
      legend_title = "Data"
    ))
  }

  primary_spec <- state[[primary_kind]]
  group <- primary_spec$group
  k <- nlevels(group)

  if (identical(primary_kind, "color")) {
    color <- primary_spec$values
    if (!is.null(state$symbol) && format_samples_groups_identical(state$symbol$group, group)) {
      symbol <- state$symbol$values
      pch_numeric <- state$symbol$pch_numeric
    } else {
      symbol <- rep(state$defaults$symbol, length.out = k)
      pch_numeric <- rep(state$defaults$pch_numeric, length.out = k)
    }
  } else {
    symbol <- primary_spec$values
    pch_numeric <- primary_spec$pch_numeric
    if (!is.null(state$color) && format_samples_groups_identical(state$color$group, group)) {
      color <- state$color$values
    } else {
      color <- rep(state$defaults$color, length.out = k)
    }
  }

  list(
    group = group,
    color = unname(color),
    symbol = unname(symbol),
    pch_numeric = as.integer(unname(pch_numeric)),
    legend_title = primary_spec$legend_title
  )
}

#' Update the stored metadata so future displays inherit the new sample format
#' @noRd
format_samples_update_meta <- function(x, state) {
  is_cva <- "cva" %in% class(x)
  ez_obj <- x$meta$x
  compat <- format_samples_compat_meta(state, x, ez_obj$n)
  mean_spec <- format_samples_mean_spec(state, ez_obj$n)

  x$meta$group <- compat$group
  x$meta$color <- compat$color
  x$meta$symbol <- compat$symbol
  x$meta$sample_format <- state

  if (is_cva) {
    if (is.null(x$meta$model_group)) {
      x$meta$model_group <- ez_obj$group.aes
    }
    ez_obj$display.group.aes <- compat$group
  } else {
    ez_obj$group.aes <- compat$group
  }

  if (is.null(ez_obj$samples)) {
    ez_obj$samples <- list()
  }
  ez_obj$samples$col <- compat$color
  ez_obj$samples$pch <- compat$pch_numeric
  ez_obj$samples$plotly_symbol <- compat$symbol

  if (format_samples_should_update_means(x, state)) {
    if (is.null(ez_obj$means.aes)) {
      ez_obj$means.aes <- list()
    }
    ez_obj$means.aes$col <- mean_spec$colors
    ez_obj$means.aes$pch <- rep(state$defaults$pch_numeric, length.out = length(mean_spec$levels))
    ez_obj$means.aes$plotly_symbol <- mean_spec$symbols
  }

  x$meta$x <- ez_obj
  x
}

#' @noRd
format_samples_or <- function(x, y) {
  if (is.null(x)) y else x
}
