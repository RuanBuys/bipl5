# ─────────────────────────────────────────────────────────────────────────────
# Naming helpers (payload, label, fit-table names from PC indices)
# ─────────────────────────────────────────────────────────────────────────────

#' @noRd
payload_name <- function(pcs) paste0("Payload_", pcs[1], pcs[2])

#' @noRd
pair_label <- function(pcs, prefix = "PC") {
  paste0(prefix, " ", pcs[1], " & ", pcs[2])
}

#' @noRd
ft_name <- function(pcs) paste0("fit_table_", pcs[1], pcs[2])

#' @noRd
ft_label <- function(ft, prefix = "PC") {
  digits <- gsub("fit_table_", "", ft)
  d1 <- substr(digits, 1, 1)
  d2 <- substr(digits, 2, 2)
  paste0(prefix, " ", d1, " & ", d2)
}


# ─────────────────────────────────────────────────────────────────────────────
# S3 Class Constructors
# ─────────────────────────────────────────────────────────────────────────────

#' Create a bipl5_data object
#'
#' @param sample_coordinates Matrix of observation coordinates (Z)
#' @param axes_coordinates List of axis tick coordinate matrices
#' @param translated_axes_coordinates Shift data from TDA translation
#'
#' @return An object of class \code{bipl5_data}
#' @noRd
new_bipl5_data <- function(
  sample_coordinates,
  axes_coordinates,
  translated_axes_coordinates
) {
  obj <- list(
    sample_coordinates = sample_coordinates,
    axes_coordinates = axes_coordinates,
    translated_axes_coordinates = translated_axes_coordinates
  )
  class(obj) <- "bipl5_data"
  obj
}

#' Create a bipl5_payload object
#'
#' Bundles a constructed payload (from the payload_* functions) together with
#' its associated \code{bipl5_data}.
#'
#' @param payload_list The raw payload list produced by the build pipeline
#' @param data A \code{bipl5_data} object
#'
#' @return An object of class \code{bipl5_payload}
#' @noRd
new_bipl5_payload <- function(payload_list, data) {
  payload_list$Data <- data
  class(payload_list) <- "bipl5_payload"
  payload_list
}

#' Create a bipl5_fitmeasures object
#'
#' @param CumPred List of cumulative predictivity traces
#' @param CumAd List of cumulative adequacy traces
#' @param VarExp List of variance explained traces
#' @param Scree List of scree plot traces
#' @param fit_tables Named list of marginal fit table traces (e.g.
#'   \code{list(fit_table_12 = ..., fit_table_13 = ...)})
#'
#' @return An object of class \code{bipl5_fitmeasures}
#' @noRd
new_bipl5_fitmeasures <- function(
  CumPred,
  CumAd,
  VarExp,
  Scree,
  fit_tables
) {
  obj <- list(
    CumPred = CumPred,
    CumAd = CumAd,
    VarExp = VarExp,
    Scree = Scree
  )
  obj <- c(obj, fit_tables)
  class(obj) <- "bipl5_fitmeasures"
  obj
}

#' Create a bipl5_biplot object
#'
#' @param payloads Named list of \code{bipl5_payload} objects
#'   (e.g. \code{list(Payload_12 = ..., Payload_13 = ...)})
#' @param fit_measures A \code{bipl5_fitmeasures} object (or \code{NULL})
#' @param meta List of metadata (biplotEZ objects, aesthetics, pc_info)
#' @param biplot_type Character string for the secondary class, e.g.
#'   \code{"pca"} or \code{"cva"}.
#'
#' @return An object of class \code{c("bipl5_biplot", biplot_type)}
#' @noRd
new_bipl5_biplot <- function(payloads, fit_measures, meta,
                             biplot_type = "pca") {
  obj <- payloads
  obj$fit_measures <- fit_measures
  obj$meta <- meta
  class(obj) <- c("bipl5_biplot", biplot_type)
  obj
}


# ─────────────────────────────────────────────────────────────────────────────
# wrap_bipl5 generic and PCA method
# ─────────────────────────────────────────────────────────────────────────────

#' Convert a biplotEZ object to a bipl5_biplot
#'
#' @param x A biplotEZ biplot object
#'
#' @return An object of class \code{bipl5_biplot}
#' @export
wrap_bipl5 <- function(x) {
  if (x$dim.biplot != 2) {
    stop("wrap_bipl5 only accepts biplots of two dimensions")
  }
  if (length(class(x)) < 2) {
    if (!is.null(x$PCOaxes)) {
      class(x) <- c(class(x), "PCO")
    }
  }
  UseMethod("wrap_bipl5", x)
}


#' Construct a bipl5_biplot from a PCA biplot
#'
#' Builds three payloads for the user's PC pair and two supplementary pairs,
#' along with fit measures.  The user's original PC pair is always first.
#' Plotting is deferred to \code{\link{plot.bipl5_biplot}}.
#'
#' @param x An object of class \code{biplot} from the biplotEZ package with
#'   PCA method applied.
#'
#' @return An object of class \code{bipl5_biplot}
#' @export
#' @method wrap_bipl5 PCA
#' @S3method wrap_bipl5 PCA
#'
#' @examples
#' \dontrun{
#' library(biplotEZ)
#' bp <- biplot(data = iris) |> PCA() |> wrap_bipl5()
#' bp
#' plot(bp)
#' }
wrap_bipl5.PCA <- function(x) {
  # ── Prepare biplotEZ object ───────────────────────────────────────────────
  if (is.null(x$samples)) {
    x <- biplotEZ::samples(x)
  }
  x <- biplotEZ::axes(x)
  x <- biplotEZ::fit.measures(x)
  x$X <- scale(x$X, center = FALSE, scale = 1 / x$sd)
  x$X <- scale(x$X, -x$means, scale = FALSE)

  corr <- is_correlation(x)

  # ── Determine the three PC pairs ──────────────────────────────────────────
  user_pcs <- as.numeric(sort(x$e.vects))
  standard <- list(c(1, 2), c(1, 3), c(2, 3))

  pcs_match <- function(a, b) length(a) == length(b) && all(a == b)

  is_standard <- any(vapply(standard, pcs_match, logical(1), b = user_pcs))

  if (is_standard) {
    others <- standard[!vapply(standard, pcs_match, logical(1), b = user_pcs)]
    all_pairs <- c(list(user_pcs), others)
  } else {
    all_pairs <- list(user_pcs, c(1, 2), c(1, 3))
  }

  # ── Extract display aesthetics ────────────────────────────────────────────
  color <- x$samples$col
  symbol <- pch_to_plotly(x$samples$pch)
  group <- x$group.aes
  if (length(levels(x$group.aes)) == 1) {
    group <- factor(rep("Data", x$n))
  }

  # ── Build all three payloads ──────────────────────────────────────────────
  build_fit_table <- function(ez_obj) {
    tmp <- list(payload = list())
    tmp <- add_table_payload(tmp, x = ez_obj)
    tmp$payload$fit_table
  }

  payloads <- list()
  fit_tables <- list()

  for (i in seq_along(all_pairs)) {
    pcs <- all_pairs[[i]]
    pname <- payload_name(pcs)

    if (pcs_match(pcs, user_pcs)) {
      ez_obj <- x
      incl_poly <- TRUE
    } else {
      ez_obj <- biplotEZ::biplot(
        x$raw.X,
        center = x$center,
        scaled = x$scaled
      ) |>
        biplotEZ::PCA(e.vects = pcs, correlation.biplot = corr) |>
        biplotEZ::axes() |>
        biplotEZ::fit.measures()
      ez_obj$X <- x$X
      incl_poly <- FALSE
    }

    payloads[[pname]] <- build_one_payload(
      ez_obj,
      group,
      color,
      symbol,
      x,
      include_polygons = incl_poly
    )
    fit_tables[[ft_name(pcs)]] <- build_fit_table(ez_obj)
  }

  # ── Build fit measures ────────────────────────────────────────────────────
  fm_payl <- list()
  fm_payl["CumPred"] <- add_axis_pred_payload(fm_payl, x, EZ = TRUE)
  fm_payl["CumAd"] <- add_axis_adeq_payload(fm_payl, x, EZ = TRUE)
  fm_payl["VarExp"] <- add_prop_variance_payload(x)
  fm_payl["Scree"] <- add_scree_payload(x)

  fit_measures <- new_bipl5_fitmeasures(
    CumPred = fm_payl$CumPred,
    CumAd = fm_payl$CumAd,
    VarExp = fm_payl$VarExp,
    Scree = fm_payl$Scree,
    fit_tables = fit_tables
  )

  # ── Build pc_info (single source of truth for downstream code) ──────────
  pc_info <- list()
  for (pcs in all_pairs) {
    pc_info[[payload_name(pcs)]] <- list(
      pcs = pcs,
      label = pair_label(pcs),
      ft_name = ft_name(pcs)
    )
  }

  # ── Store metadata for plot() ─────────────────────────────────────────────
  meta <- list(
    x = x,
    color = color,
    symbol = symbol,
    group = group,
    fit.quality = fit_quality(x$eigenvalues, x$e.vects),
    pc_info = pc_info,
    dim_prefix = "PC"
  )

  new_bipl5_biplot(payloads, fit_measures, meta, biplot_type = "pca")
}


# ─────────────────────────────────────────────────────────────────────────────
# wrap_bipl5.CVA
# ─────────────────────────────────────────────────────────────────────────────

#' Construct a bipl5_biplot from a CVA biplot
#'
#' Builds payloads for the user's CV pair and available supplementary pairs,
#' along with a dropdown menu.  Fit measures are not yet computed for CVA
#' biplots and will be \code{NULL}.
#' Plotting is deferred to \code{\link{plot.bipl5_biplot}}.
#'
#' @param x An object of class \code{biplot} from the biplotEZ package with
#'   CVA method applied.
#'
#' @return An object of class \code{c("bipl5_biplot", "cva")}
#' @export
#' @method wrap_bipl5 CVA
#' @S3method wrap_bipl5 CVA
#'
#' @examples
#' \dontrun{
#' library(biplotEZ)
#' bp <- biplot(iris[, 1:4]) |> CVA(classes = iris[, 5]) |> wrap_bipl5()
#' bp
#' plot(bp)
#' }
wrap_bipl5.CVA <- function(x) {
  # ── Prepare biplotEZ object ───────────────────────────────────────────────
  if (is.null(x$samples)) {
    x <- biplotEZ::samples(x)
  }
  x <- biplotEZ::axes(x)
  x <- biplotEZ::fit.measures(x)
  if (is.null(x$means.aes)) {
    x <- biplotEZ::means(x)
  }

  # ── Un-center/un-scale X so hovertext shows raw values ─────────────────
  if (x$scaled) {
    x$X <- scale(x$X, center = FALSE, scale = 1 / x$sd)
  }
  if (x$center) {
    x$X <- scale(x$X, -x$means, scale = FALSE)
  }

  # ── Determine how many CVs are available ────────────────────────────────
  # For CVA: max canonical variates = min(g - 1, p)
  g <- length(levels(x$group.aes))
  max_cv <- min(g - 1, x$p)

  user_cvs <- as.numeric(sort(x$e.vects))

  # Build standard pairs from available CVs
  all_cvs <- seq_len(max_cv)
  standard <- list()
  if (max_cv >= 2) standard <- c(standard, list(c(1, 2)))
  if (max_cv >= 3) {
    standard <- c(standard, list(c(1, 3)))
    standard <- c(standard, list(c(2, 3)))
  }

  pcs_match <- function(a, b) length(a) == length(b) && all(a == b)

  is_standard <- any(vapply(standard, pcs_match, logical(1), b = user_cvs))

  if (is_standard) {
    others <- standard[!vapply(standard, pcs_match, logical(1), b = user_cvs)]
    all_pairs <- c(list(user_cvs), others)
  } else {
    # User picked a non-standard pair; supplement with available standard pairs
    all_pairs <- c(list(user_cvs), standard)
    # Remove duplicates
    all_pairs <- all_pairs[!duplicated(
      vapply(all_pairs, paste, character(1), collapse = ",")
    )]
  }

  # ── Extract display aesthetics ────────────────────────────────────────────
  color <- x$samples$col
  symbol <- pch_to_plotly(x$samples$pch)
  group <- x$group.aes
  if (length(levels(x$group.aes)) == 1) {
    group <- factor(rep("Data", x$n))
  }

  # ── Build all payloads ────────────────────────────────────────────────────
  payloads <- list()

  for (i in seq_along(all_pairs)) {
    pcs <- all_pairs[[i]]
    pname <- payload_name(pcs)

    if (pcs_match(pcs, user_cvs)) {
      ez_obj <- x
      incl_poly <- TRUE
    } else {
      ez_obj <- biplotEZ::biplot(
        x$raw.X,
        center = x$center,
        scaled = x$scaled
      ) |>
        biplotEZ::CVA(classes = x$group.aes, e.vects = pcs) |>
        biplotEZ::axes() |>
        biplotEZ::fit.measures()
      if (ez_obj$scaled) {
        ez_obj$X <- scale(ez_obj$X, center = FALSE, scale = 1 / ez_obj$sd)
      }
      if (ez_obj$center) {
        ez_obj$X <- scale(ez_obj$X, -ez_obj$means, scale = FALSE)
      }
      incl_poly <- FALSE
    }

    payloads[[pname]] <- build_one_payload(
      ez_obj,
      group,
      color,
      symbol,
      x,
      include_polygons = incl_poly,
      dim_prefix = "CV",
      ax_pred = FALSE,
      vec_dis = FALSE
    )
  }

  # ── No fit measures for CVA (yet) ──────────────────────────────────────
  fit_measures <- NULL

  # ── Build pc_info (single source of truth for downstream code) ──────────
  pc_info <- list()
  for (pcs in all_pairs) {
    pc_info[[payload_name(pcs)]] <- list(
      pcs = pcs,
      label = pair_label(pcs, prefix = "CV"),
      ft_name = ft_name(pcs)
    )
  }

  # ── Store metadata for plot() ─────────────────────────────────────────────
  meta <- list(
    x = x,
    color = color,
    symbol = symbol,
    group = group,
    fit.quality = fit_quality(x$eigenvalues, x$e.vects, dim_prefix = "CV"),
    pc_info = pc_info,
    dim_prefix = "CV"
  )

  new_bipl5_biplot(payloads, fit_measures, meta, biplot_type = "cva")
}


# ─────────────────────────────────────────────────────────────────────────────
# plot.bipl5_biplot
# ─────────────────────────────────────────────────────────────────────────────

#' Plot a bipl5_biplot object
#'
#' Initialises a plotly graph, populates it with the first available payload
#' traces and annotations, then attaches the remaining payloads and fit
#' measures to the JavaScript event handler.
#'
#' @param x A \code{bipl5_biplot} object
#' @param y Ignored (for S3 consistency)
#' @param ... Additional arguments (ignored)
#'
#' @return A plotly htmlwidget
#' @export
#' @method plot bipl5_biplot
plot.bipl5_biplot <- function(x, y = NULL, ...) {
  bp <- x
  ez <- bp$meta$x
  pc_info <- bp$meta$pc_info
  has_fm <- !is.null(bp$fit_measures)
  is_cva <- "cva" %in% class(bp)

  # ── Detect available payloads ──────────────────────────────────────────────
  all_names <- names(pc_info)
  available <- all_names[
    !vapply(all_names, function(k) is.null(bp[[k]]), logical(1))
  ]

  pc_map <- vapply(pc_info[available], function(info) info$label, character(1))
  ft_map <- vapply(
    pc_info[available],
    function(info) info$ft_name,
    character(1)
  )

  use_pc_toggle <- length(available) > 1

  # The first available payload is rendered directly into plotly
  first_name <- available[1]
  first_payl <- bp[[first_name]]

  # ── Step 1: Create plotly scaffolding ──────────────────────────────────────
  p_ly <- plot_scaffolding(
    dpquality = first_payl$fit_qual,
    basis = ez$e.vects,
    PC_toggle = use_pc_toggle,
    ax_pred = has_fm,
    TDA = TRUE,
    vec_dis = !is_cva,
    x_colnames = colnames(ez$X)
  )

  # ── Step 1b: Trim PC dropdown buttons to available payloads ────────────────
  if (use_pc_toggle) {
    pc_buttons <- lapply(seq_along(available), function(i) {
      list(
        method = "skip",
        args = list("type", if (i == 1) "scatter" else "histogram"),
        label = pc_map[available[i]]
      )
    })
    p_ly$x$layoutAttrs[[1]]$updatemenus[[2]]$buttons <- pc_buttons
  }

  # ── Step 2: Add first payload traces to plotly ─────────────────────────────
  for (tr in first_payl$payload$trace_data) {
    p_ly <- do.call(plotly::add_trace, c(list(p = p_ly), tr))
  }

  # ── Step 3: Add first payload annotations ──────────────────────────────────
  if (length(first_payl$payload$layout$annotations) > 0) {
    p_ly <- plotly::layout(
      p_ly,
      annotations = first_payl$payload$layout$annotations
    )
  }

  # ── Step 4: Build payload for JS ───────────────────────────────────────────
  payload_for_js <- list()
  for (nm in available) {
    lbl <- pc_map[nm]
    if (nm == first_name) {
      payload_for_js[[lbl]] <- list(
        config = first_payl$payload$config,
        fit_table = if (has_fm) bp$fit_measures[[ft_map[nm]]]
      )
    } else {
      js_payl <- bp[[nm]]$payload
      if (has_fm) js_payl$fit_table <- bp$fit_measures[[ft_map[nm]]]
      payload_for_js[[lbl]] <- js_payl
    }
  }

  # ── Step 5: Build fit measures payload for JS ──────────────────────────────
  fm_payload <- if (has_fm) {
    list(
      CumPred = bp$fit_measures$CumPred,
      CumAd = bp$fit_measures$CumAd,
      VarExp = bp$fit_measures$VarExp,
      Scree = bp$fit_measures$Scree
    )
  }

  # ── Step 6: Attach JavaScript ──────────────────────────────────────────────
  p_ly <- insert_linear_js_v1(
    p_ly,
    p = ez$p,
    cols = ez$axes$tick.label.col,
    payload = payload_for_js,
    fm_payload = fm_payload,
    initial_pc_key = pc_map[first_name]
  )

  p_ly
}


# ─────────────────────────────────────────────────────────────────────────────
# Print methods (colored tree diagrams)
# ─────────────────────────────────────────────────────────────────────────────

#' Print a bipl5_biplot object as a tree diagram
#'
#' @param x A \code{bipl5_biplot} object
#' @param ... Additional arguments (ignored)
#'
#' @return Invisibly returns \code{x}
#' @export
#' @method print bipl5_biplot
print.bipl5_biplot <- function(x, ...) {
  bold <- crayon::bold
  cyan <- crayon::cyan
  green <- crayon::green
  yellow <- crayon::yellow
  silver <- crayon::silver

  biplot_type <- toupper(class(x)[2])
  cat(bold(paste0("bipl5_biplot [", biplot_type, "]")), "\n")

  pc_info <- x$meta$pc_info
  all_payloads <- names(pc_info)
  all_labels <- vapply(pc_info, function(info) info$label, character(1))

  # Only print non-NULL payloads
  present <- which(
    !vapply(all_payloads, function(k) is.null(x[[k]]), logical(1))
  )

  for (j in seq_along(present)) {
    i <- present[j]
    pname <- all_payloads[i]
    payl <- x[[pname]]
    is_last <- (j == length(present) && is.null(x$fit_measures))
    branch <- if (is_last) "\u2514\u2500\u2500 " else "\u251C\u2500\u2500 "
    pipe <- if (is_last) "    " else "\u2502   "

    cat(
      branch,
      cyan(bold(paste0(pname, " [", all_labels[i], "]"))),
      silver(" <bipl5_payload>"),
      "\n",
      sep = ""
    )

    # Data sub-element
    print_data_subtree(payl$Data, pipe)

    # Traces
    n_traces <- length(payl$payload$trace_data)
    cat(
      pipe,
      "\u251C\u2500\u2500 ",
      green("trace_data"),
      silver(paste0("  [", n_traces, " traces]")),
      "\n",
      sep = ""
    )

    # Annotations
    n_ann <- length(payl$payload$layout$annotations)
    cat(
      pipe,
      "\u2514\u2500\u2500 ",
      green("annotations"),
      silver(paste0("  [", n_ann, " items]")),
      "\n",
      sep = ""
    )
  }

  # Fit measures
  if (!is.null(x$fit_measures)) {
    cat(
      "\u2514\u2500\u2500 ",
      yellow(bold("fit_measures")),
      silver(" <bipl5_fitmeasures>"),
      "\n",
      sep = ""
    )
    print_fitmeasures_subtree(x$fit_measures, "    ")
  }

  invisible(x)
}


#' Print a bipl5_payload object
#'
#' @param x A \code{bipl5_payload} object
#' @param ... Additional arguments (ignored)
#'
#' @return Invisibly returns \code{x}
#' @export
#' @method print bipl5_payload
print.bipl5_payload <- function(x, ...) {
  bold <- crayon::bold
  cyan <- crayon::cyan
  green <- crayon::green
  silver <- crayon::silver

  cat(bold(cyan("bipl5_payload")), "\n")
  if (!is.null(x$fit_qual)) {
    cat(silver(x$fit_qual), "\n")
  }

  # Data
  print_data_subtree(x$Data, "")

  # Traces
  n_traces <- length(x$payload$trace_data)
  cat(
    "\u251C\u2500\u2500 ",
    green("trace_data"),
    silver(paste0("  [", n_traces, " traces]")),
    "\n",
    sep = ""
  )

  # Annotations
  n_ann <- length(x$payload$layout$annotations)
  cat(
    "\u2514\u2500\u2500 ",
    green("annotations"),
    silver(paste0("  [", n_ann, " items]")),
    "\n",
    sep = ""
  )

  invisible(x)
}


#' Print a bipl5_data object
#'
#' @param x A \code{bipl5_data} object
#' @param ... Additional arguments (ignored)
#'
#' @return Invisibly returns \code{x}
#' @export
#' @method print bipl5_data
print.bipl5_data <- function(x, ...) {
  bold <- crayon::bold
  green <- crayon::green
  silver <- crayon::silver

  cat(bold(green("bipl5_data")), "\n")

  # sample_coordinates
  dims <- dim_label(x$sample_coordinates)
  cat("\u251C\u2500\u2500 sample_coordinates", silver(dims), "\n", sep = "")

  # axes_coordinates
  n_ax <- length(x$axes_coordinates)
  cat(
    "\u251C\u2500\u2500 axes_coordinates",
    silver(paste0("  [", n_ax, " axes]")),
    "\n",
    sep = ""
  )

  # translated_axes_coordinates
  cat("\u2514\u2500\u2500 translated_axes_coordinates\n", sep = "")

  invisible(x)
}


#' Print a bipl5_fitmeasures object
#'
#' @param x A \code{bipl5_fitmeasures} object
#' @param ... Additional arguments (ignored)
#'
#' @return Invisibly returns \code{x}
#' @export
#' @method print bipl5_fitmeasures
print.bipl5_fitmeasures <- function(x, ...) {
  bold <- crayon::bold
  yellow <- crayon::yellow
  silver <- crayon::silver

  cat(bold(yellow("bipl5_fitmeasures")), "\n")
  print_fitmeasures_subtree(x, "")

  invisible(x)
}


# ── Print helpers (not exported) ─────────────────────────────────────────────

#' @noRd
print_data_subtree <- function(data, prefix) {
  green <- crayon::green
  silver <- crayon::silver

  cat(
    prefix,
    "\u251C\u2500\u2500 ",
    green("Data"),
    silver(" <bipl5_data>"),
    "\n",
    sep = ""
  )

  inner <- paste0(prefix, "\u2502   ")

  # sample_coordinates
  dims <- dim_label(data$sample_coordinates)
  cat(
    inner,
    "\u251C\u2500\u2500 sample_coordinates",
    silver(dims),
    "\n",
    sep = ""
  )

  # axes_coordinates
  n_ax <- length(data$axes_coordinates)
  cat(
    inner,
    "\u251C\u2500\u2500 axes_coordinates",
    silver(paste0("  [", n_ax, " axes]")),
    "\n",
    sep = ""
  )

  # translated_axes_coordinates
  cat(inner, "\u2514\u2500\u2500 translated_axes_coordinates\n", sep = "")
}

#' @noRd
print_fitmeasures_subtree <- function(fm, prefix) {
  silver <- crayon::silver
  green <- crayon::green

  # Chart-based measures
  nms <- c("CumPred", "CumAd", "VarExp", "Scree")
  for (nm in nms) {
    n_tr <- length(fm[[nm]])
    cat(
      prefix,
      "\u251C\u2500\u2500 ",
      nm,
      silver(paste0("  [", n_tr, " traces]")),
      "\n",
      sep = ""
    )
  }

  # Marginal fit tables (discover dynamically from names)
  all_ft <- grep("^fit_table_", names(fm), value = TRUE)
  present <- all_ft[!vapply(all_ft, function(k) is.null(fm[[k]]), logical(1))]

  for (j in seq_along(present)) {
    is_last <- (j == length(present))
    branch <- if (is_last) "\u2514\u2500\u2500 " else "\u251C\u2500\u2500 "
    cat(
      prefix,
      branch,
      green(present[j]),
      silver(paste0("  [", ft_label(present[j]), "]")),
      "\n",
      sep = ""
    )
  }
}

#' @noRd
dim_label <- function(mat) {
  if (is.matrix(mat) || is.data.frame(mat)) {
    paste0("  [", nrow(mat), " x ", ncol(mat), "]")
  } else if (is.vector(mat)) {
    paste0("  [", length(mat), "]")
  } else {
    ""
  }
}


# ─────────────────────────────────────────────────────────────────────────────
# subset_biplot – internal helper for payload subsetting
# ─────────────────────────────────────────────────────────────────────────────

#' Subset a bipl5_biplot to keep only specified payloads
#'
#' @param bp A \code{bipl5_biplot} object
#' @param keep Character vector of payload names to retain
#'   (e.g. \code{"Payload_12"} or \code{c("Payload_12", "Payload_23")})
#'
#' @return A new \code{bipl5_biplot} with only the specified payloads and
#'   their corresponding fit tables
#' @noRd
subset_biplot <- function(bp, keep) {
  pc_info <- bp$meta$pc_info
  valid <- names(pc_info)
  bad <- setdiff(keep, valid)
  if (length(bad) > 0) {
    stop(
      "Unknown payload(s): ",
      paste(bad, collapse = ", "),
      ". Must be one of: ",
      paste(valid, collapse = ", "),
      call. = FALSE
    )
  }

  # Subset payloads
  new_payloads <- list()
  for (nm in keep) {
    new_payloads[[nm]] <- bp[[nm]]
  }

  # Subset fit tables (skip if no fit measures, e.g. CVA)
  fm <- bp$fit_measures
  if (!is.null(fm)) {
    new_ft <- list()
    for (nm in keep) {
      ft <- pc_info[[nm]]$ft_name
      new_ft[[ft]] <- fm[[ft]]
    }
    new_fm <- new_bipl5_fitmeasures(
      CumPred = fm$CumPred,
      CumAd = fm$CumAd,
      VarExp = fm$VarExp,
      Scree = fm$Scree,
      fit_tables = new_ft
    )
  } else {
    new_fm <- NULL
  }

  # Subset pc_info in meta
  new_meta <- bp$meta
  new_meta$pc_info <- pc_info[keep]

  # Preserve the secondary class (pca/cva)
  biplot_type <- class(bp)[2]
  new_bipl5_biplot(new_payloads, new_fm, new_meta, biplot_type = biplot_type)
}


# ─────────────────────────────────────────────────────────────────────────────
# extract() – drill into a bipl5_biplot with bare names
# ─────────────────────────────────────────────────────────────────────────────

#' Extract nested components from a bipl5_biplot object
#'
#' Three calling styles are supported:
#' \enumerate{
#'   \item \strong{Payload subset}: \code{extract(bp, Payload_12)} — returns a
#'     new \code{bipl5_biplot} containing only that payload (plottable).
#'   \item \strong{Two-level}: \code{extract(bp, from = Payload_12, what = sample_coordinates)}
#'     — returns the nested data element.
#'   \item \strong{Arbitrary depth}: \code{extract(bp, Payload_12$Data$sample_coordinates)}
#'     — returns the nested data element.
#' }
#'
#' @param object A \code{bipl5_biplot} object
#' @param expr An unquoted payload name (e.g. \code{Payload_12}) or a path
#'   expression using \code{$} (e.g. \code{Payload_12$Data$sample_coordinates})
#' @param from Unquoted name of the top-level element
#' @param what Unquoted name of the nested element
#'
#' @return A \code{bipl5_biplot} (payload subset) or the requested sub-element
#' @export
extract <- function(object, ...) {
  UseMethod("extract")
}

#' @rdname extract
#' @export
#' @method extract bipl5_biplot
extract.bipl5_biplot <- function(object, expr, from, what) {
  payload_names <- names(object$meta$pc_info)

  # Determine which style was used
  if (!missing(from) && !missing(what)) {
    from_chr <- as.character(substitute(from))
    what_chr <- as.character(substitute(what))
    return(object[[from_chr]][[what_chr]])
  }

  if (!missing(expr)) {
    e <- substitute(expr)

    # Single bare symbol matching a payload name → subset biplot
    if (is.symbol(e)) {
      nm <- as.character(e)
      if (nm %in% payload_names) {
        return(subset_biplot(object, nm))
      }
    }

    # Otherwise: arbitrary depth via $ expression
    path <- deparse_path(e)
    result <- object
    for (field in path) {
      if (is.null(result[[field]])) {
        stop("Field '", field, "' not found at this level.", call. = FALSE)
      }
      result <- result[[field]]
    }
    return(result)
  }

  stop("Provide either (from, what) or a single $ expression.", call. = FALSE)
}

#' Walk a $ expression into a character path
#' @noRd
deparse_path <- function(expr) {
  if (is.symbol(expr)) {
    return(as.character(expr))
  }
  if (is.call(expr) && identical(expr[[1]], as.symbol("$"))) {
    return(c(deparse_path(expr[[2]]), deparse_path(expr[[3]])))
  }
  stop(
    "extract() expects a path like Payload_12$Data$sample_coordinates",
    call. = FALSE
  )
}


# ─────────────────────────────────────────────────────────────────────────────
# remove_payload() – drop a payload from a bipl5_biplot
# ─────────────────────────────────────────────────────────────────────────────

#' Remove a payload from a bipl5_biplot object
#'
#' Returns a new \code{bipl5_biplot} with the specified payload (and its
#' corresponding fit table) removed.  At least one payload must remain.
#'
#' @param object A \code{bipl5_biplot} object
#' @param payload Unquoted name of the payload to remove
#'   (e.g. \code{Payload_13})
#'
#' @return A new \code{bipl5_biplot} without the removed payload
#' @export
remove_payload <- function(object, ...) {
  UseMethod("remove_payload")
}

#' @rdname remove_payload
#' @export
#' @method remove_payload bipl5_biplot
#' @S3method remove_payload bipl5_biplot
remove_payload.bipl5_biplot <- function(object, payload) {
  nm <- as.character(substitute(payload))
  all_payloads <- names(object$meta$pc_info)

  if (!nm %in% all_payloads) {
    stop(
      "'",
      nm,
      "' is not a valid payload name. ",
      "Must be one of: ",
      paste(all_payloads, collapse = ", "),
      call. = FALSE
    )
  }
  if (is.null(object[[nm]])) {
    stop("Payload '", nm, "' does not exist in this object.", call. = FALSE)
  }

  keep <- setdiff(all_payloads, nm)
  keep <- keep[!vapply(keep, function(k) is.null(object[[k]]), logical(1))]

  if (length(keep) == 0) {
    stop("Cannot remove the last remaining payload.", call. = FALSE)
  }

  subset_biplot(object, keep)
}


# ─────────────────────────────────────────────────────────────────────────────
# append_payload() – add a new PC pair to an existing bipl5_biplot
# ─────────────────────────────────────────────────────────────────────────────

#' Append a payload to a bipl5_biplot object
#'
#' Adds a new biplot layer for a specified pair of principal components.
#' The pair is sorted automatically (e.g. \code{c(5, 3)} becomes
#' \code{c(3, 5)}).  Both PC indices must be between 1 and \code{p}
#' (the number of variables), and the pair must not already exist.
#'
#' @param object A \code{bipl5_biplot} object
#' @param eigenvectors Integer vector of length 2 giving the PC pair
#'   (e.g. \code{c(4, 5)})
#'
#' @return A new \code{bipl5_biplot} with the additional payload appended
#' @export
append_payload <- function(object, ...) {
  UseMethod("append_payload")
}

#' @rdname append_payload
#' @export
#' @method append_payload bipl5_biplot
#' @S3method append_payload bipl5_biplot
append_payload.bipl5_biplot <- function(object, eigenvectors) {
  # ── Validate input ────────────────────────────────────────────────────────
  if (!is.numeric(eigenvectors) || length(eigenvectors) != 2) {
    stop("eigenvectors must be a numeric vector of length 2.", call. = FALSE)
  }

  pcs <- as.numeric(sort(eigenvectors))
  ez <- object$meta$x
  p <- ez$p

  if (any(pcs < 1) || any(pcs > p)) {
    stop(
      "eigenvectors must be between 1 and ",
      p,
      " (the number of variables).",
      call. = FALSE
    )
  }
  if (pcs[1] == pcs[2]) {
    stop("eigenvectors must contain two different PC indices.", call. = FALSE)
  }

  pname <- payload_name(pcs)
  if (pname %in% names(object$meta$pc_info)) {
    stop(
      pname,
      " already exists in this object. ",
      "Existing payloads: ",
      paste(names(object$meta$pc_info), collapse = ", "),
      call. = FALSE
    )
  }

  # ── Build the new payload ─────────────────────────────────────────────────
  biplot_type <- class(object)[2]
  dim_prefix <- if (!is.null(object$meta$dim_prefix)) {
    object$meta$dim_prefix
  } else {
    "PC"
  }
  is_cva <- biplot_type == "cva"

  if (is_cva) {
    ez_obj <- biplotEZ::biplot(
      ez$raw.X,
      center = ez$center,
      scaled = ez$scaled
    ) |>
      biplotEZ::CVA(classes = ez$group.aes, e.vects = pcs) |>
      biplotEZ::axes() |>
      biplotEZ::fit.measures()
    if (ez_obj$scaled) {
      ez_obj$X <- scale(ez_obj$X, center = FALSE, scale = 1 / ez_obj$sd)
    }
    if (ez_obj$center) {
      ez_obj$X <- scale(ez_obj$X, -ez_obj$means, scale = FALSE)
    }
  } else {
    corr <- is_correlation(ez)
    ez_obj <- biplotEZ::biplot(
      ez$raw.X,
      center = ez$center,
      scaled = ez$scaled
    ) |>
      biplotEZ::PCA(e.vects = pcs, correlation.biplot = corr) |>
      biplotEZ::axes() |>
      biplotEZ::fit.measures()
    ez_obj$X <- ez$X
  }

  new_payl <- build_one_payload(
    ez_obj,
    group = object$meta$group,
    color = object$meta$color,
    symbol = object$meta$symbol,
    x_ref = ez,
    include_polygons = FALSE,
    dim_prefix = dim_prefix,
    ax_pred = !is_cva,
    vec_dis = !is_cva
  )

  # ── Append to existing object ─────────────────────────────────────────────
  # Copy all existing payloads
  all_payloads <- list()
  for (nm in names(object$meta$pc_info)) {
    all_payloads[[nm]] <- object[[nm]]
  }
  all_payloads[[pname]] <- new_payl

  # Build new fit_measures with the extra table (skip for CVA)
  fm <- object$fit_measures
  if (!is.null(fm)) {
    tmp <- list(payload = list())
    tmp <- add_table_payload(tmp, x = ez_obj)
    new_ft <- tmp$payload$fit_table

    old_ft <- list()
    for (nm in grep("^fit_table_", names(fm), value = TRUE)) {
      old_ft[[nm]] <- fm[[nm]]
    }
    old_ft[[ft_name(pcs)]] <- new_ft

    new_fm <- new_bipl5_fitmeasures(
      CumPred = fm$CumPred,
      CumAd = fm$CumAd,
      VarExp = fm$VarExp,
      Scree = fm$Scree,
      fit_tables = old_ft
    )
  } else {
    new_fm <- NULL
  }

  # Extend pc_info
  new_pc_info <- object$meta$pc_info
  new_pc_info[[pname]] <- list(
    pcs = pcs,
    label = pair_label(pcs, prefix = dim_prefix),
    ft_name = ft_name(pcs)
  )

  new_meta <- object$meta
  new_meta$pc_info <- new_pc_info

  new_bipl5_biplot(all_payloads, new_fm, new_meta, biplot_type = biplot_type)
}
