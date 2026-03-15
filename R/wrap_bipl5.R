# ─────────────────────────────────────────────────────────────────────────────
# Naming helpers (payload, label, fit-table names from PC indices)
# ─────────────────────────────────────────────────────────────────────────────

#' Build the canonical payload name for a dimension pair
#'
#' Payloads are stored under stable names such as \code{Payload_12} and
#' \code{Payload_45}. This helper centralises that convention so the same
#' identifier is reused when payloads are constructed, subsetted, appended,
#' removed, and registered in \code{meta$pc_info}.
#'
#' @param pcs Integer vector of length 2 giving the selected dimension pair.
#'   Callers are expected to supply the pair in sorted order.
#'
#' @return A character scalar of the form \code{"Payload_ij"}.
#' @noRd
payload_name <- function(pcs) paste0("Payload_", pcs[1], pcs[2])

#' Build the user-facing label for a dimension pair
#'
#' Converts an index pair such as \code{c(1, 3)} into the label shown in the
#' dropdown menu and fit-quality text, for example \code{"PC 1 & 3"} or
#' \code{"CV 1 & 3"}.
#'
#' @param pcs Integer vector of length 2 giving the selected dimension pair.
#' @param prefix Label prefix used for the basis family, typically
#'   \code{"PC"} or \code{"CV"}.
#'
#' @return A character scalar suitable for UI display.
#' @noRd
pair_label <- function(pcs, prefix = "PC") {
  paste0(prefix, " ", pcs[1], " & ", pcs[2])
}

#' Build the fit-table storage key for a dimension pair
#'
#' Fit tables are stored separately from the main payload traces and are keyed
#' by names such as \code{fit_table_12}. This helper keeps that mapping
#' consistent with the payload naming convention.
#'
#' @param pcs Integer vector of length 2 giving the selected dimension pair.
#'
#' @return A character scalar of the form \code{"fit_table_ij"}.
#' @noRd
ft_name <- function(pcs) paste0("fit_table_", pcs[1], pcs[2])

#' Convert a fit-table storage key back to a display label
#'
#' This is used primarily by the print methods when rendering a tree view of a
#' \code{bipl5_fitmeasures} object. For example, \code{fit_table_23} becomes
#' \code{"PC 2 & 3"}.
#'
#' @param ft Character scalar of the form \code{"fit_table_ij"}.
#' @param prefix Label prefix used for the basis family, typically
#'   \code{"PC"} or \code{"CV"}.
#'
#' @return A user-facing label corresponding to \code{ft}.
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
#' A \code{bipl5_data} object stores the numeric data behind one payload. It is
#' used for inspection via \code{extract()}, for the tree-style print methods,
#' and as a stable container for coordinates that correspond to the rendered
#' plotly traces.
#'
#' @param sample_coordinates Numeric matrix of observation coordinates in the
#'   current two-dimensional biplot space (\code{ez_obj$Z}).
#' @param axes_coordinates List of per-variable axis coordinate objects returned
#'   by \code{biplotEZ::axes_coordinates()} for the same dimension pair.
#' @param translated_axes_coordinates Translation metadata returned by the TDA
#'   builder and used to recover translated-axis positions.
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
#' Bundles a constructed payload together with its associated
#' \code{bipl5_data}. The \code{payload_list} supplied here is expected to be
#' the output of the full build pipeline: a list containing the plotly-ready
#' payload, fit-quality text, translated-axis metadata, and any slider state.
#' This constructor simply attaches the \code{Data} node and assigns the class.
#'
#' @param payload_list The raw list produced by the payload build pipeline,
#'   typically containing \code{payload}, \code{fit_qual}, \code{m}, and
#'   \code{shift}.
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
#' Collects the plotly traces used by the optional right-hand-side fit panel in
#' PCA biplots. The chart-based measures are shared across payloads, while the
#' marginal summary tables are keyed by the corresponding PC pair.
#'
#' @param CumPred List of cumulative predictivity traces.
#' @param CumAd List of cumulative adequacy traces.
#' @param VarExp List of proportion-variance-explained traces.
#' @param Scree List of scree-plot traces.
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
#' This is the top-level container returned by \code{wrap_bipl5()}. Payloads
#' are stored as top-level fields, with \code{fit_measures} and \code{meta}
#' added alongside them. The order of entries in \code{meta$pc_info} is
#' significant: downstream code treats the first registered payload as the
#' initial view shown to the user.
#'
#' @param payloads Named list of \code{bipl5_payload} objects
#'   (e.g. \code{list(Payload_12 = ..., Payload_13 = ...)})
#' @param fit_measures A \code{bipl5_fitmeasures} object (or \code{NULL} for
#'   CVA biplots).
#' @param meta List of metadata required downstream by \code{plot()},
#'   \code{print()}, \code{extract()}, \code{remove_payload()}, and
#'   \code{append_payload()}. At minimum this contains the source biplotEZ
#'   object, display aesthetics, \code{fit.quality}, \code{pc_info}, and the
#'   basis prefix.
#' @param biplot_type Character string for the secondary class, e.g.
#'   \code{"pca"} or \code{"cva"}.
#'
#' @return An object of class \code{c("bipl5_biplot", biplot_type)}
#' @noRd
new_bipl5_biplot <- function(
  payloads,
  fit_measures,
  meta,
  biplot_type = "pca"
) {
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
  # Ensure regression/PCO biplots dispatch correctly, not to .PCA
  if ("regress" %in% class(x) || "PCO" %in% class(x)) {
    class(x) <- setdiff(class(x), "PCA")
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
  fm_payl["CumPred"] <- add_axis_pred_payload(fm_payl, x)
  fm_payl["CumAd"] <- add_axis_adeq_payload(fm_payl, x)
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
  if (max_cv >= 2) {
    standard <- c(standard, list(c(1, 2)))
  }
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
    all_pairs <- all_pairs[
      !duplicated(
        vapply(all_pairs, paste, character(1), collapse = ",")
      )
    ]
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


#' Construct a bipl5_biplot from a regression biplot
#'
#' Builds the single payload used for a linear regression biplot and documents
#' the associated regression-biplot predictivity measures.
#' Regression biplots do not use the multi-payload fit machinery available for
#' PCA/CVA displays: they have one fixed payload (\code{Payload_12}),
#' \code{append_payload()} and \code{remove_payload()} are not supported,
#' and the only active toggle button is \dQuote{Translated Axes}.
#'
#' @param x An object of class \code{biplot} from the \pkg{biplotEZ} package with
#'   \code{regress()} method applied.
#'
#' @details
#' For the linear regression biplot handled by this method, let
#' \eqn{\mathbf{X}\in\mathbb{R}^{n\times p}} denote the \emph{processed} data
#' matrix stored in the \code{biplot} object after centring and any optional
#' scaling performed by \code{biplot()}, and let
#' \eqn{\mathbf{Z}\in\mathbb{R}^{n\times 2}} denote the externally supplied
#' display coordinates of the \eqn{n} samples. Write
#' \eqn{\mathbf{Z} = [\mathbf{z}_1\ \mathbf{z}_2]}, where
#' \eqn{\mathbf{z}_1} and \eqn{\mathbf{z}_2} are the first and second displayed
#' coordinates respectively. In contrast to a PCA biplot, the sample map is taken
#' as given and the variable axes are then fitted to that map by multivariate
#' least squares. This is the regression-biplot point of view used in the biplot
#' literature for general low-dimensional sample maps (Gower and Hand, 1996;
#' Gower, Lubbe and le Roux, 2011).
#'
#' The fitted linear model is
#' \deqn{\mathbf{X} = \mathbf{Z}\mathbf{H}^{\top} + \mathbf{E},}
#' where, when \eqn{\mathbf{Z}} has full column rank,
#' \deqn{\mathbf{H}^{\top} =
#'   (\mathbf{Z}^{\top}\mathbf{Z})^{-1}\mathbf{Z}^{\top}\mathbf{X}.}
#' Hence the fitted values are
#' \deqn{\widehat{\mathbf{X}} =
#'   \mathbf{Z}\mathbf{H}^{\top} =
#'   \mathbf{Z}(\mathbf{Z}^{\top}\mathbf{Z})^{-1}\mathbf{Z}^{\top}\mathbf{X}
#'   = \mathbf{P}_Z\mathbf{X},}
#' where \eqn{\mathbf{P}_Z} is the orthogonal projector onto the column space
#' of \eqn{\mathbf{Z}}. More generally, if the supplied display coordinates are
#' rank-deficient, the same fitted matrix \eqn{\widehat{\mathbf{X}}} is obtained
#' by interpreting \eqn{\mathbf{P}_Z} as the orthogonal projector onto
#' \eqn{\mathrm{col}(\mathbf{Z})}. The regression biplot therefore displays the
#' variables through the least-squares predictions obtained from the supplied
#' 2D sample map (Gower and Hand, 1996; Gower, Lubbe and le Roux, 2011).
#'
#' If \eqn{\mathbf{h}_{(j)}} denotes the \eqn{j}th column of
#' \eqn{\mathbf{H}}, then the predicted value of variable \eqn{j} for sample
#' \eqn{i} is
#' \deqn{\widehat{x}_{ij} = \mathbf{z}_i^{\top}\mathbf{h}_{(j)}.}
#' The calibrated axis for variable \eqn{j} has direction
#' \eqn{\mathbf{h}_{(j)}}, and the point on that axis corresponding to marker
#' value \eqn{\mu} is
#' \deqn{\mathbf{p}_{\mu j} =
#'   \frac{\mu}{\mathbf{h}_{(j)}^{\top}\mathbf{h}_{(j)}}\mathbf{h}_{(j)}.}
#' This is the calibration formula used to place tick marks and to recover
#' predicted values from projections onto the displayed axis, in direct analogy
#' with calibrated-axis biplot constructions (Gabriel, 1971; Gower, Lubbe and
#' le Roux, 2011). All such predicted values are on the same centred/scaled scale
#' as the stored matrix \eqn{\mathbf{X}}; if needed, they can be back-transformed
#' to the original variable scale using the means and standard deviations stored
#' in the input \code{biplot} object.
#'
#' A regression biplot admits a natural family of \emph{predictivity} measures on
#' the variable side. Let \eqn{\mathbf{x}_{(j)}} denote column \eqn{j} of
#' \eqn{\mathbf{X}}, let \eqn{\widehat{\mathbf{x}}_{(j)}} denote column
#' \eqn{j} of \eqn{\widehat{\mathbf{X}}}, and let
#' \eqn{\mathbf{e}_{(j)} = \mathbf{x}_{(j)} - \widehat{\mathbf{x}}_{(j)}}.
#' Since \eqn{\widehat{\mathbf{X}} = \mathbf{P}_Z\mathbf{X}} is an orthogonal
#' projection, the residual matrix satisfies
#' \deqn{\widehat{\mathbf{X}}^{\top}\mathbf{E} = \mathbf{0},}
#' and therefore
#' \deqn{\mathbf{X}^{\top}\mathbf{X} =
#'   \widehat{\mathbf{X}}^{\top}\widehat{\mathbf{X}} +
#'   (\mathbf{X} - \widehat{\mathbf{X}})^{\top}
#'   (\mathbf{X} - \widehat{\mathbf{X}}).}
#' This is the variable-side, or Type B, orthogonality that justifies
#' variance-accounted-for ratios for the columns of \eqn{\mathbf{X}}; it is the
#' same side of the orthogonality argument that underlies column-wise
#' predictivities in the biplot literature (Gower, Lubbe and le Roux, 2011;
#' Greenacre, 2010).
#'
#' The predictivity of variable \eqn{j} is therefore defined by
#' \deqn{\phi_j =
#'   \frac{\|\widehat{\mathbf{x}}_{(j)}\|^2}
#'        {\|\mathbf{x}_{(j)}\|^2}
#'   =
#'   1 -
#'   \frac{\|\mathbf{x}_{(j)} - \widehat{\mathbf{x}}_{(j)}\|^2}
#'        {\|\mathbf{x}_{(j)}\|^2},
#'   \qquad j=1,\ldots,p.}
#' Thus \eqn{\phi_j} is the proportion of the sum of squares of variable
#' \eqn{j} reproduced by the regression biplot, equivalently the ordinary
#' multiple-regression \eqn{R^2} obtained by regressing variable \eqn{j} on the
#' displayed coordinates \eqn{\mathbf{Z}}. Each \eqn{\phi_j} lies in
#' \eqn{[0,1]}; values near one indicate that the variable is well predicted by
#' the displayed map, while values near zero indicate that the variable is poorly
#' reproduced by the chosen display (Greenacre, 2010).
#'
#' A natural overall quality-of-display measure is the proportion of total sum
#' of squares reproduced by the display,
#' \deqn{R^2_{\mathrm{disp}} =
#'   \frac{\|\widehat{\mathbf{X}}\|_F^2}{\|\mathbf{X}\|_F^2}
#'   =
#'   1 - \frac{\|\mathbf{X} - \widehat{\mathbf{X}}\|_F^2}{\|\mathbf{X}\|_F^2}.}
#' Because the column-wise decomposition above is orthogonal, this overall
#' quality can be written as a weighted average of the variable predictivities:
#' \deqn{R^2_{\mathrm{disp}} =
#'   \sum_{j=1}^{p} w_j \phi_j,}
#' where
#' \deqn{w_j =
#'   \frac{\|\mathbf{x}_{(j)}\|^2}{\|\mathbf{X}\|_F^2},
#'   \qquad
#'   \sum_{j=1}^{p} w_j = 1.}
#' Hence variables with larger sums of squares contribute more to the overall
#' quality. In particular, if the original call to \code{biplot()} used
#' \code{scale = TRUE}, so that all processed variables have equal sums of
#' squares, then the weights are equal and
#' \deqn{R^2_{\mathrm{disp}} = \frac{1}{p}\sum_{j=1}^{p}\phi_j.}
#' This weighted-average interpretation is often the most natural way to read the
#' overall regression-biplot quality, since it combines the separate variable
#' predictivities into a single display-wide summary (Greenacre, 2010).
#'
#' The quantities \eqn{\phi_j} and \eqn{R^2_{\mathrm{disp}}} depend only on the
#' fitted projection \eqn{\mathbf{P}_Z\mathbf{X}} and therefore only on the
#' subspace \eqn{\mathrm{col}(\mathbf{Z})}. They do \emph{not} depend on any
#' particular basis chosen for that subspace. In particular, the variable
#' predictivities \eqn{\phi_j} do not require any QR decomposition.
#'
#' To decompose the total display quality into separate contributions for the two
#' displayed dimensions, this package applies an \emph{ordered orthogonalization}
#' of the supplied display coordinates. Specifically, define
#' \deqn{\mathbf{u}_1 = \mathbf{z}_1, \qquad
#'   \mathbf{q}_1 = \frac{\mathbf{u}_1}{\|\mathbf{u}_1\|}}
#' whenever \eqn{\mathbf{u}_1 \neq \mathbf{0}}, and then define
#' \deqn{\mathbf{u}_2 =
#'   \mathbf{z}_2 - \mathbf{q}_1\mathbf{q}_1^{\top}\mathbf{z}_2, \qquad
#'   \mathbf{q}_2 = \frac{\mathbf{u}_2}{\|\mathbf{u}_2\|}}
#' whenever \eqn{\mathbf{u}_2 \neq \mathbf{0}}. Equivalently,
#' \eqn{\mathbf{Q} = [\mathbf{q}_1\ \mathbf{q}_2]} is obtained from the QR
#' decomposition of \eqn{\mathbf{Z}}, preserving the supplied column order. The
#' vectors \eqn{\mathbf{q}_1} and \eqn{\mathbf{q}_2} are orthonormal and span
#' the same display subspace as the nonzero columns of \eqn{\mathbf{Z}}.
#'
#' Because \eqn{\mathbf{Q}} and \eqn{\mathbf{Z}} span the same subspace, the
#' orthogonal projector may also be written as
#' \deqn{\mathbf{P}_Z = \mathbf{Q}\mathbf{Q}^{\top}.}
#' Consequently,
#' \deqn{\widehat{\mathbf{X}} =
#'   \mathbf{Q}\mathbf{Q}^{\top}\mathbf{X}
#'   = \mathbf{q}_1\mathbf{q}_1^{\top}\mathbf{X}
#'   + \mathbf{q}_2\mathbf{q}_2^{\top}\mathbf{X}}
#' whenever both orthogonalized directions are present. Since
#' \eqn{\mathbf{q}_1^{\top}\mathbf{q}_2 = 0}, the two fitted parts are
#' orthogonal and their sums of squares add. This yields the dimension-specific
#' contributions
#' \deqn{R^2_1 =
#'   \frac{\|\mathbf{q}_1\mathbf{q}_1^{\top}\mathbf{X}\|_F^2}
#'        {\|\mathbf{X}\|_F^2},}
#' and
#' \deqn{R^2_{2\mid 1} =
#'   \frac{\|\mathbf{q}_2\mathbf{q}_2^{\top}\mathbf{X}\|_F^2}
#'        {\|\mathbf{X}\|_F^2},}
#' so that
#' \deqn{R^2_{\mathrm{disp}} = R^2_1 + R^2_{2\mid 1}}
#' whenever the display space is two-dimensional.
#'
#' Care should be taken when interpreting this decomposition. If the columns of
#' \eqn{\mathbf{Z}} are already orthogonal, then the two displayed contributions
#' correspond directly to the first and second supplied display axes. If the
#' columns of \eqn{\mathbf{Z}} are not orthogonal, however, the decomposition is
#' \emph{ordered}. The first contribution \eqn{R^2_1} is attributable to the
#' first supplied display coordinate \eqn{\mathbf{z}_1}. The second contribution
#' \eqn{R^2_{2\mid 1}} is attributable to the component of the second supplied
#' display coordinate \eqn{\mathbf{z}_2} that is orthogonal to the first.
#' Thus \eqn{R^2_{2\mid 1}} should be interpreted as the additional contribution
#' of \dQuote{Dim 2 given Dim 1}, not as the contribution of the raw second
#' column of \eqn{\mathbf{Z}} considered in isolation. The ordering of the
#' columns of \eqn{\mathbf{Z}} is therefore important for this decomposition.
#'
#' The same ordered orthogonalization yields a decomposition of each variable's
#' predictivity:
#' \deqn{\phi_j = \phi_{j1} + \phi_{j,2\mid 1},}
#' where
#' \deqn{\phi_{j1} =
#'   \frac{\|\mathbf{q}_1\mathbf{q}_1^{\top}\mathbf{x}_{(j)}\|^2}
#'        {\|\mathbf{x}_{(j)}\|^2}
#'   =
#'   \frac{(\mathbf{q}_1^{\top}\mathbf{x}_{(j)})^2}
#'        {\|\mathbf{x}_{(j)}\|^2},}
#' and
#' \deqn{\phi_{j,2\mid 1} =
#'   \frac{\|\mathbf{q}_2\mathbf{q}_2^{\top}\mathbf{x}_{(j)}\|^2}
#'        {\|\mathbf{x}_{(j)}\|^2}
#'   =
#'   \frac{(\mathbf{q}_2^{\top}\mathbf{x}_{(j)})^2}
#'        {\|\mathbf{x}_{(j)}\|^2}.}
#' Thus \eqn{\phi_{j1}} is the part of variable \eqn{j}'s predictivity explained
#' by the first supplied display dimension, while
#' \eqn{\phi_{j,2\mid 1}} is the additional part explained by the second display
#' dimension after removing its overlap with the first.
#'
#' If the supplied display coordinates are collinear, then
#' \eqn{\mathbf{u}_2 = \mathbf{0}} and the effective display space is
#' one-dimensional. In that case \eqn{R^2_{2\mid 1} = 0} and
#' \eqn{\phi_{j,2\mid 1} = 0} for all variables.
#'
#' In contrast, a regression biplot does \emph{not} in general satisfy the
#' sample-side decomposition
#' \deqn{\mathbf{X}\mathbf{X}^{\top} =
#'   \widehat{\mathbf{X}}\widehat{\mathbf{X}}^{\top} +
#'   (\mathbf{X} - \widehat{\mathbf{X}})
#'   (\mathbf{X} - \widehat{\mathbf{X}})^{\top}.}
#' Consequently, PCA-style sample predictivities are not generally justified for
#' a regression biplot. The principled fit measures are the variable
#' predictivities \eqn{\phi_j}, the overall quality
#' \eqn{R^2_{\mathrm{disp}}}, and the ordered dimension-specific contributions
#' described above. This mirrors the fact that the clean orthogonality available
#' here is on the variable side rather than the sample side (Gower, Lubbe and
#' le Roux, 2011; Greenacre, 2010).
#'
#' In the wrapped \code{bipl5_biplot} object, these formulas drive the bottom
#' display-quality label, the hover-time predicted values
#' \eqn{\widehat{\mathbf{X}}}, and the calibrated linear axes stored in
#' \code{Payload_12}. Since the regression display is tied to one externally
#' supplied map, \code{wrap_bipl5.regress()} produces a single payload only.
#' There is no PC/CV toggle and no separate PCA-style sample-fit panel.
#'
#' @references
#' Gabriel, K. R. (1971). The biplot graphical display of matrices with
#' application to principal component analysis. \emph{Biometrika},
#' 58(3), 453--467. \doi{10.1093/biomet/58.3.453}
#'
#' Gower, J. C. and Hand, D. J. (1996). \emph{Biplots}. London:
#' Chapman \& Hall.
#'
#' Gower, J. C., Lubbe, S. and le Roux, N. J. (2011).
#' \emph{Understanding Biplots}. Chichester: Wiley.
#'
#' Greenacre, M. (2010). \emph{Biplots in Practice}. Bilbao:
#' BBVA Foundation.
#'
#' la Grange, A., le Roux, N. and Gardner-Lubbe, S. (2009).
#' BiplotGUI: Interactive Biplots in R. \emph{Journal of Statistical Software},
#' 30(12), 1--37. \doi{10.18637/jss.v030.i12}
#'
#' @return An object of class \code{c("bipl5_biplot", "reg")}
#' @export
#' @method wrap_bipl5 regress
#'
#' @examples
#' \dontrun{
#' library(biplotEZ)
#' bp <- biplot(iris[, 1:4]) |>
#'   regress(Z = prcomp(iris[, 1:4])$x[, 1:2], group.aes = iris[, 5]) |>
#'   wrap_bipl5()
#' bp
#' plot(bp)
#' }
wrap_bipl5.regress <- function(x) {
  # ── Prepare biplotEZ object ───────────────────────────────────────────────
  if (is.null(x$samples)) {
    x <- biplotEZ::samples(x)
  }
  x <- biplotEZ::axes(x)

  # ── Capture axis coordinates before un-centering X ──────────────────────

  # axes_coordinates() depends on the current state of X, so we must
  # capture them while X is still centered/scaled.
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

  # ── Un-center/un-scale X so hovertext shows raw values ─────────────────
  if (x$scaled) {
    x$X <- scale(x$X, center = FALSE, scale = 1 / x$sd)
  }
  if (x$center) {
    x$X <- scale(x$X, -x$means, scale = FALSE)
  }

  # ── Extract display aesthetics ────────────────────────────────────────────
  color <- x$samples$col
  symbol <- pch_to_plotly(x$samples$pch)
  group <- x$group.aes
  if (length(levels(x$group.aes)) == 1) {
    group <- factor(rep("Data", x$n))
  }

  # ── Build single payload ────────────────────────────────────────────────
  pname <- payload_name(pcs)

  payloads <- list()
  payloads[[pname]] <- build_one_payload(
    ez_obj = x,
    group = group,
    color = color,
    symbol = symbol,
    x_ref = x,
    include_polygons = TRUE,
    dim_prefix = "Dim",
    ax_pred = FALSE,
    vec_dis = FALSE,
    z.axes = z.axes,
    fit_qual = fit_qual
  )

  # ── No fit measures for regression biplots ──────────────────────────────
  fit_measures <- NULL

  # ── Build pc_info ───────────────────────────────────────────────────────
  pc_info <- list()
  pc_info[[pname]] <- list(
    pcs = pcs,
    label = pair_label(pcs, prefix = "Dim"),
    ft_name = ft_name(pcs)
  )

  # ── Store metadata for plot() ───────────────────────────────────────────
  meta <- list(
    x = x,
    color = color,
    symbol = symbol,
    group = group,
    fit.quality = fit_qual,
    fit.quality.plotly = fit_qual_plotly,
    pc_info = pc_info,
    dim_prefix = "Dim"
  )

  new_bipl5_biplot(payloads, fit_measures, meta, biplot_type = "reg")
}


# ─────────────────────────────────────────────────────────────────────────────
# wrap_bipl5.PCO
# ─────────────────────────────────────────────────────────────────────────────

#' Construct a bipl5_biplot from a PCO biplot
#'
#' Handles two cases depending on the axis type stored in \code{x$PCOaxes}:
#' \describe{
#'   \item{Linear axes}{Built identically to regression biplots via
#'     \code{build_one_payload()}, including translated density axes.}
#'   \item{Spline axes}{Uses a custom payload builder
#'     (\code{build_spline_payload()}) that places only sample points, the
#'     spline axis curves with tick marks, and a bounding circle.
#'     A custom JavaScript handler is attached at plot time.}
#' }
#' In both cases there is a single payload (\code{Payload_12}), no fit
#' measures, and \code{append_payload()} / \code{remove_payload()} are
#' disabled.
#'
#' @param x An object of class \code{biplot} from the biplotEZ package with
#'   \code{PCO()} method applied.
#'
#' @return An object of class \code{c("bipl5_biplot", "pco")}
#' @export
#' @method wrap_bipl5 PCO
#'
#' @examples
#' \dontrun{
#' library(biplotEZ)
#' bp <- biplot(iris[, 1:4]) |>
#'   PCO(dist.func = stats::dist) |>
#'   wrap_bipl5()
#' bp
#' plot(bp)
#' }
wrap_bipl5.PCO <- function(x) {
  # ── Prepare biplotEZ object ───────────────────────────────────────────────
  if (is.null(x$samples)) {
    x <- biplotEZ::samples(x)
  }
  if (is.null(x$axes)) {
    x <- biplotEZ::axes(x)
  }

  # ── Capture axis coordinates before un-centering X ──────────────────────
  # due to the nature of axes_coordinates(), we must temporarily
  # replace x$raw.X with the centered/scaled x$X to get correct axis coordinates for spline axes.
  # This does not affect the final payload because we restore x$raw.X immediately after.
  temp <- x$raw.X
  x$raw.X <- x$X
  z.axes <- biplotEZ::axes_coordinates(x)
  x$raw.X <- temp

  # ── Un-center/un-scale X so hovertext shows raw values ─────────────────
  if (x$scaled) {
    x$X <- scale(x$X, center = FALSE, scale = 1 / x$sd)
  }
  if (x$center) {
    x$X <- scale(x$X, -x$means, scale = FALSE)
  }

  # ── Extract display aesthetics ────────────────────────────────────────────
  color <- x$samples$col
  symbol <- pch_to_plotly(x$samples$pch)
  group <- x$group.aes
  if (length(levels(x$group.aes)) == 1) {
    group <- factor(rep("Data", x$n))
  }

  # ── Build single payload ────────────────────────────────────────────────
  pcs <- c(1, 2)
  pname <- payload_name(pcs)

  is_spline <- identical(x$PCOaxes, "splines")

  payloads <- list()

  if (is_spline) {
    payloads[[pname]] <- build_spline_payload(
      ez_obj = x,
      group = group,
      color = color,
      symbol = symbol,
      z.axes = z.axes
    )
  } else {
    payloads[[pname]] <- build_one_payload(
      ez_obj = x,
      group = group,
      color = color,
      symbol = symbol,
      x_ref = x,
      include_polygons = TRUE,
      dim_prefix = "Dim",
      ax_pred = FALSE,
      vec_dis = FALSE,
      z.axes = z.axes
    )
  }

  # ── No fit measures for PCO biplots ──────────────────────────────────────
  fit_measures <- NULL

  # ── Build pc_info ───────────────────────────────────────────────────────
  pc_info <- list()
  pc_info[[pname]] <- list(
    pcs = pcs,
    label = pair_label(pcs, prefix = "Dim"),
    ft_name = ft_name(pcs)
  )

  # ── Store metadata for plot() ───────────────────────────────────────────
  meta <- list(
    x = x,
    color = color,
    symbol = symbol,
    group = group,
    fit.quality = "",
    pc_info = pc_info,
    dim_prefix = "Dim",
    spline = is_spline
  )

  new_bipl5_biplot(payloads, fit_measures, meta, biplot_type = "pco")
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
  is_reg <- "reg" %in% class(bp)
  is_pco <- "pco" %in% class(bp)
  is_spline <- isTRUE(bp$meta$spline)

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
  dpquality <- first_payl$fit_qual
  if (is_reg && !is.null(bp$meta$fit.quality.plotly)) {
    dpquality <- bp$meta$fit.quality.plotly
  }

  p_ly <- plot_scaffolding(
    dpquality = dpquality,
    basis = ez$e.vects,
    PC_toggle = use_pc_toggle,
    ax_pred = has_fm,
    TDA = !is_spline,
    vec_dis = !(is_cva || is_reg || is_pco),
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
      if (has_fm) {
        js_payl$fit_table <- bp$fit_measures[[ft_map[nm]]]
      }
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
  if (is_spline) {
    p_ly <- insert_spline_js(p_ly, ez$p)
  } else {
    p_ly <- insert_linear_js_v1(
      p_ly,
      p = ez$p,
      cols = ez$axes$tick.label.col,
      payload = payload_for_js,
      fm_payload = fm_payload,
      initial_pc_key = pc_map[first_name]
    )
  }

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

#' Print the nested \code{Data} branch used by the public print methods
#'
#' @param data A \code{bipl5_data} object.
#' @param prefix Prefix string used to align tree branches with the caller's
#'   current indentation level.
#'
#' @return Invisibly called for its side effect of writing formatted text.
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

#' Print the fit-measures branch used by the public print methods
#'
#' @param fm A \code{bipl5_fitmeasures} object.
#' @param prefix Prefix string used to align tree branches with the caller's
#'   current indentation level.
#'
#' @return Invisibly called for its side effect of writing formatted text.
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

#' Format a compact dimension label for tree-style printing
#'
#' @param mat An object that may have matrix-like dimensions or vector length.
#'
#' @return A short character label such as \code{"  [150 x 2]"},
#'   \code{"  [4]"}, or \code{""} when no compact label can be inferred.
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
#' Internal engine used by \code{extract()}, \code{remove_payload()}, and
#' \code{append_payload()} when a new top-level \code{bipl5_biplot} needs to be
#' assembled from an existing one. The order of \code{keep} is preserved and
#' becomes the new payload order in \code{meta$pc_info}, which in turn controls
#' the initial plot shown by \code{plot.bipl5_biplot()}.
#'
#' @param bp A \code{bipl5_biplot} object
#' @param keep Character vector of payload names to retain
#'   (e.g. \code{"Payload_12"} or \code{c("Payload_12", "Payload_23")})
#'
#' @return A new \code{bipl5_biplot} with only the specified payloads and
#'   their corresponding fit tables. Shared PCA fit charts
#'   (\code{CumPred}, \code{CumAd}, \code{VarExp}, \code{Scree}) are preserved.
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
extract <- function(object, expr, from, what) {
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

#' Decompose an \code{extract()} path expression into its field names
#'
#' \code{extract()} accepts expressions such as
#' \code{Payload_12$Data$sample_coordinates}. This helper walks the nested
#' \code{$} calls and returns a character vector that can be traversed
#' programmatically.
#'
#' @param expr A symbol or nested \code{$} call captured with
#'   \code{substitute()}.
#'
#' @return Character vector of field names in traversal order.
#' @details Only plain symbols and nested \code{$} expressions are supported.
#'   Any other language object is treated as invalid input and raises an error.
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
remove_payload <- function(object, payload) {
  UseMethod("remove_payload")
}

#' @rdname remove_payload
#' @export
#' @method remove_payload bipl5_biplot
remove_payload.bipl5_biplot <- function(object, payload) {
  if (any(c("reg", "pco") %in% class(object))) {
    stop(
      "remove_payload() is not supported for this biplot type.",
      call. = FALSE
    )
  }
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
append_payload <- function(object, eigenvectors) {
  UseMethod("append_payload")
}

#' @rdname append_payload
#' @export
#' @method append_payload bipl5_biplot
append_payload.bipl5_biplot <- function(object, eigenvectors) {
  if (any(c("reg", "pco") %in% class(object))) {
    stop(
      "append_payload() is not supported for this biplot type.",
      call. = FALSE
    )
  }
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
