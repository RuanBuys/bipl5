# ─────────────────────────────────────────────────────────────────────────────
# Naming helpers (mdsDisplay, label, fit-table names from PC indices)
# ─────────────────────────────────────────────────────────────────────────────

#' Build the canonical mdsDisplay name for a dimension pair
#'
#' mdsDisplays are stored under stable names such as \code{mdsDisplay_12} and
#' \code{mdsDisplay_45}. This helper centralises that convention so the same
#' identifier is reused when mdsDisplays are constructed, subsetted, appended,
#' removed, and registered in \code{meta$pc_info}.
#'
#' @param pcs Integer vector of length 2 giving the selected dimension pair.
#'   Callers are expected to supply the pair in sorted order.
#'
#' @return A character scalar of the form \code{"mdsDisplay_ij"}.
#' @noRd
mdsDisplay_name <- function(pcs) paste0("mdsDisplay_", pcs[1], pcs[2])

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
#' Fit tables are stored separately from the main mdsDisplay traces and are keyed
#' by names such as \code{fit_table_12}. This helper keeps that mapping
#' consistent with the mdsDisplay naming convention.
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
#' A \code{bipl5_data} object stores the numeric data behind one mdsDisplay. It is
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

#' Create a bipl5_mdsDisplay object
#'
#' Bundles a constructed mdsDisplay together with its associated
#' \code{bipl5_data}. The \code{mdsDisplay_list} supplied here is expected to be
#' the output of the full build pipeline: a list containing the plotly-ready
#' mdsDisplay, fit-quality text, translated-axis metadata, and any slider state.
#' This constructor simply attaches the \code{Data} node and assigns the class.
#'
#' @param mdsDisplay_list The raw list produced by the mdsDisplay build pipeline,
#'   typically containing \code{mdsDisplay}, \code{fit_qual}, \code{m}, and
#'   \code{shift}.
#' @param data A \code{bipl5_data} object
#'
#' @return An object of class \code{bipl5_mdsDisplay}
#' @noRd
new_bipl5_mdsDisplay <- function(mdsDisplay_list, data) {
  mdsDisplay_list$Data <- data
  class(mdsDisplay_list) <- "bipl5_mdsDisplay"
  mdsDisplay_list
}

#' Create a bipl5_fitmeasures object
#'
#' Collects the plotly traces used by the optional right-hand-side fit panel in
#' PCA biplots. The chart-based measures are shared across mdsDisplays, while the
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

#' Create a plottable single fit-measure object
#'
#' Wraps one graph-based fit measure extracted from \code{fit_measures} so that
#' it can be dispatched to \code{plot()} independently of the parent
#' \code{bipl5_biplot}.
#'
#' @param trace_data List of plotly trace lists for a single fit graph.
#' @param fit_name Optional storage key used by \code{extract()}, e.g.
#'   \code{"CumPred"}.
#'
#' @return An object of class \code{bipl5_fit}
#' @noRd
new_bipl5_fit <- function(trace_data, fit_name = NULL) {
  obj <- list(
    trace_data = trace_data,
    fit_name = fit_name
  )
  class(obj) <- "bipl5_fit"
  obj
}

#' Create a bipl5_biplot object
#'
#' This is the top-level container returned by \code{wrap_bipl5()}. mdsDisplays
#' are stored as top-level fields, with \code{fit_measures} and \code{meta}
#' added alongside them. The order of entries in \code{meta$pc_info} is
#' significant: downstream code treats the first registered mdsDisplay as the
#' initial view shown to the user.
#'
#' @param mdsDisplays Named list of \code{bipl5_mdsDisplay} objects
#'   (e.g. \code{list(mdsDisplay_12 = ..., mdsDisplay_13 = ...)})
#' @param fit_measures A \code{bipl5_fitmeasures} object (or \code{NULL} for
#'   CVA biplots).
#' @param meta List of metadata required downstream by \code{plot()},
#'   \code{print()}, \code{extract()}, \code{remove_mdsDisplay()}, and
#'   \code{append_mdsDisplay()}. At minimum this contains the source biplotEZ
#'   object, display aesthetics, \code{fit.quality}, \code{pc_info}, and the
#'   basis prefix.
#' @param biplot_type Character string for the secondary class, e.g.
#'   \code{"pca"} or \code{"cva"}.
#'
#' @return An object of class \code{c("bipl5_biplot", biplot_type)}
#' @noRd
new_bipl5_biplot <- function(
  mdsDisplays,
  fit_measures,
  meta,
  biplot_type = "pca"
) {
  obj <- mdsDisplays
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
#' @param x A \code{biplotEZ} biplot object
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
#' Builds the mdsDisplay(s) used for a principal component analysis (PCA) biplot
#' and documents the associated PCA-biplot fit, predictivity and direct-reading
#' measures. In contrast to a regression biplot, the low-dimensional sample map
#' is obtained internally from the singular value decomposition of the processed
#' data matrix. If the wrapped object stores more than one principal-component
#' pair as separate mdsDisplays, the same formulas below apply to each mdsDisplay
#' separately, with the active mdsDisplay determined by the displayed pair of
#' principal components.
#'
#' @param x An object of class \code{biplot} from the \pkg{biplotEZ} package with
#'   \code{PCA()} method applied.
#'
#' @details
#' For the PCA biplot handled by this method, let
#' \eqn{\mathbf{X}\in\mathbb{R}^{n\times p}} denote the \emph{processed} data
#' matrix stored in the input \code{biplot} object after centring and any
#' optional scaling performed by \code{biplot()}. Thus \eqn{\mathbf{X}} is the
#' matrix on which PCA is actually carried out. Write the singular value
#' decomposition as
#' \deqn{\mathbf{X} = \mathbf{U}\mathbf{D}\mathbf{V}^{\top},}
#' where \eqn{\mathbf{U}^{\top}\mathbf{U}=\mathbf{I}},
#' \eqn{\mathbf{V}^{\top}\mathbf{V}=\mathbf{I}}, and
#' \eqn{\mathbf{D}=\mathrm{diag}(d_1,\ldots,d_q)} with
#' \eqn{q=\mathrm{rank}(\mathbf{X})} and
#' \eqn{d_1\ge \cdots \ge d_q \ge 0}. The columns of \eqn{\mathbf{V}} are the
#' principal directions, and the corresponding principal component score vectors
#' are \eqn{\mathbf{z}_t = d_t\mathbf{u}_t = \mathbf{X}\mathbf{v}_t},
#' \eqn{t=1,\ldots,q}. This is the standard PCA biplot construction underlying
#' Gabriel's original formulation and subsequent calibrated-axis developments
#' (Gabriel, 1971; Gower and Hand, 1996; Gower, Lubbe and le Roux, 2011;
#' Greenacre, 2010).
#'
#' Suppose the user has selected two principal components
#' \eqn{a<b}. Let \eqn{\mathbf{J}_{ab}} denote the diagonal
#' \eqn{q\times q} selector matrix with ones in positions \eqn{a} and \eqn{b}
#' and zeros elsewhere. Then the two-dimensional PCA fitted matrix is
#' \deqn{\widehat{\mathbf{X}}_{ab} =
#'   \mathbf{U}\mathbf{D}\mathbf{J}_{ab}\mathbf{V}^{\top}
#'   = \mathbf{X}\mathbf{V}\mathbf{J}_{ab}\mathbf{V}^{\top}.}
#' Equivalently, if \eqn{\mathbf{U}_{ab}} and \eqn{\mathbf{V}_{ab}} denote the
#' submatrices containing columns \eqn{a} and \eqn{b}, and
#' \eqn{\mathbf{D}_{ab}=\mathrm{diag}(d_a,d_b)}, then
#' \deqn{\widehat{\mathbf{X}}_{ab} =
#'   \mathbf{U}_{ab}\mathbf{D}_{ab}\mathbf{V}_{ab}^{\top}
#'   = d_a\mathbf{u}_a\mathbf{v}_a^{\top}
#'   + d_b\mathbf{u}_b\mathbf{v}_b^{\top}.}
#' When \eqn{(a,b)=(1,2)}, this is the best rank-2 approximation to
#' \eqn{\mathbf{X}} in Frobenius norm by the Eckart--Young theorem. For any
#' other selected pair, the same formula gives the orthogonal projection of
#' \eqn{\mathbf{X}} onto the chosen two-dimensional principal-component subspace,
#' but it is not generally the globally optimal rank-2 approximation
#' (Eckart and Young, 1936; Gabriel, 1971; Greenacre, 2010).
#'
#' The calibrated-axis PCA biplot may be written in the general form
#' \deqn{\widehat{\mathbf{X}}_{ab} = \mathbf{Z}_{ab}\mathbf{H}_{ab}^{\top},}
#' where the exact factorization depends on the type of PCA biplot being
#' displayed.
#'
#' For the ordinary PCA biplot, which prioritizes the Euclidean geometry of the
#' sample points, take
#' \deqn{\mathbf{Z}_{ab} = \mathbf{U}_{ab}\mathbf{D}_{ab},
#'   \qquad
#'   \mathbf{H}_{ab} = \mathbf{V}_{ab}.}
#' Thus the displayed sample coordinates are the selected principal component
#' scores, and the fitted matrix is
#' \deqn{\widehat{\mathbf{X}}_{ab} =
#'   \mathbf{Z}_{ab}\mathbf{H}_{ab}^{\top}.}
#' If \eqn{\mathbf{h}_{(j)}} denotes the \eqn{j}th row of
#' \eqn{\mathbf{H}_{ab}} written as a column vector in \eqn{\mathbb{R}^2},
#' then for sample \eqn{i}
#' \deqn{\widehat{x}_{ij} =
#'   \mathbf{z}_{i}^{\top}\mathbf{h}_{(j)}.}
#' Hence the calibrated axis for variable \eqn{j} has direction
#' \eqn{\mathbf{h}_{(j)}}, and the point on that axis corresponding to marker
#' value \eqn{\mu} is
#' \deqn{\mathbf{p}_{\mu j} =
#'   \frac{\mu}{\mathbf{h}_{(j)}^{\top}\mathbf{h}_{(j)}}\mathbf{h}_{(j)}.}
#' This is the standard calibrated-axis formula used to place tick marks and to
#' recover predicted values from projections onto the displayed axis
#' (Gower and Hand, 1996; Gower, Lubbe and le Roux, 2011).
#'
#' A second important special case is the \emph{correlation biplot}, obtained
#' when the processed matrix \eqn{\mathbf{X}} is standardized and the display is
#' chosen so that correlations between variables are approximated by the cosines
#' of the angles between the displayed variable directions. In that case one may
#' equivalently factorize
#' \deqn{\widehat{\mathbf{X}}_{ab} =
#'   \mathbf{U}_{ab}(\mathbf{V}_{ab}\mathbf{D}_{ab})^{\top}.}
#' Hence the displayed sample coordinates are \eqn{\mathbf{U}_{ab}} and the
#' variable directions are the rows of \eqn{\mathbf{V}_{ab}\mathbf{D}_{ab}}.
#' If \eqn{\mathbf{c}_{(j)}} denotes the \eqn{j}th such row written as a column
#' vector, then
#' \deqn{\widehat{x}_{ij} =
#'   \mathbf{u}_{i,ab}^{\top}\mathbf{c}_{(j)}.}
#' In this standardized setting the coordinates
#' \eqn{\mathbf{c}_{(j)}} are proportional to the correlations of variable
#' \eqn{j} with the selected principal components, and the geometry of the
#' displayed variable directions is therefore tuned to the correlation structure
#' rather than to the raw score geometry of the samples. This is the sense in
#' which \code{correlation.biplot = TRUE} preserves variable-correlation
#' information in the display (Gabriel, 1971; Greenacre, 2010; \pkg{biplotEZ}
#' manual and vignette).
#'
#' In either factorization, all predicted values in
#' \eqn{\widehat{\mathbf{X}}_{ab}} are on the same centred/scaled scale as the
#' stored matrix \eqn{\mathbf{X}}. If required, predictions can be
#' back-transformed to the original variable scale using the means and standard
#' deviations stored in the input \code{biplot} object.
#'
#' A fundamental feature of the PCA biplot is that both \emph{sample-side} and
#' \emph{variable-side} orthogonal decompositions hold. Writing
#' \eqn{\mathbf{E}_{ab}=\mathbf{X}-\widehat{\mathbf{X}}_{ab}}, one has
#' \deqn{\mathbf{X}\mathbf{X}^{\top} =
#'   \widehat{\mathbf{X}}_{ab}\widehat{\mathbf{X}}_{ab}^{\top}
#'   + \mathbf{E}_{ab}\mathbf{E}_{ab}^{\top},}
#' and
#' \deqn{\mathbf{X}^{\top}\mathbf{X} =
#'   \widehat{\mathbf{X}}_{ab}^{\top}\widehat{\mathbf{X}}_{ab}
#'   + \mathbf{E}_{ab}^{\top}\mathbf{E}_{ab}.}
#' The first is the \emph{Type A orthogonality}, which justifies sample-side
#' measures of fit. The second is the \emph{Type B orthogonality}, which
#' justifies variable-side measures of fit. For PCA both orthogonality relations
#' hold simultaneously because \eqn{\widehat{\mathbf{X}}_{ab}} is obtained from
#' an orthogonal principal-component projection
#' (Gabriel, 1971; Gower, Lubbe and le Roux, 2011; Gardner-Lubbe, le Roux and
#' Gower, 2008).
#'
#' Let \eqn{\mathbf{x}_{i\cdot}^{\top}} denote row \eqn{i} of \eqn{\mathbf{X}}
#' and \eqn{\widehat{\mathbf{x}}_{i\cdot}^{\top}} the corresponding row of
#' \eqn{\widehat{\mathbf{X}}_{ab}}. The \emph{sample predictivity} of sample
#' \eqn{i} is then
#' \deqn{\psi_i =
#'   \frac{\|\widehat{\mathbf{x}}_{i\cdot}\|^2}
#'        {\|\mathbf{x}_{i\cdot}\|^2}
#'   =
#'   1 -
#'   \frac{\|\mathbf{x}_{i\cdot} -
#'          \widehat{\mathbf{x}}_{i\cdot}\|^2}
#'        {\|\mathbf{x}_{i\cdot}\|^2},
#'   \qquad i=1,\ldots,n.}
#' Thus \eqn{\psi_i} is the proportion of the sum of squares of sample \eqn{i}
#' reproduced by the chosen two-dimensional PCA display. Because of Type A
#' orthogonality, \eqn{0\le \psi_i \le 1}. Samples with \eqn{\psi_i} near one
#' lie close to the displayed PCA plane, whereas samples with \eqn{\psi_i} near
#' zero lie largely orthogonal to it. This is the sample-side fit measure used in
#' the PCA biplot literature and in \pkg{biplotEZ}
#' (Gardner-Lubbe, le Roux and Gower, 2008; \pkg{biplotEZ} vignette).
#'
#' Let \eqn{\mathbf{x}_{(j)}} denote column \eqn{j} of \eqn{\mathbf{X}} and
#' \eqn{\widehat{\mathbf{x}}_{(j)}} the corresponding column of
#' \eqn{\widehat{\mathbf{X}}_{ab}}. The \emph{axis predictivity} of variable
#' \eqn{j} is
#' \deqn{\phi_j =
#'   \frac{\|\widehat{\mathbf{x}}_{(j)}\|^2}
#'        {\|\mathbf{x}_{(j)}\|^2}
#'   =
#'   1 -
#'   \frac{\|\mathbf{x}_{(j)} -
#'          \widehat{\mathbf{x}}_{(j)}\|^2}
#'        {\|\mathbf{x}_{(j)}\|^2},
#'   \qquad j=1,\ldots,p.}
#' Thus \eqn{\phi_j} is the proportion of the sum of squares of variable
#' \eqn{j} reproduced by the chosen PCA plane. Because of Type B orthogonality,
#' \eqn{0\le \phi_j \le 1}. In a calibrated-axis display, \eqn{\phi_j} is the
#' natural sum-of-squares measure of how well the axis for variable \eqn{j}
#' reproduces the underlying processed values. This is the quantity reported in
#' \pkg{biplotEZ} as \dQuote{axis predictivity}
#' (Gardner-Lubbe, le Roux and Gower, 2008; \pkg{biplotEZ} vignette;
#' Greenacre, 2010).
#'
#' The overall quality of the displayed PCA subspace is
#' \deqn{R^2_{\mathrm{disp},ab} =
#'   \frac{\|\widehat{\mathbf{X}}_{ab}\|_F^2}{\|\mathbf{X}\|_F^2}
#'   =
#'   1 - \frac{\|\mathbf{X} - \widehat{\mathbf{X}}_{ab}\|_F^2}
#'            {\|\mathbf{X}\|_F^2}.}
#' Since
#' \eqn{\|\widehat{\mathbf{X}}_{ab}\|_F^2 = d_a^2+d_b^2}, this may also be
#' written as
#' \deqn{R^2_{\mathrm{disp},ab} =
#'   \frac{d_a^2+d_b^2}{\sum_{t=1}^{q} d_t^2}.}
#' In particular, when \eqn{(a,b)=(1,2)}, this is the familiar proportion of
#' total sum of squares explained by the first two principal components. More
#' generally, it is the quality of the specific displayed pair chosen by the
#' user, matching the \pkg{biplotEZ} quality measure
#' (Gabriel, 1971; Greenacre, 2010; \pkg{biplotEZ} vignette).
#'
#' Because both Type A and Type B orthogonality hold, the overall quality can be
#' expressed as a weighted average on either the sample side or the variable
#' side. On the variable side,
#' \deqn{R^2_{\mathrm{disp},ab} =
#'   \sum_{j=1}^{p} w_j\phi_j,}
#' where
#' \deqn{w_j =
#'   \frac{\|\mathbf{x}_{(j)}\|^2}{\|\mathbf{X}\|_F^2},
#'   \qquad
#'   \sum_{j=1}^{p} w_j = 1.}
#' Hence variables with larger sums of squares contribute more to the overall
#' display quality. If the original call to \code{biplot()} used
#' \code{scale = TRUE}, so that all processed variables have equal sums of
#' squares, then
#' \deqn{R^2_{\mathrm{disp},ab} = \frac{1}{p}\sum_{j=1}^{p}\phi_j.}
#' Thus, for a standardized PCA biplot, the overall quality is the simple average
#' of the individual axis predictivities. This is the weighted-average
#' interpretation requested by the present wrapper.
#'
#' Similarly, on the sample side,
#' \deqn{R^2_{\mathrm{disp},ab} =
#'   \sum_{i=1}^{n} m_i\psi_i,}
#' where
#' \deqn{m_i =
#'   \frac{\|\mathbf{x}_{i\cdot}\|^2}{\|\mathbf{X}\|_F^2},
#'   \qquad
#'   \sum_{i=1}^{n} m_i = 1.}
#' Hence the same overall display quality may be read as a weighted average of
#' sample predictivities or as a weighted average of axis predictivities
#' (Gardner-Lubbe, le Roux and Gower, 2008).
#'
#' Unlike the regression-biplot case, no ordered orthogonalization is required to
#' decompose the quality of a PCA display into separate contributions from the
#' two displayed dimensions, because principal components are already mutually
#' orthogonal. Indeed,
#' \deqn{\widehat{\mathbf{X}}_{ab} =
#'   d_a\mathbf{u}_a\mathbf{v}_a^{\top}
#'   + d_b\mathbf{u}_b\mathbf{v}_b^{\top},}
#' and these two rank-1 parts are orthogonal in Frobenius inner product. Hence
#' \deqn{R^2_{\mathrm{disp},ab} = R^2_a + R^2_b,}
#' where
#' \deqn{R^2_a = \frac{d_a^2}{\|\mathbf{X}\|_F^2},
#'   \qquad
#'   R^2_b = \frac{d_b^2}{\|\mathbf{X}\|_F^2}.}
#' Thus the contribution of each displayed principal component is obtained
#' directly from its singular value.
#'
#' The same orthogonal decomposition yields a per-component breakdown of each
#' axis predictivity. Since
#' \deqn{\widehat{\mathbf{x}}_{(j)} =
#'   d_a v_{ja}\mathbf{u}_a + d_b v_{jb}\mathbf{u}_b,}
#' with orthogonal components \eqn{\mathbf{u}_a} and \eqn{\mathbf{u}_b}, one has
#' \deqn{\phi_j = \phi_{ja} + \phi_{jb},}
#' where
#' \deqn{\phi_{ja} =
#'   \frac{d_a^2 v_{ja}^2}{\|\mathbf{x}_{(j)}\|^2},
#'   \qquad
#'   \phi_{jb} =
#'   \frac{d_b^2 v_{jb}^2}{\|\mathbf{x}_{(j)}\|^2}.}
#' Hence the predictivity of axis \eqn{j} can be decomposed exactly into the
#' separate contributions of the two displayed principal components. This is the
#' PCA analogue of the dimension-wise decomposition used elsewhere in the biplot
#' literature, but here it is especially simple because the components are
#' orthogonal from the outset. In particular, if the same variable is well
#' aligned with one selected principal direction but not the other, this will be
#' visible in the separate values \eqn{\phi_{ja}} and \eqn{\phi_{jb}}.
#'
#' Likewise, each sample predictivity decomposes as
#' \deqn{\psi_i = \psi_{ia} + \psi_{ib},}
#' where
#' \deqn{\psi_{ia} =
#'   \frac{d_a^2 u_{ia}^2}{\|\mathbf{x}_{i\cdot}\|^2},
#'   \qquad
#'   \psi_{ib} =
#'   \frac{d_b^2 u_{ib}^2}{\|\mathbf{x}_{i\cdot}\|^2}.}
#' Thus the contribution of each displayed principal component may be read not
#' only globally through \eqn{R^2_a} and \eqn{R^2_b}, but also locally through
#' the sample-wise contributions \eqn{\psi_{ia}} and \eqn{\psi_{ib}} and the
#' axis-wise contributions \eqn{\phi_{ja}} and \eqn{\phi_{jb}}
#' (Gardner-Lubbe, le Roux and Gower, 2008).
#'
#' In addition to the sum-of-squares fit measures above, this method may also
#' report direct-reading diagnostics in the sense of Alves (2012). These
#' quantities serve a different purpose from the predictivities
#' \eqn{\phi_j} and \eqn{\psi_i}. The predictivities measure how much of the
#' variation in \eqn{\mathbf{X}} is reproduced by the selected PCA plane in a
#' sum-of-squares sense. By contrast, the Alves diagnostics measure how
#' accurately values can be read directly from the displayed calibrated axes in
#' the current two-dimensional map. This distinction is central in the predictive
#' biplot literature (Alves, 2012).
#'
#' Let \eqn{\mathbf{g}_{(j)}\in\mathbb{R}^2} denote the displayed direction of
#' variable axis \eqn{j} under the active PCA factorization. Thus
#' \eqn{\mathbf{g}_{(j)}=\mathbf{h}_{(j)}} for the ordinary PCA biplot and
#' \eqn{\mathbf{g}_{(j)}=\mathbf{c}_{(j)}} for the correlation biplot. Let
#' \eqn{\mathbf{z}_i\in\mathbb{R}^2} denote the corresponding displayed sample
#' coordinate of sample \eqn{i}. Then the value read from the graph on axis
#' \eqn{j} for sample \eqn{i} is
#' \deqn{\widehat{x}_{ij} = \mathbf{z}_{i}^{\top}\mathbf{g}_{(j)},}
#' and the point on the calibrated axis corresponding to that reading is
#' \deqn{\mathbf{p}_{ij} =
#'   \frac{\widehat{x}_{ij}}
#'        {\mathbf{g}_{(j)}^{\top}\mathbf{g}_{(j)}}\mathbf{g}_{(j)}.}
#' Thus the direct reading from the displayed PCA axis coincides exactly with the
#' fitted value from the active two-dimensional PCA approximation.
#'
#' Let \eqn{s_j} denote the standard deviation used to standardize variable
#' \eqn{j}. When the processed matrix \eqn{\mathbf{X}} is already standardized,
#' \eqn{s_j=1}. The pointwise direct-reading error for sample \eqn{i} on axis
#' \eqn{j} is then
#' \deqn{\delta_{ij} =
#'   \frac{|x_{ij}-\widehat{x}_{ij}|}{s_j}.}
#' If \eqn{\mathbf{X}} is already standardized, then
#' \eqn{\delta_{ij}=|x_{ij}-\widehat{x}_{ij}|}. The corresponding axis-level mean
#' direct-reading error is
#' \deqn{\bar{\delta}_j =
#'   \frac{1}{n}\sum_{i=1}^{n}\delta_{ij}
#'   =
#'   \frac{1}{n}\sum_{i=1}^{n}
#'   \frac{|x_{ij}-\widehat{x}_{ij}|}{s_j}.}
#' This is the two-dimensional PCA-biplot analogue of the mean standard
#' predictive error of Alves (2012).
#'
#' Let \eqn{\tau_{\mathrm{axis}} > 0} be a user-specified
#' \emph{tolerance parameter} for axis-level direct-reading error. Then the Alves
#' selection rule becomes
#' \deqn{\text{retain axis }j
#'   \quad\Longleftrightarrow\quad
#'   \bar{\delta}_j \le \tau_{\mathrm{axis}}.}
#' Likewise, for an observation-level tolerance parameter
#' \eqn{\tau_{\mathrm{units}} > 0}, sample \eqn{i} is flagged with respect to
#' axis \eqn{j} whenever
#' \deqn{\delta_{ij} > \tau_{\mathrm{units}}.}
#' Hence \eqn{\bar{\delta}_j} may be used for axis selection and
#' \eqn{\delta_{ij}} for observation-level checking, exactly as in the predictive
#' PCA-biplot framework of Alves (2012).
#'
#' The Alves diagnostics and the PCA predictivities are complementary. The
#' quantity \eqn{\phi_j} answers the question:
#' \dQuote{How much of variable \eqn{j}'s sum of squares is reproduced by the
#' selected PCA plane?} The quantity \eqn{\psi_i} answers the corresponding
#' sample-side question:
#' \dQuote{How much of sample \eqn{i}'s sum of squares is reproduced by the
#' selected PCA plane?} By contrast, \eqn{\bar{\delta}_j} answers the distinct
#' question:
#' \dQuote{How accurately can values of variable \eqn{j} be read directly from
#' the displayed calibrated axis?} Consequently, an axis may have high
#' \eqn{\phi_j} yet still have non-negligible direct-reading error in the current
#' display, while an axis with only moderate \eqn{\phi_j} may nevertheless admit
#' acceptable direct readings. In this implementation, the
#' \eqn{\phi_j}- and \eqn{\psi_i}-families are the primary sum-of-squares fit
#' measures, whereas the Alves quantities \eqn{\bar{\delta}_j} and
#' \eqn{\delta_{ij}} provide supplementary, display-specific diagnostics.
#'
#' In the wrapped \code{bipl5_biplot} object, these formulas drive the
#' hover-time fitted values \eqn{\widehat{\mathbf{X}}_{ab}}, the calibrated tick
#' markers for each active PCA mdsDisplay, the bottom display-quality label, and
#' the axis/sample fit summaries attached to the active two-dimensional
#' principal-component view. If several PC pairs are stored as separate mdsDisplays,
#' the same construction applies to each mdsDisplay separately.
#'
#' @references
#' Eckart, C. and Young, G. (1936). The approximation of one matrix by another of
#' lower rank. \emph{Psychometrika}, 1, 211--218.
#'
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
#' Gardner-Lubbe, S., le Roux, N. J. and Gower, J. C. (2008).
#' Measures of fit in principal component and canonical variate analyses.
#' \emph{Journal of Applied Statistics}, 35(9), 947--965.
#' \doi{10.1080/02664760802185399}
#'
#' Lubbe, S., le Roux, N. J., Nienkemper-Swanepoel, J., Ganey, R.,
#' Buys, R., Adams, Z.-M. and Manefeldt, P. (2025).
#' \emph{biplotEZ: EZ-to-Use Biplots}. R package version 2.2.
#'
#' Alves, M. R. (2012). Evaluation of the predictive power of biplot axes to
#' automate the construction and layout of biplots based on the accuracy of
#' direct readings from common outputs of multivariate analyses:
#' 1. application to principal component analysis.
#' \emph{Journal of Chemometrics}, 26(5), 180--190.
#' \doi{10.1002/cem.2433}
#'
#' @return An object of class \code{c("bipl5_biplot", "PCA")}
#' @export
#' @method wrap_bipl5 PCA
#'
#' @examples
#' \dontrun{
#' library(biplotEZ)
#' bp <- biplot(iris[, 1:4], scale = TRUE) |>
#'   PCA(e.vects = c(1, 2), group.aes = iris[, 5]) |>
#'   wrap_bipl5()
#' bp
#' plot(bp)
#'
#' bp_cor <- biplot(iris[, 1:4], scale = TRUE) |>
#'   PCA(
#'     e.vects = c(1, 2),
#'     group.aes = iris[, 5],
#'     correlation.biplot = TRUE
#'   ) |>
#'   wrap_bipl5()
#' plot(bp_cor)
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

  # ── Build all three mdsDisplays ──────────────────────────────────────────────
  build_fit_table <- function(ez_obj) {
    tmp <- list(mdsDisplay = list())
    tmp <- add_table_mdsDisplay(tmp, x = ez_obj)
    tmp$mdsDisplay$fit_table
  }

  mdsDisplays <- list()
  fit_tables <- list()

  for (i in seq_along(all_pairs)) {
    pcs <- all_pairs[[i]]
    pname <- mdsDisplay_name(pcs)

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

    mdsDisplays[[pname]] <- build_one_mdsDisplay(
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

  # ── Build pc_info (single source of truth for downstream code) ──────────
  pc_info <- list()
  for (pcs in all_pairs) {
    pc_info[[mdsDisplay_name(pcs)]] <- list(
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

  new_bipl5_biplot(mdsDisplays, fit_measures, meta, biplot_type = "pca")
}


# ─────────────────────────────────────────────────────────────────────────────
# wrap_bipl5.CVA
# ─────────────────────────────────────────────────────────────────────────────

#' Construct a bipl5_biplot from a CVA biplot
#'
#' Builds mdsDisplays for the user's CV pair and available supplementary pairs,
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

  # ── Build all mdsDisplays ────────────────────────────────────────────────────
  mdsDisplays <- list()

  for (i in seq_along(all_pairs)) {
    pcs <- all_pairs[[i]]
    pname <- mdsDisplay_name(pcs)

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

    mdsDisplays[[pname]] <- build_one_mdsDisplay(
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
    pc_info[[mdsDisplay_name(pcs)]] <- list(
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

  new_bipl5_biplot(mdsDisplays, fit_measures, meta, biplot_type = "cva")
}


#' Construct a bipl5_biplot from a regression biplot
#'
#' Builds the single mdsDisplay used for a linear regression biplot and documents
#' the associated regression-biplot fit and predictivity measures.
#' Regression biplots do not use the multi-mdsDisplay fit machinery available for
#' PCA/CVA displays: they have one fixed mdsDisplay (\code{mdsDisplay_12}),
#' \code{append_mdsDisplay()} and \code{remove_mdsDisplay()} are not supported,
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
#' In addition to the sum-of-squares fit measures above, this method may also
#' report direct-reading error diagnostics in the sense of Alves (2012). The
#' purpose of these diagnostics is different from that of the predictivities
#' \eqn{\phi_j}. The quantities \eqn{\phi_j}, \eqn{\phi_{j1}},
#' \eqn{\phi_{j,2\mid 1}} and \eqn{R^2_{\mathrm{disp}}} measure how much of the
#' variation in \eqn{\mathbf{X}} is reproduced by the fitted regression biplot.
#' By contrast, the Alves diagnostics measure how accurately values can be read
#' directly from a displayed calibrated axis in the current two-dimensional map.
#' Alves (2012) proposed this idea for predictive PCA biplots; in the present
#' two-dimensional regression-biplot setting the same principle applies in a
#' particularly simple form because there is only one displayed map.
#'
#' For each sample \eqn{i=1,\ldots,n} and variable axis
#' \eqn{j=1,\ldots,p}, the reading taken from the displayed axis of variable
#' \eqn{j} is precisely the fitted value
#' \deqn{\widehat{x}_{ij} = \mathbf{z}_i^{\top}\mathbf{h}_{(j)}.}
#' The corresponding point on the calibrated axis is
#' \deqn{\mathbf{p}_{ij} =
#'   \frac{\widehat{x}_{ij}}
#'        {\mathbf{h}_{(j)}^{\top}\mathbf{h}_{(j)}}\mathbf{h}_{(j)},}
#' obtained by substituting \eqn{\mu = \widehat{x}_{ij}} into the calibration
#' formula. Thus the direct reading from the graph and the fitted value from the
#' regression model coincide.
#'
#' Let \eqn{s_j} denote the standard deviation used to standardize variable
#' \eqn{j}. When \code{scale = TRUE}, the processed matrix \eqn{\mathbf{X}}
#' already has unit-variance columns and hence \eqn{s_j = 1}. The
#' pointwise direct-reading error for sample \eqn{i} on variable axis
#' \eqn{j} is defined by
#' \deqn{\delta_{ij} =
#'   \frac{|x_{ij} - \widehat{x}_{ij}|}{s_j}.}
#' If the processed matrix is already standardized, then
#' \eqn{\delta_{ij} = |x_{ij} - \widehat{x}_{ij}|}; in that case
#' \eqn{\delta_{ij}} is the direct analogue of Alves's standard predictive error.
#' More generally, dividing by \eqn{s_j} expresses the discrepancy on a
#' comparable variable-wise scale. The quantity \eqn{\delta_{ij}} is therefore a
#' sample-by-axis direct-reading error.
#'
#' The corresponding axis-level mean direct-reading error is
#' \deqn{\bar{\delta}_j =
#'   \frac{1}{n}\sum_{i=1}^{n}\delta_{ij}
#'   =
#'   \frac{1}{n}\sum_{i=1}^{n}
#'   \frac{|x_{ij} - \widehat{x}_{ij}|}{s_j}.}
#' This is the two-dimensional regression-biplot analogue of the mean standard
#' predictive error of Alves (2012). Small values of \eqn{\bar{\delta}_j}
#' indicate that the calibrated axis for variable \eqn{j} supports accurate
#' direct readings on average across the displayed observations, whereas large
#' values indicate that direct readings from that axis are unreliable in the
#' current display.
#'
#' Let \eqn{\tau_{\mathrm{axis}} > 0} be a user-specified
#' \emph{tolerance parameter} for axis-level direct-reading error. Then the
#' Alves selection rule specialized to the present two-dimensional regression
#' biplot is
#' \deqn{\text{retain axis }j
#'   \quad\Longleftrightarrow\quad
#'   \bar{\delta}_j \le \tau_{\mathrm{axis}}.}
#' Thus an axis is shown only when its average direct-reading error is at most
#' the allowed tolerance. Larger values of \eqn{\tau_{\mathrm{axis}}} retain
#' more axes and therefore produce denser displays; smaller values enforce
#' stricter axis selection and lead to sparser, more conservative displays.
#' In Alves (2012), values around 0.5 are discussed as a practical starting
#' point in conventional settings, but no universal default should be assumed.
#'
#' In addition to axis selection, Alves (2012) proposed a second
#' \emph{tolerance parameter} for individual sample-axis discrepancies. Let
#' \eqn{\tau_{\mathrm{units}} > 0}. A sample \eqn{i} is then flagged as an
#' outlier with respect to axis \eqn{j} whenever
#' \deqn{\delta_{ij} > \tau_{\mathrm{units}}.}
#' Such a flag indicates that, even if axis \eqn{j} is acceptable on average,
#' the direct reading for sample \eqn{i} from that axis is poor in the current
#' display. Alves (2012) discusses values around 0.75 as a practical starting
#' point for this tolerance parameter, again subject to the application and the
#' scale of the analysis.
#'
#' Because this wrapper is tied to a single two-dimensional regression-biplot
#' display, the quantities \eqn{\delta_{ij}} and \eqn{\bar{\delta}_j} are
#' display-specific diagnostics. They are not measures of the quality of the
#' underlying fitted subspace in the sum-of-squares sense; rather, they quantify
#' the numerical accuracy of direct readings from the currently displayed axes.
#' This distinction is central in Alves (2012), who emphasizes that direct-reading
#' error is conceptually different from earlier axis-predictivity measures.
#'
#' The Alves diagnostics and the regression-biplot predictivities are therefore
#' complementary. The quantity \eqn{\phi_j} is a variance-accounted-for ratio
#' justified by Type B orthogonality and answers the question:
#' \dQuote{How much of variable \eqn{j}'s sum of squares is reproduced by the
#' displayed regression biplot?} The quantity \eqn{\bar{\delta}_j} is a mean
#' absolute direct-reading error and answers the different question:
#' \dQuote{How accurately can values of variable \eqn{j} be read from the
#' displayed calibrated axis?} Consequently, a variable may have high
#' \eqn{\phi_j} and still have a non-negligible direct-reading error, while a
#' variable with moderate \eqn{\phi_j} may nevertheless support acceptable
#' average direct readings. In this implementation, the \eqn{\phi_j}-family is
#' the primary set of sum-of-squares fit measures, whereas the Alves quantities
#' \eqn{\bar{\delta}_j} and \eqn{\delta_{ij}} provide supplementary,
#' display-specific diagnostics for axis selection and observation-level
#' checking.
#'
#' In contrast, a regression biplot does \emph{not} in general satisfy the
#' sample-side decomposition
#' \deqn{\mathbf{X}\mathbf{X}^{\top} =
#'   \widehat{\mathbf{X}}\widehat{\mathbf{X}}^{\top} +
#'   (\mathbf{X} - \widehat{\mathbf{X}})
#'   (\mathbf{X} - \widehat{\mathbf{X}})^{\top}.}
#' Consequently, PCA-style sample predictivities are not generally justified for
#' a regression biplot. The principled sum-of-squares fit measures are the
#' variable predictivities \eqn{\phi_j}, the overall quality
#' \eqn{R^2_{\mathrm{disp}}}, and the ordered dimension-specific contributions
#' described above, with the Alves direct-reading errors providing a distinct
#' supplementary perspective on the quality of the displayed axes.
#'
#' In the wrapped \code{bipl5_biplot} object, these formulas drive the bottom
#' display-quality label, the hover-time predicted values
#' \eqn{\widehat{\mathbf{X}}}, and the calibrated linear axes stored in
#' \code{mdsDisplay_12}. Since the regression display is tied to one externally
#' supplied map, \code{wrap_bipl5.regress()} produces a single mdsDisplay only.
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
#' Alves, M. R. (2012). Evaluation of the predictive power of biplot axes to
#' automate the construction and layout of biplots based on the accuracy of
#' direct readings from common outputs of multivariate analyses:
#' application to principal component analysis.
#' \emph{Journal of Chemometrics}, 26(5), 180--190.
#' \doi{10.1002/cem.2433}
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
  z.axes <- clean_linear_axes_coordinates(x)
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

  # ── Build single mdsDisplay ────────────────────────────────────────────────
  pname <- mdsDisplay_name(pcs)

  mdsDisplays <- list()
  mdsDisplays[[pname]] <- build_one_mdsDisplay(
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

  new_bipl5_biplot(mdsDisplays, fit_measures, meta, biplot_type = "reg")
}


# ─────────────────────────────────────────────────────────────────────────────
# wrap_bipl5.PCO
# ─────────────────────────────────────────────────────────────────────────────

#' Construct a bipl5_biplot from a PCO biplot
#'
#' Handles two cases depending on the axis type stored in \code{x$PCOaxes}:
#' \describe{
#'   \item{Linear axes}{Built identically to regression biplots via
#'     \code{build_one_mdsDisplay()}, including translated density axes.}
#'   \item{Spline axes}{Uses a custom mdsDisplay builder
#'     (\code{build_spline_mdsDisplay()}) that places only sample points, the
#'     spline axis curves with tick marks, and a bounding circle.
#'     A custom JavaScript handler is attached at plot time.}
#' }
#' In both cases there is a single mdsDisplay (\code{mdsDisplay_12}), no fit
#' measures, and \code{append_mdsDisplay()} / \code{remove_mdsDisplay()} are
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
  # This does not affect the final mdsDisplay because we restore x$raw.X immediately after.
  temp <- x$raw.X
  x$raw.X <- x$X
  z.axes <- zero_to_near_zero(biplotEZ::axes_coordinates(x))
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

  # ── Build single mdsDisplay ────────────────────────────────────────────────
  pcs <- c(1, 2)
  pname <- mdsDisplay_name(pcs)

  is_spline <- identical(x$PCOaxes, "splines")

  if (!is_spline) {
    z.axes <- clean_linear_axes_coordinates(x, z.axes)
  }

  mdsDisplays <- list()

  if (is_spline) {
    mdsDisplays[[pname]] <- build_spline_mdsDisplay(
      ez_obj = x,
      group = group,
      color = color,
      symbol = symbol,
      z.axes = z.axes
    )
  } else {
    mdsDisplays[[pname]] <- build_one_mdsDisplay(
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

  new_bipl5_biplot(mdsDisplays, fit_measures, meta, biplot_type = "pco")
}


# ─────────────────────────────────────────────────────────────────────────────
# plot.bipl5_biplot
# ─────────────────────────────────────────────────────────────────────────────

resolve_fit_display_mode <- function(bp, fit_display = c("inherit", "panel", "overlay")) {
  fit_display <- match.arg(fit_display)

  if (fit_display == "inherit") {
    stored <- bp$meta$plot_options$fit_display %||% "panel"
    fit_display <- match.arg(stored, c("panel", "overlay"))
  }

  if (is.null(bp$fit_measures)) {
    return("panel")
  }

  fit_display
}

fit_display_config <- function(mode = "panel") {
  list(
    mode = mode,
    panel = list(
      xaxis_domain = c(0, 0.5),
      xaxis3_domain = c(0.65, 1),
      yaxis3_domain = c(0.15, 0.85),
      yaxis3_position = 0.65,
      yaxis3_side = "left",
      table_domain_x = c(0.5, 1),
      table_domain_y = c(0.15, 0.85),
      slider_len = 0.5,
      menu_pad_right = 0
    ),
    overlay = list(
      xaxis_domain = c(0, 1),
      xaxis3_domain = c(0, 1),
      yaxis3_domain = c(0.15, 0.85),
      yaxis3_position = 0,
      yaxis3_side = "left",
      table_domain_x = c(0, 1),
      table_domain_y = c(0.15, 0.85),
      slider_len = 1,
      menu_pad_right = 60
    )
  )
}

#' Store a default fit-measure display mode on a biplot
#'
#' `overlay_fit()` is a convenience helper for pipelines. It does not refit the
#' underlying ordination; it only stores whether fit measures should default to
#' the right-hand panel or an overlay view when [plot()] is called.
#'
#' A later call to `plot(x, fit_display = ...)` always takes precedence over the
#' stored default.
#'
#' @param x A `bipl5_biplot` object with fit measures.
#' @param overlay Logical scalar. `TRUE` stores `"overlay"`; `FALSE` stores
#'   `"panel"`.
#'
#' @return A modified `bipl5_biplot`.
#' @export
overlay_fit <- function(x, overlay = TRUE) {
  if (!inherits(x, "bipl5_biplot")) {
    stop("'x' must inherit from 'bipl5_biplot'.", call. = FALSE)
  }
  if (is.null(x$fit_measures)) {
    stop("overlay_fit() requires a biplot with fit measures.", call. = FALSE)
  }
  if (!is.logical(overlay) || length(overlay) != 1L || is.na(overlay)) {
    stop("'overlay' must be TRUE or FALSE.", call. = FALSE)
  }

  out <- x
  plot_options <- out$meta$plot_options
  if (is.null(plot_options)) {
    plot_options <- list()
  }
  plot_options$fit_display <- if (overlay) "overlay" else "panel"
  out$meta$plot_options <- plot_options
  out
}

#' Plot a bipl5_biplot object
#'
#' Initialises a plotly graph, populates it with the first available mdsDisplay
#' traces and annotations, then attaches the remaining mdsDisplays and fit
#' measures to the JavaScript event handler.
#'
#' @param x A \code{bipl5_biplot} object
#' @param y Ignored (for S3 consistency)
#' @param fit_display How fit measures should be shown for biplots that support
#'   them: inherit the object's stored preference, render them in the right-hand
#'   panel, or render them as an overlay over the full plot width.
#' @param ... Additional arguments (ignored)
#'
#' @return A plotly htmlwidget
#' @export
#' @method plot bipl5_biplot
plot.bipl5_biplot <- function(
  x,
  y = NULL,
  fit_display = c("inherit", "panel", "overlay"),
  ...
) {
  bp <- x
  ez <- bp$meta$x
  pc_info <- bp$meta$pc_info
  has_fm <- !is.null(bp$fit_measures)
  is_cva <- "cva" %in% class(bp)
  is_reg <- "reg" %in% class(bp)
  is_pco <- "pco" %in% class(bp)
  is_spline <- isTRUE(bp$meta$spline)
  fit_display_mode <- resolve_fit_display_mode(bp, fit_display)
  fit_display_cfg <- fit_display_config(fit_display_mode)

  # ── Detect available mdsDisplays ──────────────────────────────────────────────
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

  # The first available mdsDisplay is rendered directly into plotly
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

  # ── Step 1b: Trim PC dropdown buttons to available mdsDisplays ────────────────
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

  # ── Step 2: Add first mdsDisplay traces to plotly ─────────────────────────────
  for (tr in first_payl$mdsDisplay$trace_data) {
    p_ly <- do.call(plotly::add_trace, c(list(p = p_ly), tr))
  }

  # ── Step 3: Add first mdsDisplay annotations ──────────────────────────────────
  if (length(first_payl$mdsDisplay$layout$annotations) > 0) {
    p_ly <- plotly::layout(
      p_ly,
      annotations = first_payl$mdsDisplay$layout$annotations
    )
  }

  # ── Step 4: Build mdsDisplay for JS ───────────────────────────────────────────
  mdsDisplay_for_js <- list()
  for (nm in available) {
    lbl <- pc_map[nm]
    if (nm == first_name) {
      mdsDisplay_for_js[[lbl]] <- list(
        config = first_payl$mdsDisplay$config,
        fit_table = if (has_fm) bp$fit_measures[[ft_map[nm]]]
      )
    } else {
      js_payl <- bp[[nm]]$mdsDisplay
      if (has_fm) {
        js_payl$fit_table <- bp$fit_measures[[ft_map[nm]]]
      }
      mdsDisplay_for_js[[lbl]] <- js_payl
    }
  }

  # ── Step 5: Build fit measures mdsDisplay for JS ──────────────────────────────
  fm_mdsDisplay <- if (has_fm) {
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
      mdsDisplay = mdsDisplay_for_js,
      fm_mdsDisplay = fm_mdsDisplay,
      fit_display_cfg = fit_display_cfg,
      initial_pc_key = pc_map[first_name]
    )
  }

  p_ly
}

fit_plot_labels <- function(key) {
  switch(
    key,
    "Cum. Predictivity" = list(
      title = paste(
        "Figure 1: Cumulative quality and axis predictivities",
        "across the subspace.",
        sep = "\n"
      ),
      x = "Dimension of subspace",
      y = "Overall quality and axis predictivities (cumulative)"
    ),
    "Cum. Adequacy" = list(
      title = paste(
        "Figure 2: Cumulative adequacy across dimensions",
        "of the subspace.",
        sep = "\n"
      ),
      x = "Dimension of subspace",
      y = "Cumulative adequacy"
    ),
    "Scree Plot" = list(
      title = paste(
        "Figure 3: Scree profile of eigenvalues across",
        "subspace dimensions.",
        sep = "\n"
      ),
      x = "Dimension of subspace",
      y = "Scree profile (eigenvalues)"
    ),
    "Variance Explained" = list(
      title = paste(
        "Figure 4: Cumulative proportion of total variation",
        "explained across subspace dimensions.",
        sep = "\n"
      ),
      x = "Dimension of subspace",
      y = "Proportion of total variation (cumulative)"
    ),
    stop("Unsupported fit graph type '", key, "'.", call. = FALSE)
  )
}

plotly_default_colorway <- function() {
  c(
    "#1F77B4",
    "#FF7F0E",
    "#2CA02C",
    "#D62728",
    "#9467BD",
    "#8C564B",
    "#E377C2",
    "#7F7F7F",
    "#BCBD22",
    "#17BECF"
  )
}

clean_plotly_text <- function(x) {
  x <- x %||% ""
  x <- gsub("<[^>]+>", "", x)
  trimws(x)
}

trace_meta_values <- function(trace) {
  meta <- trace$meta
  if (is.null(meta)) {
    return(character(0))
  }
  unname(as.character(unlist(meta, use.names = FALSE)))
}

infer_bipl5_fit_key <- function(trace_data) {
  is_var_exp <- vapply(
    trace_data,
    function(tr) {
      identical(tr$type, "bar") || identical(tr$legendgroup, "VarExplained")
    },
    logical(1)
  )
  if (any(is_var_exp)) {
    return("Variance Explained")
  }

  meta_vals <- unique(unlist(
    lapply(trace_data, trace_meta_values),
    use.names = FALSE
  ))
  known <- c(
    "Cum. Predictivity",
    "Cum. Adequacy",
    "Scree Plot",
    "Variance Explained"
  )
  hit <- intersect(known, meta_vals)

  if (length(hit) > 0) {
    return(hit[[1]])
  }

  stop(
    "Unable to infer fit graph type from the stored Plotly traces.",
    call. = FALSE
  )
}

plotly_dash_to_ggplot <- function(dash) {
  switch(
    dash %||% "solid",
    solid = "solid",
    dash = "dashed",
    dot = "dotted",
    dashdot = "dotdash",
    longdash = "longdash",
    longdashdot = "twodash",
    "solid"
  )
}

fit_trace_data_frame <- function(trace, series_name = trace$name %||% "Trace") {
  data.frame(
    x = as.numeric(unlist(trace$x)),
    y = as.numeric(unlist(trace$y)),
    series = series_name,
    stringsAsFactors = FALSE
  )
}

fit_trace_legend_title <- function(trace_data) {
  titles <- vapply(
    trace_data,
    function(tr) clean_plotly_text(tr$legendgrouptitle$text %||% NULL),
    character(1)
  )
  titles <- titles[nzchar(titles)]
  if (length(titles) > 0) titles[[1]] else NULL
}

fit_trace_color <- function(trace, fallback) {
  as.character(
    trace$line$color %||%
      trace$marker$color %||%
      trace$marker$line$color %||%
      fallback
  )
}

fit_trace_width <- function(trace, default = 1.25) {
  as.numeric(trace$line$width %||% default)
}

fit_trace_alpha <- function(trace, default = 1) {
  as.numeric(trace$marker$opacity %||% trace$opacity %||% default)
}

fit_plot_theme <- function() {
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = "#E5ECF6"),
      legend.position = "right",
      legend.title = ggplot2::element_text(face = "bold"),
      plot.title = ggplot2::element_text(face = "bold"),
      axis.title = ggplot2::element_text(face = "bold")
    )
}

build_bipl5_fit_line_plot <- function(trace_data, fit_key) {
  labels <- fit_plot_labels(fit_key)
  palette <- plotly_default_colorway()
  legend_title <- fit_trace_legend_title(trace_data)
  series_levels <- vapply(
    trace_data,
    function(tr) tr$name %||% labels$title,
    character(1)
  )
  colors <- vapply(
    seq_along(trace_data),
    function(i) {
      fit_trace_color(
        trace_data[[i]],
        palette[((i - 1) %% length(palette)) + 1]
      )
    },
    character(1)
  )
  linetypes <- vapply(
    trace_data,
    function(tr) plotly_dash_to_ggplot(tr$line$dash %||% "solid"),
    character(1)
  )
  widths <- vapply(trace_data, fit_trace_width, numeric(1))

  p <- ggplot2::ggplot()

  for (i in seq_along(trace_data)) {
    df <- fit_trace_data_frame(trace_data[[i]], series_levels[[i]])
    df$series <- factor(df$series, levels = series_levels)

    p <- p +
      ggplot2::geom_line(
        data = df,
        mapping = ggplot2::aes(
          x = x,
          y = y,
          color = series,
          linetype = series,
          group = series
        ),
        linewidth = widths[[i]],
        na.rm = TRUE
      )

    if (grepl("markers", trace_data[[i]]$mode %||% "", fixed = TRUE)) {
      p <- p +
        ggplot2::geom_point(
          data = df,
          mapping = ggplot2::aes(x = x, y = y, color = series),
          size = 2.2,
          na.rm = TRUE
        )
    }
  }

  x_breaks <- sort(unique(unlist(
    lapply(trace_data, function(tr) as.numeric(unlist(tr$x))),
    use.names = FALSE
  )))
  color_scale <- stats::setNames(colors, series_levels)
  linetype_scale <- stats::setNames(linetypes, series_levels)

  p +
    ggplot2::scale_color_manual(
      values = color_scale,
      breaks = series_levels,
      name = legend_title %||% NULL
    ) +
    ggplot2::scale_linetype_manual(
      values = linetype_scale,
      breaks = series_levels,
      name = legend_title %||% NULL
    ) +
    ggplot2::scale_x_continuous(breaks = x_breaks, minor_breaks = NULL) +
    ggplot2::labs(
      title = labels$title,
      x = labels$x,
      y = labels$y,
      colour = legend_title %||% NULL,
      linetype = legend_title %||% NULL
    ) +
    ggplot2::coord_cartesian(
      ylim = if (identical(fit_key, "Scree Plot")) NULL else c(0, 1)
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(title = legend_title %||% NULL),
      linetype = ggplot2::guide_legend(title = legend_title %||% NULL)
    ) +
    fit_plot_theme()
}

build_bipl5_fit_variance_plot <- function(trace_data) {
  labels <- fit_plot_labels("Variance Explained")
  palette <- plotly_default_colorway()
  is_bar <- vapply(
    trace_data,
    function(tr) identical(tr$type, "bar"),
    logical(1)
  )
  bar_traces <- trace_data[is_bar]
  line_traces <- trace_data[!is_bar]
  bar_title <- fit_trace_legend_title(bar_traces)
  line_title <- fit_trace_legend_title(line_traces)

  p <- ggplot2::ggplot()
  x_breaks <- numeric(0)

  if (length(bar_traces) > 0) {
    bar_levels <- vapply(
      bar_traces,
      function(tr) tr$name %||% "Trace",
      character(1)
    )
    bar_colors <- vapply(
      seq_along(bar_traces),
      function(i) {
        fit_trace_color(
          bar_traces[[i]],
          palette[((i - 1) %% length(palette)) + 1]
        )
      },
      character(1)
    )
    bar_df <- do.call(
      rbind,
      lapply(seq_along(bar_traces), function(i) {
        fit_trace_data_frame(bar_traces[[i]], bar_levels[[i]])
      })
    )
    bar_df$series <- factor(bar_df$series, levels = bar_levels)
    x_breaks <- c(x_breaks, bar_df$x)

    p <- p +
      ggplot2::geom_col(
        data = bar_df,
        mapping = ggplot2::aes(x = x, y = y, fill = series),
        alpha = fit_trace_alpha(bar_traces[[1]], default = 0.7),
        width = 0.85,
        position = "stack",
        na.rm = TRUE
      ) +
      ggplot2::scale_fill_manual(
        values = stats::setNames(bar_colors, bar_levels),
        breaks = bar_levels,
        name = bar_title %||% NULL
      )
  }

  if (length(line_traces) > 0) {
    line_levels <- vapply(
      line_traces,
      function(tr) tr$name %||% labels$title,
      character(1)
    )
    line_colors <- vapply(
      seq_along(line_traces),
      function(i) {
        fit_trace_color(
          line_traces[[i]],
          palette[((i - 1) %% length(palette)) + 1]
        )
      },
      character(1)
    )
    line_linetypes <- vapply(
      line_traces,
      function(tr) plotly_dash_to_ggplot(tr$line$dash %||% "solid"),
      character(1)
    )
    line_widths <- vapply(line_traces, fit_trace_width, numeric(1))
    for (i in seq_along(line_traces)) {
      df <- fit_trace_data_frame(line_traces[[i]], line_levels[[i]])
      df$series <- factor(df$series, levels = line_levels)
      x_breaks <- c(x_breaks, df$x)

      p <- p +
        ggplot2::geom_line(
          data = df,
          mapping = ggplot2::aes(
            x = x,
            y = y,
            color = series,
            linetype = series,
            group = series
          ),
          linewidth = line_widths[[i]],
          na.rm = TRUE
        )

      if (grepl("markers", line_traces[[i]]$mode %||% "", fixed = TRUE)) {
        p <- p +
          ggplot2::geom_point(
            data = df,
            mapping = ggplot2::aes(x = x, y = y, color = series),
            size = 2.2,
            na.rm = TRUE
          )
      }
    }

    p <- p +
      ggplot2::scale_color_manual(
        values = stats::setNames(line_colors, line_levels),
        breaks = line_levels,
        name = line_title %||% NULL
      ) +
      ggplot2::scale_linetype_manual(
        values = stats::setNames(line_linetypes, line_levels),
        breaks = line_levels,
        name = line_title %||% NULL
      ) +
      ggplot2::guides(
        fill = ggplot2::guide_legend(
          title = bar_title %||% NULL,
          order = 1
        ),
        color = ggplot2::guide_legend(title = line_title %||% NULL, order = 2),
        linetype = ggplot2::guide_legend(
          title = line_title %||% NULL,
          order = 2
        )
      )
  }

  p +
    ggplot2::scale_x_continuous(
      breaks = sort(unique(x_breaks)),
      minor_breaks = NULL
    ) +
    ggplot2::labs(
      title = labels$title,
      x = labels$x,
      y = labels$y,
      fill = bar_title %||% NULL,
      colour = line_title %||% NULL,
      linetype = line_title %||% NULL
    ) +
    ggplot2::coord_cartesian(ylim = c(0, 1)) +
    fit_plot_theme()
}

#' Plot a single extracted fit graph as a ggplot
#'
#' Reconstructs one of the PCA fit graphs from its stored plotly traces. The fit
#' type is inferred from the trace metadata and trace types, then translated into
#' a \pkg{ggplot2} chart with matching title, legend titles, and axes.
#'
#' Supported fit graphs are cumulative predictivity (\code{CumPred}),
#' cumulative adequacy (\code{CumAd}), variance explained (\code{VarExp}), and
#' the scree plot (\code{Scree}). The summary-table fit objects are not handled
#' by this plotting method.
#'
#' @param x A \code{bipl5_fit} object, typically returned by
#'   \code{extract(bp, fit_measures, CumPred)} or a similar fit graph extraction.
#' @param y Ignored (for S3 consistency)
#' @param ... Additional arguments (ignored)
#'
#' @return A \code{ggplot} object.
#'
#' @examples
#' bp <- biplotEZ::biplot(iris[, 1:4]) |>
#'   biplotEZ::PCA() |>
#'   wrap_bipl5()
#'
#' fit_plot <- extract(bp, fit_measures, Scree)
#' plot(fit_plot)
#' @export
#' @method plot bipl5_fit
plot.bipl5_fit <- function(x, y = NULL, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("plot.bipl5_fit() requires the ggplot2 package.", call. = FALSE)
  }

  trace_data <- x$trace_data
  fit_key <- infer_bipl5_fit_key(trace_data)

  if (identical(fit_key, "Variance Explained")) {
    return(build_bipl5_fit_variance_plot(trace_data))
  }

  build_bipl5_fit_line_plot(trace_data, fit_key)
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
  all_mdsDisplays <- names(pc_info)
  all_labels <- vapply(pc_info, function(info) info$label, character(1))

  # Only print non-NULL mdsDisplays
  present <- which(
    !vapply(all_mdsDisplays, function(k) is.null(x[[k]]), logical(1))
  )

  for (j in seq_along(present)) {
    i <- present[j]
    pname <- all_mdsDisplays[i]
    payl <- x[[pname]]
    is_last <- (j == length(present) && is.null(x$fit_measures))
    branch <- if (is_last) "\u2514\u2500\u2500 " else "\u251C\u2500\u2500 "
    pipe <- if (is_last) "    " else "\u2502   "

    cat(
      branch,
      cyan(bold(paste0(pname, " [", all_labels[i], "]"))),
      silver(" <bipl5_mdsDisplay>"),
      "\n",
      sep = ""
    )

    # Data sub-element
    print_data_subtree(payl$Data, pipe)

    # Traces
    n_traces <- length(payl$mdsDisplay$trace_data)
    cat(
      pipe,
      "\u251C\u2500\u2500 ",
      green("trace_data"),
      silver(paste0("  [", n_traces, " traces]")),
      "\n",
      sep = ""
    )

    # Annotations
    n_ann <- length(payl$mdsDisplay$layout$annotations)
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


#' Print a bipl5_mdsDisplay object
#'
#' @param x A \code{bipl5_mdsDisplay} object
#' @param ... Additional arguments (ignored)
#'
#' @return Invisibly returns \code{x}
#' @export
#' @method print bipl5_mdsDisplay
print.bipl5_mdsDisplay <- function(x, ...) {
  bold <- crayon::bold
  cyan <- crayon::cyan
  green <- crayon::green
  silver <- crayon::silver

  cat(bold(cyan("bipl5_mdsDisplay")), "\n")
  if (!is.null(x$fit_qual)) {
    cat(silver(x$fit_qual), "\n")
  }

  # Data
  print_data_subtree(x$Data, "")

  # Traces
  n_traces <- length(x$mdsDisplay$trace_data)
  cat(
    "\u251C\u2500\u2500 ",
    green("trace_data"),
    silver(paste0("  [", n_traces, " traces]")),
    "\n",
    sep = ""
  )

  # Annotations
  n_ann <- length(x$mdsDisplay$layout$annotations)
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
# subset_biplot – internal helper for mdsDisplay subsetting
# ─────────────────────────────────────────────────────────────────────────────

#' Subset a bipl5_biplot to keep only specified mdsDisplays
#'
#' Internal engine used by \code{extract()}, \code{remove_mdsDisplay()}, and
#' \code{append_mdsDisplay()} when a new top-level \code{bipl5_biplot} needs to be
#' assembled from an existing one. The order of \code{keep} is preserved and
#' becomes the new mdsDisplay order in \code{meta$pc_info}, which in turn controls
#' the initial plot shown by \code{plot.bipl5_biplot()}.
#'
#' @param bp A \code{bipl5_biplot} object
#' @param keep Character vector of mdsDisplay names to retain
#'   (e.g. \code{"mdsDisplay_12"} or \code{c("mdsDisplay_12", "mdsDisplay_23")})
#'
#' @return A new \code{bipl5_biplot} with only the specified mdsDisplays and
#'   their corresponding fit tables. Shared PCA fit charts
#'   (\code{CumPred}, \code{CumAd}, \code{VarExp}, \code{Scree}) are preserved.
#' @noRd
subset_biplot <- function(bp, keep) {
  pc_info <- bp$meta$pc_info
  valid <- names(pc_info)
  bad <- setdiff(keep, valid)
  if (length(bad) > 0) {
    stop(
      "Unknown mdsDisplay(s): ",
      paste(bad, collapse = ", "),
      ". Must be one of: ",
      paste(valid, collapse = ", "),
      call. = FALSE
    )
  }

  # Subset mdsDisplays
  new_mdsDisplays <- list()
  for (nm in keep) {
    new_mdsDisplays[[nm]] <- bp[[nm]]
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
  new_bipl5_biplot(new_mdsDisplays, new_fm, new_meta, biplot_type = biplot_type)
}

fit_graph_names <- function() c("CumPred", "CumAd", "VarExp", "Scree")

extract_fit_component <- function(object, what_chr) {
  fm <- object$fit_measures

  if (is.null(fm)) {
    stop("This biplot has no fit measures.", call. = FALSE)
  }

  value <- fm[[what_chr]]
  if (is.null(value)) {
    stop("Fit measure '", what_chr, "' not found.", call. = FALSE)
  }

  if (what_chr %in% fit_graph_names()) {
    return(new_bipl5_fit(value, fit_name = what_chr))
  }

  value
}

is_fit_graph_path <- function(path) {
  length(path) == 2 &&
    identical(path[[1]], "fit_measures") &&
    path[[2]] %in% fit_graph_names()
}


# ─────────────────────────────────────────────────────────────────────────────
# extract() – drill into a bipl5_biplot with bare names
# ─────────────────────────────────────────────────────────────────────────────

#' Extract nested components from a bipl5_biplot object
#'
#' Three calling styles are supported:
#' \enumerate{
#'   \item \strong{mdsDisplay subset}: \code{extract(bp, mdsDisplay_12)} — returns a
#'     new \code{bipl5_biplot} containing only that mdsDisplay (plottable).
#'   \item \strong{Two-level}: \code{extract(bp, from = mdsDisplay_12, what = sample_coordinates)}
#'     — returns the nested data element.
#'   \item \strong{Arbitrary depth}: \code{extract(bp, mdsDisplay_12$Data$sample_coordinates)}
#'     — returns the nested data element.
#' }
#'
#' In addition to the mdsDisplay access patterns above, graph-based fit measures
#' can be extracted directly with calls such as
#' \code{extract(bp, fit_measures, CumPred)} or
#' \code{extract(bp, fit_measures$CumPred)}. Supported graph-based fit measures
#' are \code{CumPred}, \code{CumAd}, \code{VarExp}, and \code{Scree}. These
#' calls return a \code{bipl5_fit} object that can be passed to \code{plot()} to
#' obtain a static \pkg{ggplot2} version of the corresponding fit graph.
#'
#' @param object A \code{bipl5_biplot} object
#' @param expr An unquoted mdsDisplay name (e.g. \code{mdsDisplay_12}) or a path
#'   expression using \code{$} (e.g. \code{mdsDisplay_12$Data$sample_coordinates}
#'   or \code{fit_measures$CumPred})
#' @param from Unquoted name of the top-level element
#' @param what Unquoted name of the nested element
#'
#' @return A \code{bipl5_biplot} (mdsDisplay subset), a \code{bipl5_fit} object
#'   for graph-based fit measures, or the requested sub-element.
#'
#' @examples
#' bp <- biplotEZ::biplot(iris[, 1:4]) |>
#'   biplotEZ::PCA() |>
#'   wrap_bipl5()
#'
#' only_12 <- extract(bp, mdsDisplay_12)
#' data_obj <- extract(bp, from = mdsDisplay_12, what = Data)
#' coords <- extract(bp, mdsDisplay_12$Data$sample_coordinates)
#'
#' fit_plot <- extract(bp, fit_measures, CumPred)
#' plot(fit_plot)
#' @export
extract <- function(object, expr, from, what) {
  UseMethod("extract")
}

#' @rdname extract
#' @export
#' @method extract bipl5_biplot
extract.bipl5_biplot <- function(object, expr, from, what) {
  mdsDisplay_names <- names(object$meta$pc_info)

  # Determine which style was used
  if (!missing(expr) && !missing(from) && missing(what)) {
    expr_sub <- substitute(expr)
    from_sub <- substitute(from)
    if (is.symbol(expr_sub)) {
      expr_chr <- as.character(expr_sub)
      from_chr <- as.character(from_sub)
      if (identical(expr_chr, "fit_measures")) {
        return(extract_fit_component(object, from_chr))
      }
      return(object[[expr_chr]][[from_chr]])
    }
  }

  if (!missing(from) && !missing(what)) {
    from_chr <- as.character(substitute(from))
    what_chr <- as.character(substitute(what))
    if (identical(from_chr, "fit_measures")) {
      return(extract_fit_component(object, what_chr))
    }
    return(object[[from_chr]][[what_chr]])
  }

  if (!missing(expr)) {
    e <- substitute(expr)

    # Single bare symbol matching a mdsDisplay name → subset biplot
    if (is.symbol(e)) {
      nm <- as.character(e)
      if (nm %in% mdsDisplay_names) {
        return(subset_biplot(object, nm))
      }
    }

    # Otherwise: arbitrary depth via $ expression
    path <- deparse_path(e)
    if (is_fit_graph_path(path)) {
      return(extract_fit_component(object, path[[2]]))
    }
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
#' \code{mdsDisplay_12$Data$sample_coordinates}. This helper walks the nested
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
    "extract() expects a path like mdsDisplay_12$Data$sample_coordinates",
    call. = FALSE
  )
}


# ─────────────────────────────────────────────────────────────────────────────
# remove_mdsDisplay() – drop a mdsDisplay from a bipl5_biplot
# ─────────────────────────────────────────────────────────────────────────────

#' Remove a mdsDisplay from a bipl5_biplot object
#'
#' Returns a new \code{bipl5_biplot} with the specified mdsDisplay (and its
#' corresponding fit table) removed.  At least one mdsDisplay must remain.
#'
#' @param object A \code{bipl5_biplot} object
#' @param mdsDisplay Unquoted name of the mdsDisplay to remove
#'   (e.g. \code{mdsDisplay_13})
#'
#' @return A new \code{bipl5_biplot} without the removed mdsDisplay
#' @export
remove_mdsDisplay <- function(object, mdsDisplay) {
  UseMethod("remove_mdsDisplay")
}

#' @rdname remove_mdsDisplay
#' @export
#' @method remove_mdsDisplay bipl5_biplot
remove_mdsDisplay.bipl5_biplot <- function(object, mdsDisplay) {
  if (any(c("reg", "pco") %in% class(object))) {
    stop(
      "remove_mdsDisplay() is not supported for this biplot type.",
      call. = FALSE
    )
  }
  nm <- as.character(substitute(mdsDisplay))
  all_mdsDisplays <- names(object$meta$pc_info)

  if (!nm %in% all_mdsDisplays) {
    stop(
      "'",
      nm,
      "' is not a valid mdsDisplay name. ",
      "Must be one of: ",
      paste(all_mdsDisplays, collapse = ", "),
      call. = FALSE
    )
  }
  if (is.null(object[[nm]])) {
    stop("mdsDisplay '", nm, "' does not exist in this object.", call. = FALSE)
  }

  keep <- setdiff(all_mdsDisplays, nm)
  keep <- keep[!vapply(keep, function(k) is.null(object[[k]]), logical(1))]

  if (length(keep) == 0) {
    stop("Cannot remove the last remaining mdsDisplay.", call. = FALSE)
  }

  subset_biplot(object, keep)
}


# ─────────────────────────────────────────────────────────────────────────────
# append_mdsDisplay() – add a new PC pair to an existing bipl5_biplot
# ─────────────────────────────────────────────────────────────────────────────

#' Append a mdsDisplay to a bipl5_biplot object
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
#' @return A new \code{bipl5_biplot} with the additional mdsDisplay appended
#' @export
append_mdsDisplay <- function(object, eigenvectors) {
  UseMethod("append_mdsDisplay")
}

#' @rdname append_mdsDisplay
#' @export
#' @method append_mdsDisplay bipl5_biplot
append_mdsDisplay.bipl5_biplot <- function(object, eigenvectors) {
  if (any(c("reg", "pco") %in% class(object))) {
    stop(
      "append_mdsDisplay() is not supported for this biplot type.",
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

  pname <- mdsDisplay_name(pcs)
  if (pname %in% names(object$meta$pc_info)) {
    stop(
      pname,
      " already exists in this object. ",
      "Existing mdsDisplays: ",
      paste(names(object$meta$pc_info), collapse = ", "),
      call. = FALSE
    )
  }

  # ── Build the new mdsDisplay ─────────────────────────────────────────────────
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

  new_payl <- build_one_mdsDisplay(
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

  if (!is.null(object$meta$sample_format)) {
    sample_state <- format_samples_get_state(object, object$meta$x$n)
    new_payl <- format_samples_rebuild_mdsDisplay(
      mds = new_payl,
      update_means = format_samples_should_update_means(object, sample_state),
      state = sample_state,
      ez_obj = object$meta$x,
      rebuild_tda = !is.null(sample_state$color)
    )
  }

  # ── Append to existing object ─────────────────────────────────────────────
  # Copy all existing mdsDisplays
  all_mdsDisplays <- list()
  for (nm in names(object$meta$pc_info)) {
    all_mdsDisplays[[nm]] <- object[[nm]]
  }
  all_mdsDisplays[[pname]] <- new_payl

  # Build new fit_measures with the extra table (skip for CVA)
  fm <- object$fit_measures
  if (!is.null(fm)) {
    tmp <- list(mdsDisplay = list())
    tmp <- add_table_mdsDisplay(tmp, x = ez_obj)
    new_ft <- tmp$mdsDisplay$fit_table

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

  new_bipl5_biplot(all_mdsDisplays, new_fm, new_meta, biplot_type = biplot_type)
}
