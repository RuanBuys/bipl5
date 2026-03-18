# Construct a bipl5_biplot from a PCA biplot

Builds the mdsDisplay(s) used for a principal component analysis (PCA)
biplot and documents the associated PCA-biplot fit, predictivity and
direct-reading measures. In contrast to a regression biplot, the
low-dimensional sample map is obtained internally from the singular
value decomposition of the processed data matrix. If the wrapped object
stores more than one principal-component pair as separate mdsDisplays,
the same formulas below apply to each mdsDisplay separately, with the
active mdsDisplay determined by the displayed pair of principal
components.

## Usage

``` r
# S3 method for class 'PCA'
wrap_bipl5(x)
```

## Arguments

- x:

  An object of class `biplot` from the biplotEZ package with
  [`PCA()`](https://rdrr.io/pkg/biplotEZ/man/PCA.html) method applied.

## Value

An object of class `c("bipl5_biplot", "PCA")`

## Details

For the PCA biplot handled by this method, let
\\\mathbf{X}\in\mathbb{R}^{n\times p}\\ denote the *processed* data
matrix stored in the input `biplot` object after centring and any
optional scaling performed by
[`biplot()`](https://rdrr.io/pkg/biplotEZ/man/biplot.html). Thus
\\\mathbf{X}\\ is the matrix on which PCA is actually carried out. Write
the singular value decomposition as \$\$\mathbf{X} =
\mathbf{U}\mathbf{D}\mathbf{V}^{\top},\$\$ where
\\\mathbf{U}^{\top}\mathbf{U}=\mathbf{I}\\,
\\\mathbf{V}^{\top}\mathbf{V}=\mathbf{I}\\, and
\\\mathbf{D}=\mathrm{diag}(d_1,\ldots,d_q)\\ with
\\q=\mathrm{rank}(\mathbf{X})\\ and \\d_1\ge \cdots \ge d_q \ge 0\\. The
columns of \\\mathbf{V}\\ are the principal directions, and the
corresponding principal component score vectors are \\\mathbf{z}\_t =
d_t\mathbf{u}\_t = \mathbf{X}\mathbf{v}\_t\\, \\t=1,\ldots,q\\. This is
the standard PCA biplot construction underlying Gabriel's original
formulation and subsequent calibrated-axis developments (Gabriel, 1971;
Gower and Hand, 1996; Gower, Lubbe and le Roux, 2011; Greenacre, 2010).

Suppose the user has selected two principal components \\a\<b\\. Let
\\\mathbf{J}\_{ab}\\ denote the diagonal \\q\times q\\ selector matrix
with ones in positions \\a\\ and \\b\\ and zeros elsewhere. Then the
two-dimensional PCA fitted matrix is \$\$\widehat{\mathbf{X}}\_{ab} =
\mathbf{U}\mathbf{D}\mathbf{J}\_{ab}\mathbf{V}^{\top} =
\mathbf{X}\mathbf{V}\mathbf{J}\_{ab}\mathbf{V}^{\top}.\$\$ Equivalently,
if \\\mathbf{U}\_{ab}\\ and \\\mathbf{V}\_{ab}\\ denote the submatrices
containing columns \\a\\ and \\b\\, and
\\\mathbf{D}\_{ab}=\mathrm{diag}(d_a,d_b)\\, then
\$\$\widehat{\mathbf{X}}\_{ab} =
\mathbf{U}\_{ab}\mathbf{D}\_{ab}\mathbf{V}\_{ab}^{\top} =
d_a\mathbf{u}\_a\mathbf{v}\_a^{\top} +
d_b\mathbf{u}\_b\mathbf{v}\_b^{\top}.\$\$ When \\(a,b)=(1,2)\\, this is
the best rank-2 approximation to \\\mathbf{X}\\ in Frobenius norm by the
Eckart–Young theorem. For any other selected pair, the same formula
gives the orthogonal projection of \\\mathbf{X}\\ onto the chosen
two-dimensional principal-component subspace, but it is not generally
the globally optimal rank-2 approximation (Eckart and Young, 1936;
Gabriel, 1971; Greenacre, 2010).

The calibrated-axis PCA biplot may be written in the general form
\$\$\widehat{\mathbf{X}}\_{ab} =
\mathbf{Z}\_{ab}\mathbf{H}\_{ab}^{\top},\$\$ where the exact
factorization depends on the type of PCA biplot being displayed.

For the ordinary PCA biplot, which prioritizes the Euclidean geometry of
the sample points, take \$\$\mathbf{Z}\_{ab} =
\mathbf{U}\_{ab}\mathbf{D}\_{ab}, \qquad \mathbf{H}\_{ab} =
\mathbf{V}\_{ab}.\$\$ Thus the displayed sample coordinates are the
selected principal component scores, and the fitted matrix is
\$\$\widehat{\mathbf{X}}\_{ab} =
\mathbf{Z}\_{ab}\mathbf{H}\_{ab}^{\top}.\$\$ If \\\mathbf{h}\_{(j)}\\
denotes the \\j\\th row of \\\mathbf{H}\_{ab}\\ written as a column
vector in \\\mathbb{R}^2\\, then for sample \\i\\ \$\$\widehat{x}\_{ij}
= \mathbf{z}\_{i}^{\top}\mathbf{h}\_{(j)}.\$\$ Hence the calibrated axis
for variable \\j\\ has direction \\\mathbf{h}\_{(j)}\\, and the point on
that axis corresponding to marker value \\\mu\\ is \$\$\mathbf{p}\_{\mu
j} =
\frac{\mu}{\mathbf{h}\_{(j)}^{\top}\mathbf{h}\_{(j)}}\mathbf{h}\_{(j)}.\$\$
This is the standard calibrated-axis formula used to place tick marks
and to recover predicted values from projections onto the displayed axis
(Gower and Hand, 1996; Gower, Lubbe and le Roux, 2011).

A second important special case is the *correlation biplot*, obtained
when the processed matrix \\\mathbf{X}\\ is standardized and the display
is chosen so that correlations between variables are approximated by the
cosines of the angles between the displayed variable directions. In that
case one may equivalently factorize \$\$\widehat{\mathbf{X}}\_{ab} =
\mathbf{U}\_{ab}(\mathbf{V}\_{ab}\mathbf{D}\_{ab})^{\top}.\$\$ Hence the
displayed sample coordinates are \\\mathbf{U}\_{ab}\\ and the variable
directions are the rows of \\\mathbf{V}\_{ab}\mathbf{D}\_{ab}\\. If
\\\mathbf{c}\_{(j)}\\ denotes the \\j\\th such row written as a column
vector, then \$\$\widehat{x}\_{ij} =
\mathbf{u}\_{i,ab}^{\top}\mathbf{c}\_{(j)}.\$\$ In this standardized
setting the coordinates \\\mathbf{c}\_{(j)}\\ are proportional to the
correlations of variable \\j\\ with the selected principal components,
and the geometry of the displayed variable directions is therefore tuned
to the correlation structure rather than to the raw score geometry of
the samples. This is the sense in which `correlation.biplot = TRUE`
preserves variable-correlation information in the display (Gabriel,
1971; Greenacre, 2010; biplotEZ manual and vignette).

In either factorization, all predicted values in
\\\widehat{\mathbf{X}}\_{ab}\\ are on the same centred/scaled scale as
the stored matrix \\\mathbf{X}\\. If required, predictions can be
back-transformed to the original variable scale using the means and
standard deviations stored in the input `biplot` object.

A fundamental feature of the PCA biplot is that both *sample-side* and
*variable-side* orthogonal decompositions hold. Writing
\\\mathbf{E}\_{ab}=\mathbf{X}-\widehat{\mathbf{X}}\_{ab}\\, one has
\$\$\mathbf{X}\mathbf{X}^{\top} =
\widehat{\mathbf{X}}\_{ab}\widehat{\mathbf{X}}\_{ab}^{\top} +
\mathbf{E}\_{ab}\mathbf{E}\_{ab}^{\top},\$\$ and
\$\$\mathbf{X}^{\top}\mathbf{X} =
\widehat{\mathbf{X}}\_{ab}^{\top}\widehat{\mathbf{X}}\_{ab} +
\mathbf{E}\_{ab}^{\top}\mathbf{E}\_{ab}.\$\$ The first is the *Type A
orthogonality*, which justifies sample-side measures of fit. The second
is the *Type B orthogonality*, which justifies variable-side measures of
fit. For PCA both orthogonality relations hold simultaneously because
\\\widehat{\mathbf{X}}\_{ab}\\ is obtained from an orthogonal
principal-component projection (Gabriel, 1971; Gower, Lubbe and le Roux,
2011; Gardner-Lubbe, le Roux and Gower, 2008).

Let \\\mathbf{x}\_{i\cdot}^{\top}\\ denote row \\i\\ of \\\mathbf{X}\\
and \\\widehat{\mathbf{x}}\_{i\cdot}^{\top}\\ the corresponding row of
\\\widehat{\mathbf{X}}\_{ab}\\. The *sample predictivity* of sample
\\i\\ is then \$\$\psi_i = \frac{\\\widehat{\mathbf{x}}\_{i\cdot}\\^2}
{\\\mathbf{x}\_{i\cdot}\\^2} = 1 - \frac{\\\mathbf{x}\_{i\cdot} -
\widehat{\mathbf{x}}\_{i\cdot}\\^2} {\\\mathbf{x}\_{i\cdot}\\^2}, \qquad
i=1,\ldots,n.\$\$ Thus \\\psi_i\\ is the proportion of the sum of
squares of sample \\i\\ reproduced by the chosen two-dimensional PCA
display. Because of Type A orthogonality, \\0\le \psi_i \le 1\\. Samples
with \\\psi_i\\ near one lie close to the displayed PCA plane, whereas
samples with \\\psi_i\\ near zero lie largely orthogonal to it. This is
the sample-side fit measure used in the PCA biplot literature and in
biplotEZ (Gardner-Lubbe, le Roux and Gower, 2008; biplotEZ vignette).

Let \\\mathbf{x}\_{(j)}\\ denote column \\j\\ of \\\mathbf{X}\\ and
\\\widehat{\mathbf{x}}\_{(j)}\\ the corresponding column of
\\\widehat{\mathbf{X}}\_{ab}\\. The *axis predictivity* of variable
\\j\\ is \$\$\phi_j = \frac{\\\widehat{\mathbf{x}}\_{(j)}\\^2}
{\\\mathbf{x}\_{(j)}\\^2} = 1 - \frac{\\\mathbf{x}\_{(j)} -
\widehat{\mathbf{x}}\_{(j)}\\^2} {\\\mathbf{x}\_{(j)}\\^2}, \qquad
j=1,\ldots,p.\$\$ Thus \\\phi_j\\ is the proportion of the sum of
squares of variable \\j\\ reproduced by the chosen PCA plane. Because of
Type B orthogonality, \\0\le \phi_j \le 1\\. In a calibrated-axis
display, \\\phi_j\\ is the natural sum-of-squares measure of how well
the axis for variable \\j\\ reproduces the underlying processed values.
This is the quantity reported in biplotEZ as “axis predictivity”
(Gardner-Lubbe, le Roux and Gower, 2008; biplotEZ vignette; Greenacre,
2010).

The overall quality of the displayed PCA subspace is
\$\$R^2\_{\mathrm{disp},ab} =
\frac{\\\widehat{\mathbf{X}}\_{ab}\\\_F^2}{\\\mathbf{X}\\\_F^2} = 1 -
\frac{\\\mathbf{X} - \widehat{\mathbf{X}}\_{ab}\\\_F^2}
{\\\mathbf{X}\\\_F^2}.\$\$ Since \\\\\widehat{\mathbf{X}}\_{ab}\\\_F^2 =
d_a^2+d_b^2\\, this may also be written as \$\$R^2\_{\mathrm{disp},ab} =
\frac{d_a^2+d_b^2}{\sum\_{t=1}^{q} d_t^2}.\$\$ In particular, when
\\(a,b)=(1,2)\\, this is the familiar proportion of total sum of squares
explained by the first two principal components. More generally, it is
the quality of the specific displayed pair chosen by the user, matching
the biplotEZ quality measure (Gabriel, 1971; Greenacre, 2010; biplotEZ
vignette).

Because both Type A and Type B orthogonality hold, the overall quality
can be expressed as a weighted average on either the sample side or the
variable side. On the variable side, \$\$R^2\_{\mathrm{disp},ab} =
\sum\_{j=1}^{p} w_j\phi_j,\$\$ where \$\$w_j =
\frac{\\\mathbf{x}\_{(j)}\\^2}{\\\mathbf{X}\\\_F^2}, \qquad
\sum\_{j=1}^{p} w_j = 1.\$\$ Hence variables with larger sums of squares
contribute more to the overall display quality. If the original call to
[`biplot()`](https://rdrr.io/pkg/biplotEZ/man/biplot.html) used
`scale = TRUE`, so that all processed variables have equal sums of
squares, then \$\$R^2\_{\mathrm{disp},ab} =
\frac{1}{p}\sum\_{j=1}^{p}\phi_j.\$\$ Thus, for a standardized PCA
biplot, the overall quality is the simple average of the individual axis
predictivities. This is the weighted-average interpretation requested by
the present wrapper.

Similarly, on the sample side, \$\$R^2\_{\mathrm{disp},ab} =
\sum\_{i=1}^{n} m_i\psi_i,\$\$ where \$\$m_i =
\frac{\\\mathbf{x}\_{i\cdot}\\^2}{\\\mathbf{X}\\\_F^2}, \qquad
\sum\_{i=1}^{n} m_i = 1.\$\$ Hence the same overall display quality may
be read as a weighted average of sample predictivities or as a weighted
average of axis predictivities (Gardner-Lubbe, le Roux and Gower, 2008).

Unlike the regression-biplot case, no ordered orthogonalization is
required to decompose the quality of a PCA display into separate
contributions from the two displayed dimensions, because principal
components are already mutually orthogonal. Indeed,
\$\$\widehat{\mathbf{X}}\_{ab} = d_a\mathbf{u}\_a\mathbf{v}\_a^{\top} +
d_b\mathbf{u}\_b\mathbf{v}\_b^{\top},\$\$ and these two rank-1 parts are
orthogonal in Frobenius inner product. Hence \$\$R^2\_{\mathrm{disp},ab}
= R^2_a + R^2_b,\$\$ where \$\$R^2_a =
\frac{d_a^2}{\\\mathbf{X}\\\_F^2}, \qquad R^2_b =
\frac{d_b^2}{\\\mathbf{X}\\\_F^2}.\$\$ Thus the contribution of each
displayed principal component is obtained directly from its singular
value.

The same orthogonal decomposition yields a per-component breakdown of
each axis predictivity. Since \$\$\widehat{\mathbf{x}}\_{(j)} = d_a
v\_{ja}\mathbf{u}\_a + d_b v\_{jb}\mathbf{u}\_b,\$\$ with orthogonal
components \\\mathbf{u}\_a\\ and \\\mathbf{u}\_b\\, one has \$\$\phi_j =
\phi\_{ja} + \phi\_{jb},\$\$ where \$\$\phi\_{ja} = \frac{d_a^2
v\_{ja}^2}{\\\mathbf{x}\_{(j)}\\^2}, \qquad \phi\_{jb} = \frac{d_b^2
v\_{jb}^2}{\\\mathbf{x}\_{(j)}\\^2}.\$\$ Hence the predictivity of axis
\\j\\ can be decomposed exactly into the separate contributions of the
two displayed principal components. This is the PCA analogue of the
dimension-wise decomposition used elsewhere in the biplot literature,
but here it is especially simple because the components are orthogonal
from the outset. In particular, if the same variable is well aligned
with one selected principal direction but not the other, this will be
visible in the separate values \\\phi\_{ja}\\ and \\\phi\_{jb}\\.

Likewise, each sample predictivity decomposes as \$\$\psi_i =
\psi\_{ia} + \psi\_{ib},\$\$ where \$\$\psi\_{ia} = \frac{d_a^2
u\_{ia}^2}{\\\mathbf{x}\_{i\cdot}\\^2}, \qquad \psi\_{ib} = \frac{d_b^2
u\_{ib}^2}{\\\mathbf{x}\_{i\cdot}\\^2}.\$\$ Thus the contribution of
each displayed principal component may be read not only globally through
\\R^2_a\\ and \\R^2_b\\, but also locally through the sample-wise
contributions \\\psi\_{ia}\\ and \\\psi\_{ib}\\ and the axis-wise
contributions \\\phi\_{ja}\\ and \\\phi\_{jb}\\ (Gardner-Lubbe, le Roux
and Gower, 2008).

In addition to the sum-of-squares fit measures above, this method may
also report direct-reading diagnostics in the sense of Alves (2012).
These quantities serve a different purpose from the predictivities
\\\phi_j\\ and \\\psi_i\\. The predictivities measure how much of the
variation in \\\mathbf{X}\\ is reproduced by the selected PCA plane in a
sum-of-squares sense. By contrast, the Alves diagnostics measure how
accurately values can be read directly from the displayed calibrated
axes in the current two-dimensional map. This distinction is central in
the predictive biplot literature (Alves, 2012).

Let \\\mathbf{g}\_{(j)}\in\mathbb{R}^2\\ denote the displayed direction
of variable axis \\j\\ under the active PCA factorization. Thus
\\\mathbf{g}\_{(j)}=\mathbf{h}\_{(j)}\\ for the ordinary PCA biplot and
\\\mathbf{g}\_{(j)}=\mathbf{c}\_{(j)}\\ for the correlation biplot. Let
\\\mathbf{z}\_i\in\mathbb{R}^2\\ denote the corresponding displayed
sample coordinate of sample \\i\\. Then the value read from the graph on
axis \\j\\ for sample \\i\\ is \$\$\widehat{x}\_{ij} =
\mathbf{z}\_{i}^{\top}\mathbf{g}\_{(j)},\$\$ and the point on the
calibrated axis corresponding to that reading is \$\$\mathbf{p}\_{ij} =
\frac{\widehat{x}\_{ij}}
{\mathbf{g}\_{(j)}^{\top}\mathbf{g}\_{(j)}}\mathbf{g}\_{(j)}.\$\$ Thus
the direct reading from the displayed PCA axis coincides exactly with
the fitted value from the active two-dimensional PCA approximation.

Let \\s_j\\ denote the standard deviation used to standardize variable
\\j\\. When the processed matrix \\\mathbf{X}\\ is already standardized,
\\s_j=1\\. The pointwise direct-reading error for sample \\i\\ on axis
\\j\\ is then \$\$\delta\_{ij} =
\frac{\|x\_{ij}-\widehat{x}\_{ij}\|}{s_j}.\$\$ If \\\mathbf{X}\\ is
already standardized, then
\\\delta\_{ij}=\|x\_{ij}-\widehat{x}\_{ij}\|\\. The corresponding
axis-level mean direct-reading error is \$\$\bar{\delta}\_j =
\frac{1}{n}\sum\_{i=1}^{n}\delta\_{ij} = \frac{1}{n}\sum\_{i=1}^{n}
\frac{\|x\_{ij}-\widehat{x}\_{ij}\|}{s_j}.\$\$ This is the
two-dimensional PCA-biplot analogue of the mean standard predictive
error of Alves (2012).

Let \\\tau\_{\mathrm{axis}} \> 0\\ be a user-specified *tolerance
parameter* for axis-level direct-reading error. Then the Alves selection
rule becomes \$\$\text{retain axis }j \quad\Longleftrightarrow\quad
\bar{\delta}\_j \le \tau\_{\mathrm{axis}}.\$\$ Likewise, for an
observation-level tolerance parameter \\\tau\_{\mathrm{units}} \> 0\\,
sample \\i\\ is flagged with respect to axis \\j\\ whenever
\$\$\delta\_{ij} \> \tau\_{\mathrm{units}}.\$\$ Hence
\\\bar{\delta}\_j\\ may be used for axis selection and \\\delta\_{ij}\\
for observation-level checking, exactly as in the predictive PCA-biplot
framework of Alves (2012).

The Alves diagnostics and the PCA predictivities are complementary. The
quantity \\\phi_j\\ answers the question: “How much of variable \\j\\'s
sum of squares is reproduced by the selected PCA plane?” The quantity
\\\psi_i\\ answers the corresponding sample-side question: “How much of
sample \\i\\'s sum of squares is reproduced by the selected PCA plane?”
By contrast, \\\bar{\delta}\_j\\ answers the distinct question: “How
accurately can values of variable \\j\\ be read directly from the
displayed calibrated axis?” Consequently, an axis may have high
\\\phi_j\\ yet still have non-negligible direct-reading error in the
current display, while an axis with only moderate \\\phi_j\\ may
nevertheless admit acceptable direct readings. In this implementation,
the \\\phi_j\\- and \\\psi_i\\-families are the primary sum-of-squares
fit measures, whereas the Alves quantities \\\bar{\delta}\_j\\ and
\\\delta\_{ij}\\ provide supplementary, display-specific diagnostics.

In the wrapped `bipl5_biplot` object, these formulas drive the
hover-time fitted values \\\widehat{\mathbf{X}}\_{ab}\\, the calibrated
tick markers for each active PCA mdsDisplay, the bottom display-quality
label, and the axis/sample fit summaries attached to the active
two-dimensional principal-component view. If several PC pairs are stored
as separate mdsDisplays, the same construction applies to each
mdsDisplay separately.

## References

Eckart, C. and Young, G. (1936). The approximation of one matrix by
another of lower rank. *Psychometrika*, 1, 211–218.

Gabriel, K. R. (1971). The biplot graphical display of matrices with
application to principal component analysis. *Biometrika*, 58(3),
453–467.
[doi:10.1093/biomet/58.3.453](https://doi.org/10.1093/biomet/58.3.453)

Gower, J. C. and Hand, D. J. (1996). *Biplots*. London: Chapman \\ Hall.

Gower, J. C., Lubbe, S. and le Roux, N. J. (2011). *Understanding
Biplots*. Chichester: Wiley.

Greenacre, M. (2010). *Biplots in Practice*. Bilbao: BBVA Foundation.

Gardner-Lubbe, S., le Roux, N. J. and Gower, J. C. (2008). Measures of
fit in principal component and canonical variate analyses. *Journal of
Applied Statistics*, 35(9), 947–965.
[doi:10.1080/02664760802185399](https://doi.org/10.1080/02664760802185399)

Lubbe, S., le Roux, N. J., Nienkemper-Swanepoel, J., Ganey, R., Buys,
R., Adams, Z.-M. and Manefeldt, P. (2025). *biplotEZ: EZ-to-Use
Biplots*. R package version 2.2.

Alves, M. R. (2012). Evaluation of the predictive power of biplot axes
to automate the construction and layout of biplots based on the accuracy
of direct readings from common outputs of multivariate analyses:

1.  application to principal component analysis. *Journal of
    Chemometrics*, 26(5), 180–190.
    [doi:10.1002/cem.2433](https://doi.org/10.1002/cem.2433)

## Examples

``` r
if (FALSE) { # \dontrun{
library(biplotEZ)
bp <- biplot(iris[, 1:4], scale = TRUE) |>
  PCA(e.vects = c(1, 2), group.aes = iris[, 5]) |>
  wrap_bipl5()
bp
plot(bp)

bp_cor <- biplot(iris[, 1:4], scale = TRUE) |>
  PCA(
    e.vects = c(1, 2),
    group.aes = iris[, 5],
    correlation.biplot = TRUE
  ) |>
  wrap_bipl5()
plot(bp_cor)
} # }
```
