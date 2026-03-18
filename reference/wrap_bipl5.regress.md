# Construct a bipl5_biplot from a regression biplot

Builds the single mdsDisplay used for a linear regression biplot and
documents the associated regression-biplot fit and predictivity
measures. Regression biplots do not use the multi-mdsDisplay fit
machinery available for PCA/CVA displays: they have one fixed mdsDisplay
(`mdsDisplay_12`),
[`append_mdsDisplay()`](https://www.bipl5.co.za/reference/append_mdsDisplay.md)
and
[`remove_mdsDisplay()`](https://www.bipl5.co.za/reference/remove_mdsDisplay.md)
are not supported, and the only active toggle button is “Translated
Axes”.

## Usage

``` r
# S3 method for class 'regress'
wrap_bipl5(x)
```

## Arguments

- x:

  An object of class `biplot` from the biplotEZ package with
  [`regress()`](https://rdrr.io/pkg/biplotEZ/man/regress.html) method
  applied.

## Value

An object of class `c("bipl5_biplot", "reg")`

## Details

For the linear regression biplot handled by this method, let
\\\mathbf{X}\in\mathbb{R}^{n\times p}\\ denote the *processed* data
matrix stored in the `biplot` object after centring and any optional
scaling performed by
[`biplot()`](https://rdrr.io/pkg/biplotEZ/man/biplot.html), and let
\\\mathbf{Z}\in\mathbb{R}^{n\times 2}\\ denote the externally supplied
display coordinates of the \\n\\ samples. Write \\\mathbf{Z} =
\[\mathbf{z}\_1\\ \mathbf{z}\_2\]\\, where \\\mathbf{z}\_1\\ and
\\\mathbf{z}\_2\\ are the first and second displayed coordinates
respectively. In contrast to a PCA biplot, the sample map is taken as
given and the variable axes are then fitted to that map by multivariate
least squares. This is the regression-biplot point of view used in the
biplot literature for general low-dimensional sample maps (Gower and
Hand, 1996; Gower, Lubbe and le Roux, 2011).

The fitted linear model is \$\$\mathbf{X} =
\mathbf{Z}\mathbf{H}^{\top} + \mathbf{E},\$\$ where, when \\\mathbf{Z}\\
has full column rank, \$\$\mathbf{H}^{\top} =
(\mathbf{Z}^{\top}\mathbf{Z})^{-1}\mathbf{Z}^{\top}\mathbf{X}.\$\$ Hence
the fitted values are \$\$\widehat{\mathbf{X}} =
\mathbf{Z}\mathbf{H}^{\top} =
\mathbf{Z}(\mathbf{Z}^{\top}\mathbf{Z})^{-1}\mathbf{Z}^{\top}\mathbf{X}
= \mathbf{P}\_Z\mathbf{X},\$\$ where \\\mathbf{P}\_Z\\ is the orthogonal
projector onto the column space of \\\mathbf{Z}\\. More generally, if
the supplied display coordinates are rank-deficient, the same fitted
matrix \\\widehat{\mathbf{X}}\\ is obtained by interpreting
\\\mathbf{P}\_Z\\ as the orthogonal projector onto
\\\mathrm{col}(\mathbf{Z})\\. The regression biplot therefore displays
the variables through the least-squares predictions obtained from the
supplied 2D sample map (Gower and Hand, 1996; Gower, Lubbe and le Roux,
2011).

If \\\mathbf{h}\_{(j)}\\ denotes the \\j\\th column of \\\mathbf{H}\\,
then the predicted value of variable \\j\\ for sample \\i\\ is
\$\$\widehat{x}\_{ij} = \mathbf{z}\_i^{\top}\mathbf{h}\_{(j)}.\$\$ The
calibrated axis for variable \\j\\ has direction \\\mathbf{h}\_{(j)}\\,
and the point on that axis corresponding to marker value \\\mu\\ is
\$\$\mathbf{p}\_{\mu j} =
\frac{\mu}{\mathbf{h}\_{(j)}^{\top}\mathbf{h}\_{(j)}}\mathbf{h}\_{(j)}.\$\$
This is the calibration formula used to place tick marks and to recover
predicted values from projections onto the displayed axis, in direct
analogy with calibrated-axis biplot constructions (Gabriel, 1971; Gower,
Lubbe and le Roux, 2011). All such predicted values are on the same
centred/scaled scale as the stored matrix \\\mathbf{X}\\; if needed,
they can be back-transformed to the original variable scale using the
means and standard deviations stored in the input `biplot` object.

A regression biplot admits a natural family of *predictivity* measures
on the variable side. Let \\\mathbf{x}\_{(j)}\\ denote column \\j\\ of
\\\mathbf{X}\\, let \\\widehat{\mathbf{x}}\_{(j)}\\ denote column \\j\\
of \\\widehat{\mathbf{X}}\\, and let \\\mathbf{e}\_{(j)} =
\mathbf{x}\_{(j)} - \widehat{\mathbf{x}}\_{(j)}\\. Since
\\\widehat{\mathbf{X}} = \mathbf{P}\_Z\mathbf{X}\\ is an orthogonal
projection, the residual matrix satisfies
\$\$\widehat{\mathbf{X}}^{\top}\mathbf{E} = \mathbf{0},\$\$ and
therefore \$\$\mathbf{X}^{\top}\mathbf{X} =
\widehat{\mathbf{X}}^{\top}\widehat{\mathbf{X}} + (\mathbf{X} -
\widehat{\mathbf{X}})^{\top} (\mathbf{X} - \widehat{\mathbf{X}}).\$\$
This is the variable-side, or Type B, orthogonality that justifies
variance-accounted-for ratios for the columns of \\\mathbf{X}\\; it is
the same side of the orthogonality argument that underlies column-wise
predictivities in the biplot literature (Gower, Lubbe and le Roux, 2011;
Greenacre, 2010).

The predictivity of variable \\j\\ is therefore defined by \$\$\phi_j =
\frac{\\\widehat{\mathbf{x}}\_{(j)}\\^2} {\\\mathbf{x}\_{(j)}\\^2} = 1 -
\frac{\\\mathbf{x}\_{(j)} - \widehat{\mathbf{x}}\_{(j)}\\^2}
{\\\mathbf{x}\_{(j)}\\^2}, \qquad j=1,\ldots,p.\$\$ Thus \\\phi_j\\ is
the proportion of the sum of squares of variable \\j\\ reproduced by the
regression biplot, equivalently the ordinary multiple-regression \\R^2\\
obtained by regressing variable \\j\\ on the displayed coordinates
\\\mathbf{Z}\\. Each \\\phi_j\\ lies in \\\[0,1\]\\; values near one
indicate that the variable is well predicted by the displayed map, while
values near zero indicate that the variable is poorly reproduced by the
chosen display (Greenacre, 2010).

A natural overall quality-of-display measure is the proportion of total
sum of squares reproduced by the display, \$\$R^2\_{\mathrm{disp}} =
\frac{\\\widehat{\mathbf{X}}\\\_F^2}{\\\mathbf{X}\\\_F^2} = 1 -
\frac{\\\mathbf{X} -
\widehat{\mathbf{X}}\\\_F^2}{\\\mathbf{X}\\\_F^2}.\$\$ Because the
column-wise decomposition above is orthogonal, this overall quality can
be written as a weighted average of the variable predictivities:
\$\$R^2\_{\mathrm{disp}} = \sum\_{j=1}^{p} w_j \phi_j,\$\$ where \$\$w_j
= \frac{\\\mathbf{x}\_{(j)}\\^2}{\\\mathbf{X}\\\_F^2}, \qquad
\sum\_{j=1}^{p} w_j = 1.\$\$ Hence variables with larger sums of squares
contribute more to the overall quality. In particular, if the original
call to [`biplot()`](https://rdrr.io/pkg/biplotEZ/man/biplot.html) used
`scale = TRUE`, so that all processed variables have equal sums of
squares, then the weights are equal and \$\$R^2\_{\mathrm{disp}} =
\frac{1}{p}\sum\_{j=1}^{p}\phi_j.\$\$ This weighted-average
interpretation is often the most natural way to read the overall
regression-biplot quality, since it combines the separate variable
predictivities into a single display-wide summary (Greenacre, 2010).

The quantities \\\phi_j\\ and \\R^2\_{\mathrm{disp}}\\ depend only on
the fitted projection \\\mathbf{P}\_Z\mathbf{X}\\ and therefore only on
the subspace \\\mathrm{col}(\mathbf{Z})\\. They do *not* depend on any
particular basis chosen for that subspace. In particular, the variable
predictivities \\\phi_j\\ do not require any QR decomposition.

To decompose the total display quality into separate contributions for
the two displayed dimensions, this package applies an *ordered
orthogonalization* of the supplied display coordinates. Specifically,
define \$\$\mathbf{u}\_1 = \mathbf{z}\_1, \qquad \mathbf{q}\_1 =
\frac{\mathbf{u}\_1}{\\\mathbf{u}\_1\\}\$\$ whenever \\\mathbf{u}\_1
\neq \mathbf{0}\\, and then define \$\$\mathbf{u}\_2 = \mathbf{z}\_2 -
\mathbf{q}\_1\mathbf{q}\_1^{\top}\mathbf{z}\_2, \qquad \mathbf{q}\_2 =
\frac{\mathbf{u}\_2}{\\\mathbf{u}\_2\\}\$\$ whenever \\\mathbf{u}\_2
\neq \mathbf{0}\\. Equivalently, \\\mathbf{Q} = \[\mathbf{q}\_1\\
\mathbf{q}\_2\]\\ is obtained from the QR decomposition of
\\\mathbf{Z}\\, preserving the supplied column order. The vectors
\\\mathbf{q}\_1\\ and \\\mathbf{q}\_2\\ are orthonormal and span the
same display subspace as the nonzero columns of \\\mathbf{Z}\\.

Because \\\mathbf{Q}\\ and \\\mathbf{Z}\\ span the same subspace, the
orthogonal projector may also be written as \$\$\mathbf{P}\_Z =
\mathbf{Q}\mathbf{Q}^{\top}.\$\$ Consequently, \$\$\widehat{\mathbf{X}}
= \mathbf{Q}\mathbf{Q}^{\top}\mathbf{X} =
\mathbf{q}\_1\mathbf{q}\_1^{\top}\mathbf{X} +
\mathbf{q}\_2\mathbf{q}\_2^{\top}\mathbf{X}\$\$ whenever both
orthogonalized directions are present. Since
\\\mathbf{q}\_1^{\top}\mathbf{q}\_2 = 0\\, the two fitted parts are
orthogonal and their sums of squares add. This yields the
dimension-specific contributions \$\$R^2_1 =
\frac{\\\mathbf{q}\_1\mathbf{q}\_1^{\top}\mathbf{X}\\\_F^2}
{\\\mathbf{X}\\\_F^2},\$\$ and \$\$R^2\_{2\mid 1} =
\frac{\\\mathbf{q}\_2\mathbf{q}\_2^{\top}\mathbf{X}\\\_F^2}
{\\\mathbf{X}\\\_F^2},\$\$ so that \$\$R^2\_{\mathrm{disp}} = R^2_1 +
R^2\_{2\mid 1}\$\$ whenever the display space is two-dimensional.

Care should be taken when interpreting this decomposition. If the
columns of \\\mathbf{Z}\\ are already orthogonal, then the two displayed
contributions correspond directly to the first and second supplied
display axes. If the columns of \\\mathbf{Z}\\ are not orthogonal,
however, the decomposition is *ordered*. The first contribution
\\R^2_1\\ is attributable to the first supplied display coordinate
\\\mathbf{z}\_1\\. The second contribution \\R^2\_{2\mid 1}\\ is
attributable to the component of the second supplied display coordinate
\\\mathbf{z}\_2\\ that is orthogonal to the first. Thus \\R^2\_{2\mid
1}\\ should be interpreted as the additional contribution of “Dim 2
given Dim 1”, not as the contribution of the raw second column of
\\\mathbf{Z}\\ considered in isolation. The ordering of the columns of
\\\mathbf{Z}\\ is therefore important for this decomposition.

The same ordered orthogonalization yields a decomposition of each
variable's predictivity: \$\$\phi_j = \phi\_{j1} + \phi\_{j,2\mid
1},\$\$ where \$\$\phi\_{j1} =
\frac{\\\mathbf{q}\_1\mathbf{q}\_1^{\top}\mathbf{x}\_{(j)}\\^2}
{\\\mathbf{x}\_{(j)}\\^2} =
\frac{(\mathbf{q}\_1^{\top}\mathbf{x}\_{(j)})^2}
{\\\mathbf{x}\_{(j)}\\^2},\$\$ and \$\$\phi\_{j,2\mid 1} =
\frac{\\\mathbf{q}\_2\mathbf{q}\_2^{\top}\mathbf{x}\_{(j)}\\^2}
{\\\mathbf{x}\_{(j)}\\^2} =
\frac{(\mathbf{q}\_2^{\top}\mathbf{x}\_{(j)})^2}
{\\\mathbf{x}\_{(j)}\\^2}.\$\$ Thus \\\phi\_{j1}\\ is the part of
variable \\j\\'s predictivity explained by the first supplied display
dimension, while \\\phi\_{j,2\mid 1}\\ is the additional part explained
by the second display dimension after removing its overlap with the
first.

If the supplied display coordinates are collinear, then \\\mathbf{u}\_2
= \mathbf{0}\\ and the effective display space is one-dimensional. In
that case \\R^2\_{2\mid 1} = 0\\ and \\\phi\_{j,2\mid 1} = 0\\ for all
variables.

In addition to the sum-of-squares fit measures above, this method may
also report direct-reading error diagnostics in the sense of Alves
(2012). The purpose of these diagnostics is different from that of the
predictivities \\\phi_j\\. The quantities \\\phi_j\\, \\\phi\_{j1}\\,
\\\phi\_{j,2\mid 1}\\ and \\R^2\_{\mathrm{disp}}\\ measure how much of
the variation in \\\mathbf{X}\\ is reproduced by the fitted regression
biplot. By contrast, the Alves diagnostics measure how accurately values
can be read directly from a displayed calibrated axis in the current
two-dimensional map. Alves (2012) proposed this idea for predictive PCA
biplots; in the present two-dimensional regression-biplot setting the
same principle applies in a particularly simple form because there is
only one displayed map.

For each sample \\i=1,\ldots,n\\ and variable axis \\j=1,\ldots,p\\, the
reading taken from the displayed axis of variable \\j\\ is precisely the
fitted value \$\$\widehat{x}\_{ij} =
\mathbf{z}\_i^{\top}\mathbf{h}\_{(j)}.\$\$ The corresponding point on
the calibrated axis is \$\$\mathbf{p}\_{ij} = \frac{\widehat{x}\_{ij}}
{\mathbf{h}\_{(j)}^{\top}\mathbf{h}\_{(j)}}\mathbf{h}\_{(j)},\$\$
obtained by substituting \\\mu = \widehat{x}\_{ij}\\ into the
calibration formula. Thus the direct reading from the graph and the
fitted value from the regression model coincide.

Let \\s_j\\ denote the standard deviation used to standardize variable
\\j\\. When `scale = TRUE`, the processed matrix \\\mathbf{X}\\ already
has unit-variance columns and hence \\s_j = 1\\. The pointwise
direct-reading error for sample \\i\\ on variable axis \\j\\ is defined
by \$\$\delta\_{ij} = \frac{\|x\_{ij} - \widehat{x}\_{ij}\|}{s_j}.\$\$
If the processed matrix is already standardized, then \\\delta\_{ij} =
\|x\_{ij} - \widehat{x}\_{ij}\|\\; in that case \\\delta\_{ij}\\ is the
direct analogue of Alves's standard predictive error. More generally,
dividing by \\s_j\\ expresses the discrepancy on a comparable
variable-wise scale. The quantity \\\delta\_{ij}\\ is therefore a
sample-by-axis direct-reading error.

The corresponding axis-level mean direct-reading error is
\$\$\bar{\delta}\_j = \frac{1}{n}\sum\_{i=1}^{n}\delta\_{ij} =
\frac{1}{n}\sum\_{i=1}^{n} \frac{\|x\_{ij} -
\widehat{x}\_{ij}\|}{s_j}.\$\$ This is the two-dimensional
regression-biplot analogue of the mean standard predictive error of
Alves (2012). Small values of \\\bar{\delta}\_j\\ indicate that the
calibrated axis for variable \\j\\ supports accurate direct readings on
average across the displayed observations, whereas large values indicate
that direct readings from that axis are unreliable in the current
display.

Let \\\tau\_{\mathrm{axis}} \> 0\\ be a user-specified *tolerance
parameter* for axis-level direct-reading error. Then the Alves selection
rule specialized to the present two-dimensional regression biplot is
\$\$\text{retain axis }j \quad\Longleftrightarrow\quad \bar{\delta}\_j
\le \tau\_{\mathrm{axis}}.\$\$ Thus an axis is shown only when its
average direct-reading error is at most the allowed tolerance. Larger
values of \\\tau\_{\mathrm{axis}}\\ retain more axes and therefore
produce denser displays; smaller values enforce stricter axis selection
and lead to sparser, more conservative displays. In Alves (2012), values
around 0.5 are discussed as a practical starting point in conventional
settings, but no universal default should be assumed.

In addition to axis selection, Alves (2012) proposed a second *tolerance
parameter* for individual sample-axis discrepancies. Let
\\\tau\_{\mathrm{units}} \> 0\\. A sample \\i\\ is then flagged as an
outlier with respect to axis \\j\\ whenever \$\$\delta\_{ij} \>
\tau\_{\mathrm{units}}.\$\$ Such a flag indicates that, even if axis
\\j\\ is acceptable on average, the direct reading for sample \\i\\ from
that axis is poor in the current display. Alves (2012) discusses values
around 0.75 as a practical starting point for this tolerance parameter,
again subject to the application and the scale of the analysis.

Because this wrapper is tied to a single two-dimensional
regression-biplot display, the quantities \\\delta\_{ij}\\ and
\\\bar{\delta}\_j\\ are display-specific diagnostics. They are not
measures of the quality of the underlying fitted subspace in the
sum-of-squares sense; rather, they quantify the numerical accuracy of
direct readings from the currently displayed axes. This distinction is
central in Alves (2012), who emphasizes that direct-reading error is
conceptually different from earlier axis-predictivity measures.

The Alves diagnostics and the regression-biplot predictivities are
therefore complementary. The quantity \\\phi_j\\ is a
variance-accounted-for ratio justified by Type B orthogonality and
answers the question: “How much of variable \\j\\'s sum of squares is
reproduced by the displayed regression biplot?” The quantity
\\\bar{\delta}\_j\\ is a mean absolute direct-reading error and answers
the different question: “How accurately can values of variable \\j\\ be
read from the displayed calibrated axis?” Consequently, a variable may
have high \\\phi_j\\ and still have a non-negligible direct-reading
error, while a variable with moderate \\\phi_j\\ may nevertheless
support acceptable average direct readings. In this implementation, the
\\\phi_j\\-family is the primary set of sum-of-squares fit measures,
whereas the Alves quantities \\\bar{\delta}\_j\\ and \\\delta\_{ij}\\
provide supplementary, display-specific diagnostics for axis selection
and observation-level checking.

In contrast, a regression biplot does *not* in general satisfy the
sample-side decomposition \$\$\mathbf{X}\mathbf{X}^{\top} =
\widehat{\mathbf{X}}\widehat{\mathbf{X}}^{\top} + (\mathbf{X} -
\widehat{\mathbf{X}}) (\mathbf{X} - \widehat{\mathbf{X}})^{\top}.\$\$
Consequently, PCA-style sample predictivities are not generally
justified for a regression biplot. The principled sum-of-squares fit
measures are the variable predictivities \\\phi_j\\, the overall quality
\\R^2\_{\mathrm{disp}}\\, and the ordered dimension-specific
contributions described above, with the Alves direct-reading errors
providing a distinct supplementary perspective on the quality of the
displayed axes.

In the wrapped `bipl5_biplot` object, these formulas drive the bottom
display-quality label, the hover-time predicted values
\\\widehat{\mathbf{X}}\\, and the calibrated linear axes stored in
`mdsDisplay_12`. Since the regression display is tied to one externally
supplied map, `wrap_bipl5.regress()` produces a single mdsDisplay only.
There is no PC/CV toggle and no separate PCA-style sample-fit panel.

## References

Gabriel, K. R. (1971). The biplot graphical display of matrices with
application to principal component analysis. *Biometrika*, 58(3),
453–467.
[doi:10.1093/biomet/58.3.453](https://doi.org/10.1093/biomet/58.3.453)

Gower, J. C. and Hand, D. J. (1996). *Biplots*. London: Chapman \\ Hall.

Gower, J. C., Lubbe, S. and le Roux, N. J. (2011). *Understanding
Biplots*. Chichester: Wiley.

Greenacre, M. (2010). *Biplots in Practice*. Bilbao: BBVA Foundation.

la Grange, A., le Roux, N. and Gardner-Lubbe, S. (2009). BiplotGUI:
Interactive Biplots in R. *Journal of Statistical Software*, 30(12),
1–37. [doi:10.18637/jss.v030.i12](https://doi.org/10.18637/jss.v030.i12)

Alves, M. R. (2012). Evaluation of the predictive power of biplot axes
to automate the construction and layout of biplots based on the accuracy
of direct readings from common outputs of multivariate analyses:
application to principal component analysis. *Journal of Chemometrics*,
26(5), 180–190. [doi:10.1002/cem.2433](https://doi.org/10.1002/cem.2433)

## Examples

``` r
if (FALSE) { # \dontrun{
library(biplotEZ)
bp <- biplot(iris[, 1:4]) |>
  regress(Z = prcomp(iris[, 1:4])$x[, 1:2], group.aes = iris[, 5]) |>
  wrap_bipl5()
bp
plot(bp)
} # }
```
