# Regression Biplots

``` r
suppressPackageStartupMessages(library(bipl5))
```

This vignette covers regression biplots constructed with the newer
`bipl5` workflow:

``` r
init_biplot(...) |> scale_mds(type = "regress", Z = ...)
```

The mathematics mirror the current
[`wrap_bipl5.regress()`](https://www.bipl5.co.za/reference/wrap_bipl5.regress.md)
documentation, but the examples use
[`init_biplot()`](https://www.bipl5.co.za/reference/init_biplot.md) and
[`scale_mds()`](https://www.bipl5.co.za/reference/scale_mds.md) because
this is now the most direct way to build a regression biplot inside the
package.

Regression biplots differ from PCA biplots in one crucial way: the
sample map is supplied from outside. The display coordinates in `Z` are
treated as fixed, and the variable axes are then fitted to that map by
least squares.

## Building a regression biplot

As in the PCA vignette, we keep an extra categorical column in the
original data so we can later demonstrate
[`format_samples()`](https://www.bipl5.co.za/reference/format_samples.md)
with a second sample stratification.

``` r
iris2 <- iris
iris2$Band <- factor(
  rep(c("class1", "class2", "class3", "class4"), length.out = nrow(iris2))
)

Z <- prcomp(iris2[, 1:4], scale. = TRUE)$x[, 1:2]

reg_spec <- init_biplot(iris2, scale = TRUE)

bp_reg <- reg_spec |>
  scale_mds(
    type = "regress",
    Z = Z,
    group_aes = iris2$Species,
    show_group_means = TRUE
  )
```

After `scale_mds(type = "regress", ...)`, the object contains one fixed
display only:

``` r
bp_reg
#> bipl5_biplot [REG] 
#> └── mdsDisplay_12 [Dim 1 & 2] <bipl5_mdsDisplay>
#>     ├── Data <bipl5_data>
#>     │   ├── sample_coordinates  [150 x 2]
#>     │   ├── axes_coordinates  [4 axes]
#>     │   └── translated_axes_coordinates
#>     ├── trace_data  [34 traces]
#>     └── annotations  [124 items]
```

That single display is always stored as `mdsDisplay_12` and labelled
`Dim 1 & 2`, because the geometry comes from the supplied two-column map
instead of an internally generated sequence of principal components.

``` r
plot(bp_reg)
```

## Formatting samples

[`format_samples()`](https://www.bipl5.co.za/reference/format_samples.md)
works for regression biplots in the same way that it works for PCA
biplots: the fitted regression geometry is left untouched, and only the
sample-trace block is rebuilt.

First we colour the samples by species:

``` r
bp_reg_species <- bp_reg |>
  format_samples(
    stratify = "col",
    by = Species,
    col = c("tomato", "steelblue", "darkgreen")
  )
```

Then we add a second categorical stratification by plotting symbol:

``` r
bp_reg_dual <- bp_reg_species |>
  format_samples(
    stratify = "symbol",
    by = Band,
    pch = c(15, 16, 17, 18)
  )
```

The resulting widget now has separate sample legend sections for
`Species` and `Band`, while the hidden observation traces are the
observed `Species x Band` combinations.

``` r
plot(bp_reg_dual)
```

## How regression biplots differ from PCA biplots

Regression biplots are intentionally simpler in the current package:

- they have one fixed `mdsDisplay` only
- [`append_mdsDisplay()`](https://www.bipl5.co.za/reference/append_mdsDisplay.md)
  and
  [`remove_mdsDisplay()`](https://www.bipl5.co.za/reference/remove_mdsDisplay.md)
  are not supported
- they do not carry the PCA `fit_measures` branch
- there is no PCA-style right-hand fit panel
- the fitted variable axes are tied to the supplied map `Z`

The main quality label in the widget still reports the overall
regression-biplot quality and its ordered dimension-specific
contributions, but the multi-display PCA fit machinery is absent by
design.

## Mathematical background

This section preserves the long-form regression-biplot documentation in
a vignette-style narrative.

Let $\mathbf{X} \in {\mathbb{R}}^{n \times p}$ denote the processed data
matrix after centring and optional scaling, and let
$\mathbf{Z} \in {\mathbb{R}}^{n \times 2}$ denote the externally
supplied display coordinates. In a regression biplot the sample map is
taken as given, and the variable axes are fitted to that map by least
squares.

The fitted model is

$$\mathbf{X} = \mathbf{Z}\mathbf{H}^{\top} + \mathbf{E},$$

with

$$\mathbf{H}^{\top} = \left( \mathbf{Z}^{\top}\mathbf{Z} \right)^{- 1}\mathbf{Z}^{\top}\mathbf{X}$$

when $\mathbf{Z}$ has full column rank. Equivalently,

$$\widehat{\mathbf{X}} = \mathbf{Z}\mathbf{H}^{\top} = \mathbf{Z}\left( \mathbf{Z}^{\top}\mathbf{Z} \right)^{- 1}\mathbf{Z}^{\top}\mathbf{X} = \mathbf{P}_{Z}\mathbf{X},$$

where $\mathbf{P}_{Z}$ is the orthogonal projector onto the column space
of $\mathbf{Z}$.

If $\mathbf{h}_{(j)}$ is the fitted direction for variable $j$, then the
predicted value for sample $i$ on that variable is

$${\widehat{x}}_{ij} = \mathbf{z}_{i}^{\top}\mathbf{h}_{(j)}.$$

The point on the calibrated axis corresponding to marker value $\mu$ is

$$\mathbf{p}_{\mu j} = \frac{\mu}{\mathbf{h}_{(j)}^{\top}\mathbf{h}_{(j)}}\mathbf{h}_{(j)}.$$

So, just as in the PCA case, direct readings from the axis coincide with
the fitted values from the display.

Because $\widehat{\mathbf{X}} = \mathbf{P}_{Z}\mathbf{X}$ is an
orthogonal projection, the regression biplot admits a natural
variable-side predictivity measure. For variable $j$,

$$\phi_{j} = \frac{\parallel {\widehat{\mathbf{x}}}_{(j)} \parallel^{2}}{\parallel \mathbf{x}_{(j)} \parallel^{2}} = 1 - \frac{\parallel \mathbf{x}_{(j)} - {\widehat{\mathbf{x}}}_{(j)} \parallel^{2}}{\parallel \mathbf{x}_{(j)} \parallel^{2}}.$$

This is the proportion of variable $j$’s sum of squares reproduced by
the display. It is also the ordinary multiple-regression $R^{2}$
obtained by regressing that variable on the supplied coordinates in
$\mathbf{Z}$.

The overall regression-biplot quality is

$$R_{disp}^{2} = \frac{\parallel \widehat{\mathbf{X}} \parallel_{F}^{2}}{\parallel \mathbf{X} \parallel_{F}^{2}} = 1 - \frac{\parallel \mathbf{X} - \widehat{\mathbf{X}} \parallel_{F}^{2}}{\parallel \mathbf{X} \parallel_{F}^{2}}.$$

Because the column-wise decomposition is orthogonal, the same quantity
can be written as a weighted average of the variable predictivities:

$$R_{disp}^{2} = \sum\limits_{j = 1}^{p}w_{j}\phi_{j},\qquad w_{j} = \frac{\parallel \mathbf{x}_{(j)} \parallel^{2}}{\parallel \mathbf{X} \parallel_{F}^{2}}.$$

If the processed variables all have equal sums of squares, the overall
quality is simply the average of the variable predictivities.

To separate the quality into contributions from the two displayed
dimensions, the package uses an ordered orthogonalization of the
supplied coordinates. Let

$$\mathbf{u}_{1} = \mathbf{z}_{1},\qquad\mathbf{q}_{1} = \frac{\mathbf{u}_{1}}{\parallel \mathbf{u}_{1} \parallel},$$

and then let

$$\mathbf{u}_{2} = \mathbf{z}_{2} - \mathbf{q}_{1}\mathbf{q}_{1}^{\top}\mathbf{z}_{2},\qquad\mathbf{q}_{2} = \frac{\mathbf{u}_{2}}{\parallel \mathbf{u}_{2} \parallel}.$$

Equivalently, this is the QR orthogonalization of $\mathbf{Z}$ while
preserving the supplied column order. The display projector can then be
written as

$$\mathbf{P}_{Z} = \mathbf{Q}\mathbf{Q}^{\top},\qquad\mathbf{Q} = \left\lbrack \mathbf{q}_{1}\ \mathbf{q}_{2} \right\rbrack.$$

Hence

$$\widehat{\mathbf{X}} = \mathbf{q}_{1}\mathbf{q}_{1}^{\top}\mathbf{X} + \mathbf{q}_{2}\mathbf{q}_{2}^{\top}\mathbf{X},$$

and the two pieces are orthogonal. This yields the ordered contributions

$$R_{1}^{2} = \frac{\parallel \mathbf{q}_{1}\mathbf{q}_{1}^{\top}\mathbf{X} \parallel_{F}^{2}}{\parallel \mathbf{X} \parallel_{F}^{2}},\qquad R_{2 \mid 1}^{2} = \frac{\parallel \mathbf{q}_{2}\mathbf{q}_{2}^{\top}\mathbf{X} \parallel_{F}^{2}}{\parallel \mathbf{X} \parallel_{F}^{2}},$$

with

$$R_{disp}^{2} = R_{1}^{2} + R_{2 \mid 1}^{2}.$$

This decomposition is ordered. If the original columns of $\mathbf{Z}$
are not orthogonal, then $R_{2 \mid 1}^{2}$ is the additional
contribution of the second supplied dimension after removing overlap
with the first. It should not be interpreted as the contribution of the
raw second column in isolation.

The same idea gives an ordered decomposition of each variable
predictivity:

$$\phi_{j} = \phi_{j1} + \phi_{j,2 \mid 1}.$$

This is a useful way to understand which part of the supplied map is
doing the predictive work for each variable.

The documentation also preserves the distinction between predictivity
and direct-reading diagnostics. If $s_{j}$ is the standard deviation
used in preprocessing, the pointwise direct-reading error is

$$\delta_{ij} = \frac{\left| x_{ij} - {\widehat{x}}_{ij} \right|}{s_{j}},$$

and the mean direct-reading error for axis $j$ is

$${\bar{\delta}}_{j} = \frac{1}{n}\sum\limits_{i = 1}^{n}\delta_{ij}.$$

These are display-specific diagnostics. They measure how accurately a
user can read values directly from the currently drawn calibrated axis.
They are not the same thing as the variance-accounted-for quantities
$\phi_{j}$ and $R_{disp}^{2}$.

One final distinction from PCA is especially important: a regression
biplot does not, in general, satisfy the sample-side orthogonality that
would justify PCA-style sample predictivities. So the principled
sum-of-squares fit measures here are the variable predictivities, the
overall display quality, and the ordered dimension-specific
contributions derived from the supplied map.

## References

Alves, M. R. (2012). Evaluation of the predictive power of biplot axes
to automate the construction and layout of biplots based on the accuracy
of direct readings from common outputs of multivariate analyses:
application to principal component analysis. *Journal of Chemometrics*,
26(5), 180-190.

Gabriel, K. R. (1971). The biplot graphical display of matrices with
application to principal component analysis. *Biometrika*, 58(3),
453-467.

Gower, J. C. and Hand, D. J. (1996). *Biplots*. London: Chapman and
Hall.

Gower, J. C., Lubbe, S. and le Roux, N. J. (2011). *Understanding
Biplots*. Chichester: Wiley.

Greenacre, M. (2010). *Biplots in Practice*. Bilbao: BBVA Foundation.

la Grange, A., le Roux, N. and Gardner-Lubbe, S. (2009). BiplotGUI:
Interactive Biplots in R. *Journal of Statistical Software*, 30(12),
1-37.
