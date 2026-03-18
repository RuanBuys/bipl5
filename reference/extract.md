# Extract nested components from a bipl5_biplot object

Three calling styles are supported:

1.  **mdsDisplay subset**: `extract(bp, mdsDisplay_12)` — returns a new
    `bipl5_biplot` containing only that mdsDisplay (plottable).

2.  **Two-level**:
    `extract(bp, from = mdsDisplay_12, what = sample_coordinates)` —
    returns the nested data element.

3.  **Arbitrary depth**:
    `extract(bp, mdsDisplay_12$Data$sample_coordinates)` — returns the
    nested data element.

## Usage

``` r
extract(object, expr, from, what)

# S3 method for class 'bipl5_biplot'
extract(object, expr, from, what)
```

## Arguments

- object:

  A `bipl5_biplot` object

- expr:

  An unquoted mdsDisplay name (e.g. `mdsDisplay_12`) or a path
  expression using `$` (e.g. `mdsDisplay_12$Data$sample_coordinates` or
  `fit_measures$CumPred`)

- from:

  Unquoted name of the top-level element

- what:

  Unquoted name of the nested element

## Value

A `bipl5_biplot` (mdsDisplay subset), a `bipl5_fit` object for
graph-based fit measures, or the requested sub-element.

## Details

In addition to the mdsDisplay access patterns above, graph-based fit
measures can be extracted directly with calls such as
`extract(bp, fit_measures, CumPred)` or
`extract(bp, fit_measures$CumPred)`. Supported graph-based fit measures
are `CumPred`, `CumAd`, `VarExp`, and `Scree`. These calls return a
`bipl5_fit` object that can be passed to
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) to obtain a
static ggplot2 version of the corresponding fit graph.

## Examples

``` r
bp <- biplotEZ::biplot(iris[, 1:4]) |>
  biplotEZ::PCA() |>
  wrap_bipl5()

only_12 <- extract(bp, mdsDisplay_12)
data_obj <- extract(bp, from = mdsDisplay_12, what = Data)
coords <- extract(bp, mdsDisplay_12$Data$sample_coordinates)

fit_plot <- extract(bp, fit_measures, CumPred)
plot(fit_plot)
```
