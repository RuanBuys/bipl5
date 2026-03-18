# Construct a bipl5_biplot from a PCO biplot

Handles two cases depending on the axis type stored in `x$PCOaxes`:

- Linear axes:

  Built identically to regression biplots via `build_one_mdsDisplay()`,
  including translated density axes.

- Spline axes:

  Uses a custom mdsDisplay builder (`build_spline_mdsDisplay()`) that
  places only sample points, the spline axis curves with tick marks, and
  a bounding circle. A custom JavaScript handler is attached at plot
  time.

In both cases there is a single mdsDisplay (`mdsDisplay_12`), no fit
measures, and
[`append_mdsDisplay()`](https://www.bipl5.co.za/reference/append_mdsDisplay.md)
/
[`remove_mdsDisplay()`](https://www.bipl5.co.za/reference/remove_mdsDisplay.md)
are disabled.

## Usage

``` r
# S3 method for class 'PCO'
wrap_bipl5(x)
```

## Arguments

- x:

  An object of class `biplot` from the biplotEZ package with
  [`PCO()`](https://rdrr.io/pkg/biplotEZ/man/PCO.html) method applied.

## Value

An object of class `c("bipl5_biplot", "pco")`

## Examples

``` r
if (FALSE) { # \dontrun{
library(biplotEZ)
bp <- biplot(iris[, 1:4]) |>
  PCO(dist.func = stats::dist) |>
  wrap_bipl5()
bp
plot(bp)
} # }
```
