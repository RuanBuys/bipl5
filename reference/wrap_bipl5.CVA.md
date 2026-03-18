# Construct a bipl5_biplot from a CVA biplot

Builds mdsDisplays for the user's CV pair and available supplementary
pairs, along with a dropdown menu. Fit measures are not yet computed for
CVA biplots and will be `NULL`. Plotting is deferred to
[`plot.bipl5_biplot`](https://www.bipl5.co.za/reference/plot.bipl5_biplot.md).

## Usage

``` r
# S3 method for class 'CVA'
wrap_bipl5(x)
```

## Arguments

- x:

  An object of class `biplot` from the biplotEZ package with CVA method
  applied.

## Value

An object of class `c("bipl5_biplot", "cva")`

## Examples

``` r
if (FALSE) { # \dontrun{
library(biplotEZ)
bp <- biplot(iris[, 1:4]) |> CVA(classes = iris[, 5]) |> wrap_bipl5()
bp
plot(bp)
} # }
```
