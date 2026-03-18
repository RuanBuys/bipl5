# Create a bipl5 specification object

`init_biplot()` stores the raw data and preprocessing options needed to
construct a biplot later with
[`scale_mds()`](https://www.bipl5.co.za/reference/scale_mds.md). It does
not perform any ordination itself. When `data` is a data frame
containing both numeric and non-numeric columns, only the numeric
columns are used for the biplot calculation, while the full data frame
is retained for later formatting steps such as
[`format_samples()`](https://www.bipl5.co.za/reference/format_samples.md).

## Usage

``` r
init_biplot(data, center = TRUE, scale = FALSE)
```

## Arguments

- data:

  A matrix or data frame. If a data frame contains non-numeric columns,
  they are stored but excluded from the ordination input.

- center:

  Logical; should numeric variables be centered before analysis?

- scale:

  Logical; should numeric variables be scaled before analysis?

## Value

An object of class `bipl5_spec`.
