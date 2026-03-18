# Scale a biplot specification into a bipl5_biplot

`scale_mds()` turns a `bipl5_spec` created by
[`init_biplot()`](https://www.bipl5.co.za/reference/init_biplot.md) into
a fully formed `bipl5_biplot` by dispatching to one of the underlying
[`biplotEZ::PCA()`](https://rdrr.io/pkg/biplotEZ/man/PCA.html),
[`biplotEZ::CVA()`](https://rdrr.io/pkg/biplotEZ/man/CVA.html),
[`biplotEZ::PCO()`](https://rdrr.io/pkg/biplotEZ/man/PCO.html), or
[`biplotEZ::regress()`](https://rdrr.io/pkg/biplotEZ/man/regress.html)
methods and then compiling only the requested `mdsDisplay`. Any
additional displays can be added later with
[`append_mdsDisplay()`](https://www.bipl5.co.za/reference/append_mdsDisplay.md).

## Usage

``` r
scale_mds(x, type = c("pca", "cva", "pco", "regress"), ...)

# S3 method for class 'bipl5_spec'
scale_mds(x, type = c("pca", "cva", "pco", "regress"), ...)
```

## Arguments

- x:

  A `bipl5_spec` created by
  [`init_biplot()`](https://www.bipl5.co.za/reference/init_biplot.md).

- type:

  The biplot method to construct. One of `"pca"`, `"cva"`, `"pco"`,
  `"regress"`, or `"regression"`.

- ...:

  Additional named arguments for the chosen method.

## Value

A fully formed `bipl5_biplot`.

## Details

The `type` argument chooses the underlying biplot method. Additional
arguments are method-specific and should be supplied via `...`.

Supported aliases in `...`:

- Common: `classes`, `group_aes` / `group.aes`, `title` / `Title`

- PCA: `dimensions` / `dim.biplot`, `eigenvectors` / `e.vects`,
  `show_class_means` / `show.class.means` / `show_group_means` /
  `show.group.means`, `correlation_biplot` / `correlation.biplot`

- CVA: `classes`, `dimensions` / `dim.biplot`, `eigenvectors` /
  `e.vects`, `weighted_cva` / `weightedCVA`, `show_class_means` /
  `show.class.means` / `show_group_means` / `show.group.means`,
  `low_dim` / `low.dim`

- PCO: `Dmat` / `dist_mat`, `dist_func` / `dist.func`, `dist_func_cat` /
  `dist.func.cat`, `dimensions` / `dim.biplot`, `eigenvectors` /
  `e.vects`, `show_class_means` / `show.class.means` /
  `show_group_means` / `show.group.means`, `axes`

- `regress`: `Z` / `z`, `show_group_means` / `show.group.means` /
  `show_class_means` / `show.class.means`, `axes`

For `type = "pco"`, any remaining named arguments in `...` are forwarded
to the chosen distance function.
