# Format sample aesthetics on a bipl5_biplot

`format_samples()` rebuilds the sample-trace block inside each
`mdsDisplay` so that observations are grouped by `by` and rendered with
one trace per visual class. This means the visible trace structure,
legend labels, and stored sample-format metadata all stay aligned.

## Usage

``` r
format_samples(
  x,
  stratify = c("col", "symbol"),
  by = NULL,
  col = NULL,
  pch = NULL
)
```

## Arguments

- x:

  A `bipl5_biplot` object.

- stratify:

  Which aesthetic to change: `"col"` for marker colour or `"symbol"` for
  marker symbol.

- by:

  Optional grouping variable for the sample traces. This can be:

  - a bare column name stored in the dataset supplied to
    [`init_biplot()`](https://www.bipl5.co.za/reference/init_biplot.md)

  - a single character column name stored in the object, or

  - a vector/factor of length `n`, one value per observation.

  When `NULL`, the current sample grouping in `x$meta$group` is reused.

- col:

  Optional vector of colours. When `stratify = "col"`, this must have
  one value per visual class defined by `by`. If omitted, a default
  palette is used.

- pch:

  Optional vector of plotting symbols. When `stratify = "symbol"`, this
  must have one value per visual class defined by `by`. Numeric base-R
  pch codes are converted internally to plotly symbols; character plotly
  symbol names are also accepted.

## Value

A modified `bipl5_biplot`.

## Details

The function is intended for sample formatting only. It does not refit
the underlying ordination model. In particular, for CVA biplots the
fitted CVA classes are preserved and only the sample traces are
reformatted.

A first call to `format_samples()` creates one sample legend section for
the requested aesthetic. For example,
`format_samples(stratify = "col", by = Species)` will colour the
observations by `Species` and create a legend section headed `Species`
with one entry per class.

A second call can be used to add a second, independent sample
stratification. If the second call uses the same grouping variable as
the first call, both aesthetics are applied to the same set of classes
and the legend remains unified. If the second call uses a different
grouping variable, `format_samples()` creates a second legend section
and internally splits the observation layer into all observed
combinations of the two grouping variables.

For example, the sequence

`init_biplot(iris2) |> scale_mds("pca") |> format_samples(stratify = "col", by = Species) |> format_samples(stratify = "symbol", by = Band)`

will produce two sample legend sections:

- `Species` for the colour grouping

- `Band` for the symbol grouping

The visible observation traces are then split by `Species x Band`, but
these combination traces are hidden from the legend. Instead,
`format_samples()` inserts legend-only sample entries so the legend
remains easy to read.

If translated axes are available in the `mdsDisplay`, a colour
stratification also rebuilds the kernel-density traces on the translated
axes so that those densities reflect the colour classes. Symbol-only
stratification does not change the translated-axis densities. This
means:

- `format_samples(stratify = "col", ...)` recalculates translated-axis
  densities by the colour grouping

- `format_samples(stratify = "symbol", ...)` leaves the existing
  translated densities unchanged

The legend toggles operate across the full dual stratification:

- clicking a colour legend entry hides or shows all observations
  belonging to that colour class, across every symbol class

- clicking a symbol legend entry hides or shows all observations
  belonging to that symbol class, across every colour class

The formatting is applied to every `mdsDisplay` currently stored in the
object. If additional displays are later added with
[`append_mdsDisplay()`](https://www.bipl5.co.za/reference/append_mdsDisplay.md),
the stored sample-format state is reused so the new displays inherit the
same sample legend structure.

`format_samples()` supports two complementary workflows.

`Single stratification`

A single call to `format_samples()` rebuilds the sample layer so that
one trace is created per class in `by`. This updates the marker
appearance, the legend entries, and the stored sample-format metadata
consistently.

`Second stratification`

A second call to `format_samples()` can be used to add a second sample
aesthetic. This is most useful when colour and plotting symbol represent
different variables.

If the second call uses the same grouping structure as the first, the
result is still one legend section with one entry per class, but each
class now carries both a colour and a plotting symbol.

If the second call uses a different grouping structure, the object
stores two independent sample legend sections. Internally, the
observation layer is rebuilt as one hidden trace per observed
combination of the two grouping variables. The visible legend then shows
one section for each stratifying variable.

`Translated-axis densities`

When translated axes are present, the kernel-density traces on those
axes are tied to the current colour grouping. Applying
`format_samples(stratify = "col", ...)` rebuilds the translated-axis
density traces so they match the colour classes. Applying
`format_samples(stratify = "symbol", ...)` does not rebuild those
densities.

So:

- a first colour stratification updates both the sample layer and the
  translated-axis densities

- a later symbol stratification leaves those densities as they are

- if a symbol stratification is applied first and a colour
  stratification is added later, the translated-axis densities are
  rebuilt when the colour stratification is added

`Legend click behaviour`

When two different stratifications are active, the legend entries behave
like filters:

- clicking a class in the first legend section toggles all observations
  in that class, regardless of their membership in the second
  stratification

- clicking a class in the second legend section toggles all observations
  in that class, regardless of their membership in the first
  stratification

So if colours represent `Species` and symbols represent `Band`, clicking
`setosa` hides all `setosa` observations, while clicking `class1` hides
all `class1` observations across every species.

`Non-standard evaluation`

If `by` is supplied as a bare column name, `format_samples()` looks for
that column in the dataset stored by
[`init_biplot()`](https://www.bipl5.co.za/reference/init_biplot.md). If
`by` is supplied as a character string, it is interpreted as the name of
a stored column. If `by` is supplied as a vector, it must have one value
per observation; in that case the legend title defaults to `"Data"`
because there is no stored column name to display.

`CVA note`

`format_samples()` does not change the fitted CVA model. It only
reformats the sample traces. The grouping used to fit the CVA model
should therefore be specified in
[`scale_mds()`](https://www.bipl5.co.za/reference/scale_mds.md), not in
`format_samples()`.

## Examples

``` r
bp <- init_biplot(iris) |>
  scale_mds(type = "pca", eigenvectors = c(1, 2))

bp_species <- format_samples(
  bp,
  stratify = "col",
  by = Species,
  col = c("tomato", "steelblue", "darkgreen")
)

sample_idx <- vapply(
  bp_species$mdsDisplay_12$mdsDisplay$trace_data,
  function(tr) "data" %in% unlist(tr$meta),
  logical(1)
)

vapply(
  bp_species$mdsDisplay_12$mdsDisplay$trace_data[sample_idx],
  `[[`,
  character(1),
  "name"
)
#> [1] "setosa"     "versicolor" "virginica" 

bp_symbol <- format_samples(
  bp,
  stratify = "symbol",
  by = Species,
  pch = c(16, 17, 15)
)

iris2 <- iris
iris2$Band <- factor(
  rep(c("class1", "class2", "class3", "class4"), length.out = nrow(iris2))
)

bp_dual <- init_biplot(iris2) |>
  scale_mds(type = "pca", eigenvectors = c(1, 2)) |>
  format_samples(
    stratify = "col",
    by = Species,
    col = c("tomato", "steelblue", "darkgreen")
  ) |>
  format_samples(
    stratify = "symbol",
    by = Band,
    pch = c(12, 13, 14, 15)
  )

# When plotted, the legend now has one section for Species and one for Band.
# Clicking a Species entry hides that species across all Band classes.
# Clicking a Band entry hides that Band class across all Species classes.
if (interactive()) {
  plot(bp_dual)
}

bp_species_13 <- append_mdsDisplay(bp_species, c(1, 3))
```
