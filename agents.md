# agents.md — bipl5 (R + Plotly.js) Agent Guide

This file explains **what this package does**, how the **biplot object**
is structured, and how the **R mdsDisplay layer** maps to the
**Plotly.js event layer**.

It is written for contributors (human or agent) who need to modify: -
the **R-side layer builders** (traces + layout + mdsDisplays), and/or -
the **JavaScript bindings** (event handlers + dynamic updates).

------------------------------------------------------------------------

## 1) What the package does

`bipl5` builds **interactive PCA and CVA biplots** using **plotly** via
**htmlwidgets**.

The central user-facing workflow is:

``` r
# PCA
biplot(iris) |> PCA() |> wrap_bipl5() |> plot()

# CVA
biplot(iris[,1:4]) |> CVA(classes = iris[,5]) |> wrap_bipl5() |> plot()
```

[`wrap_bipl5()`](https://www.bipl5.co.za/reference/wrap_bipl5.md) is an
S3 generic that dispatches to
[`wrap_bipl5.PCA()`](https://www.bipl5.co.za/reference/wrap_bipl5.PCA.md)
or
[`wrap_bipl5.CVA()`](https://www.bipl5.co.za/reference/wrap_bipl5.CVA.md)
based on the class of the biplotEZ object. It returns a `bipl5_biplot`
object — a rich, self-contained R structure that can be printed,
inspected, subsetted, extended, and plotted.

Key features: - **Biplot display (LHS)** with observation points and
multiple visual layers: - data points (with hovertext showing actual and
predicted values) - optional polygons (alpha bags / concentration
ellipses) - class means (CVA biplots always; PCA when requested) -
**linear axes** (calibrated axes) - **translated density axes (TDA)**
(“ExpAx” axes + densities) - vector representation (PCA only; not
applicable to CVA) - unit circle / outer circle layers (PCA only) -
**Fit panel (RHS)** shown/hidden via the “Measures of Fit” button (PCA
only): - cumulative predictivity - cumulative adequacy - scree plot -
proportion variance explained - summary table (PC-pair-dependent) -
**PC/CV toggle** dropdown to switch between dimension pairs: - PCA: PC 1
& 2, PC 1 & 3, PC 2 & 3 (always 3 pairs) - CVA: up to 3 CV pairs,
determined by `min(g-1, p)` where `g` = number of groups - Event-driven
interactions in JS: - legend clicks toggle axes / layers / densities -
clicking an observation draws “prediction lines” to axes - translated
axes toggle between centered axes (“Ax”) and translated axes (“ExpAx”) -
optional slider-driven **parallel axis translation** (for ExpAx)

------------------------------------------------------------------------

## 2) Repository structure (conceptual)

- `R/wrap_bipl5.R` — The main file: S3 class constructors,
  [`wrap_bipl5()`](https://www.bipl5.co.za/reference/wrap_bipl5.md)
  generic + PCA/CVA methods,
  [`plot.bipl5_biplot()`](https://www.bipl5.co.za/reference/plot.bipl5_biplot.md),
  print methods,
  [`extract()`](https://www.bipl5.co.za/reference/extract.md),
  [`remove_mdsDisplay()`](https://www.bipl5.co.za/reference/remove_mdsDisplay.md),
  [`append_mdsDisplay()`](https://www.bipl5.co.za/reference/append_mdsDisplay.md),
  and internal helpers (`subset_biplot()`, naming utilities).
- `R/wrap_bipl5_helper.R` — `build_one_mdsDisplay()`: the unified
  mdsDisplay builder used by both PCA and CVA.
- `R/build_secondary_biplots.R` — All `insert_*_mdsDisplay()` and
  `add_*_mdsDisplay()` functions that construct individual layers
  (observations, class means, axes, polygons, TDA, vectors, fit tables,
  etc.).
- `R/mdsDisplay_constructor.R` — Low-level mdsDisplay primitives:
  `mdsDisplay_new()`, `mdsDisplay_add_traces()`,
  `mdsDisplay_add_layout()`.
- `R/biplotEZ_helper.R` — Helper functions: `obtain_xhat()`,
  `fit_quality()`, `hovertext_generator()`, `pch_to_plotly()`,
  `is_correlation()`.
- `R/deps.R` — `bipl5_dependency()` and `insert_linear_js_v1()`: the
  bridge between R mdsDisplays and the JS event layer.
- `inst/htmlwidgets/bipl5_plotly.js` — JS dependency: event handlers,
  `togglePC()`, fit panel switching, prediction lines, slider
  translation.

------------------------------------------------------------------------

## 3) The `wrap_bipl5()` system — S3 class hierarchy

### 3.1 Overview

[`wrap_bipl5()`](https://www.bipl5.co.za/reference/wrap_bipl5.md)
converts a biplotEZ `biplot` object into a structured `bipl5_biplot`
object. It: 1. Prepares the biplotEZ object (calls `samples()`,
`axes()`, `fit.measures()`, and for CVA also `means()`). 2.
Un-centers/un-scales `x$X` so hovertext shows **raw** observation values
(not the internally-centered/scaled values). 3. Enumerates all dimension
pairs (PC or CV) to build. 4. Calls `build_one_mdsDisplay()` for each
pair to produce a `bipl5_mdsDisplay`. 5. Assembles fit measures (PCA
only; CVA has `NULL` fit measures). 6. Bundles everything into a
`bipl5_biplot`.

### 3.2 S3 class hierarchy

    bipl5_biplot
    ├── class: c("bipl5_biplot", "pca")   ← PCA biplots
    │   or:    c("bipl5_biplot", "cva")   ← CVA biplots
    │
    ├── mdsDisplay_12 <bipl5_mdsDisplay>
    │   ├── mdsDisplay
    │   │   ├── trace_data   [list of plotly trace lists]
    │   │   ├── layout       [list with $annotations]
    │   │   └── slider_info  [list: slider_pos, step_size]
    │   ├── fit_qual         [character string, e.g. "PC 1 & 2: 82.3%"]
    │   ├── m, shift         [TDA slope/shift data]
    │   └── Data <bipl5_data>
    │       ├── sample_coordinates             [n x 2 matrix (Z)]
    │       ├── axes_coordinates               [list of p axis coordinate matrices]
    │       └── translated_axes_coordinates    [TDA shift data]
    │
    ├── mdsDisplay_13 <bipl5_mdsDisplay>
    │   └── (same structure)
    ├── mdsDisplay_23 <bipl5_mdsDisplay>
    │   └── (same structure)
    │
    ├── fit_measures <bipl5_fitmeasures>   ← NULL for CVA
    │   ├── CumPred      [plotly traces]
    │   ├── CumAd        [plotly traces]
    │   ├── VarExp       [plotly traces]
    │   ├── Scree        [plotly traces]
    │   ├── fit_table_12 [plotly table traces]
    │   ├── fit_table_13 [plotly table traces]
    │   └── fit_table_23 [plotly table traces]
    │
    └── meta
        ├── x             [the original biplotEZ object]
        ├── color         [character vector of group colours]
        ├── symbol        [character vector of plotly marker symbols]
        ├── group         [factor of group memberships]
        ├── fit.quality   [character string]
        ├── pc_info       [named list: mdsDisplay_name → {pcs, label, ft_name}]
        └── dim_prefix    ["PC" or "CV"]

### 3.3 The four S3 classes

| Class               | Purpose                                                                                                                                      | Constructor               |
|---------------------|----------------------------------------------------------------------------------------------------------------------------------------------|---------------------------|
| `bipl5_biplot`      | Top-level container for all mdsDisplays + fit measures + metadata. Has a secondary class (`"pca"` or `"cva"`) for type-aware dispatch.       | `new_bipl5_biplot()`      |
| `bipl5_mdsDisplay`  | One dimension pair’s complete plotly data: traces, annotations, slider info, TDA data, and a nested `bipl5_data`.                            | `new_bipl5_mdsDisplay()`  |
| `bipl5_data`        | The numeric data behind one mdsDisplay: sample coordinates (Z), axis coordinates, and translated axis coordinates.                           | `new_bipl5_data()`        |
| `bipl5_fitmeasures` | Collection of fit-panel plotly traces (predictivity, adequacy, variance, scree) plus per-pair summary tables. PCA only — CVA returns `NULL`. | `new_bipl5_fitmeasures()` |

Each class has a [`print()`](https://rdrr.io/r/base/print.html) method
that renders a coloured tree diagram in the console (using the `crayon`
package).

### 3.4 mdsDisplay naming conventions

mdsDisplays are named by their dimension indices:

| Dimension pair | mdsDisplay name | Label (PCA)  | Label (CVA)  | Fit table name |
|----------------|-----------------|--------------|--------------|----------------|
| 1 & 2          | `mdsDisplay_12` | `"PC 1 & 2"` | `"CV 1 & 2"` | `fit_table_12` |
| 1 & 3          | `mdsDisplay_13` | `"PC 1 & 3"` | `"CV 1 & 3"` | `fit_table_13` |
| 2 & 3          | `mdsDisplay_23` | `"PC 2 & 3"` | `"CV 2 & 3"` | `fit_table_23` |
| 4 & 5          | `mdsDisplay_45` | `"PC 4 & 5"` | `"CV 4 & 5"` | `fit_table_45` |

These are generated by the helper functions `mdsDisplay_name()`,
`pair_label()`, and `ft_name()` at the top of `wrap_bipl5.R`.

### 3.5 The `pc_info` registry

`meta$pc_info` is the **single source of truth** for which mdsDisplays
exist and their human-readable labels. It is a named list keyed by
mdsDisplay name:

``` r
pc_info = list(
  mdsDisplay_13 = list(pcs = c(1, 3), label = "CV 1 & 3", ft_name = "fit_table_13"),
  mdsDisplay_12 = list(pcs = c(1, 2), label = "CV 1 & 2", ft_name = "fit_table_12"),
  mdsDisplay_23 = list(pcs = c(2, 3), label = "CV 2 & 3", ft_name = "fit_table_23")
)
```

All downstream code —
[`plot.bipl5_biplot()`](https://www.bipl5.co.za/reference/plot.bipl5_biplot.md),
[`print.bipl5_biplot()`](https://www.bipl5.co.za/reference/print.bipl5_biplot.md),
`subset_biplot()`,
[`append_mdsDisplay()`](https://www.bipl5.co.za/reference/append_mdsDisplay.md),
[`remove_mdsDisplay()`](https://www.bipl5.co.za/reference/remove_mdsDisplay.md),
[`extract()`](https://www.bipl5.co.za/reference/extract.md) — reads
`pc_info` to discover available mdsDisplays. **The first entry in
`pc_info` is always the user’s originally requested dimension pair.**

------------------------------------------------------------------------

## 4) `wrap_bipl5.PCA()` — PCA method

**File:** `R/wrap_bipl5.R`, line ~161

### What it does

1.  **Prepares** the biplotEZ object: `samples()`, `axes()`,
    `fit.measures()`.
2.  **Un-scales X**: Reverses the internal centering/scaling so `x$X`
    holds raw observation values for hovertext.
3.  **Determines 3 PC pairs**: The user’s pair is always first; the
    other two are chosen from `{(1,2), (1,3), (2,3)}`. If the user chose
    a non-standard pair (e.g., `e.vects = c(4, 5)`), the supplements are
    `(1,2)` and `(1,3)`.
4.  **Builds mdsDisplays** via `build_one_mdsDisplay()` for each pair.
    For the user’s pair, `include_polygons = TRUE` (alpha bags and
    ellipses are in that coordinate space). Secondary pairs get
    `include_polygons = FALSE`.
5.  **Builds fit measures**: cumulative predictivity, cumulative
    adequacy, variance explained, scree plot (shared across all pairs),
    plus per-pair summary tables.
6.  **Returns**
    `new_bipl5_biplot(mdsDisplays, fit_measures, meta, biplot_type = "pca")`.

### Key behaviours

- `ax_pred = TRUE` — axis predictivity traces are included in the
  scaffolding.
- `vec_dis = TRUE` — unit circle and vector loading annotations are
  included.
- `fit_measures` is a full `bipl5_fitmeasures` object.
- Correlation biplot detection: `is_correlation(x)` is checked and
  propagated to secondary
  [`biplotEZ::PCA()`](https://rdrr.io/pkg/biplotEZ/man/PCA.html) calls.

------------------------------------------------------------------------

## 5) `wrap_bipl5.CVA()` — CVA method

**File:** `R/wrap_bipl5.R`, line ~303

### What it does

1.  **Prepares** the biplotEZ object: `samples()`, `axes()`,
    `fit.measures()`, `means()`.
2.  **Un-scales X**: Same reversal as PCA.
3.  **Determines CV pairs**: Computes `max_cv = min(g - 1, p)`. If
    `max_cv >= 2`, builds standard pairs from `{(1,2), (1,3), (2,3)}` as
    available. The user’s pair is always first.
4.  **Builds mdsDisplays** with CVA-specific settings:
    - `dim_prefix = "CV"` — labels use “CV” not “PC”.
    - `ax_pred = FALSE` — no axis predictivity button (fit measures not
      yet implemented for CVA).
    - `vec_dis = FALSE` — no unit circle or vector loading annotations
      (not meaningful for CVA).
5.  **Secondary CVA objects**: For non-primary pairs, a fresh
    `biplotEZ::biplot() |> CVA(classes, e.vects)` is constructed. `x$X`
    is un-centered/un-scaled for each.
6.  **Fit measures = `NULL`**: CVA fit measures are not yet implemented.
7.  **Returns**
    `new_bipl5_biplot(mdsDisplays, NULL, meta, biplot_type = "cva")`.

### Key differences from PCA

| Feature                  | PCA                                                                                   | CVA                               |
|--------------------------|---------------------------------------------------------------------------------------|-----------------------------------|
| Secondary class          | `"pca"`                                                                               | `"cva"`                           |
| Dimension prefix         | `"PC"`                                                                                | `"CV"`                            |
| Fit measures             | Full `bipl5_fitmeasures`                                                              | `NULL`                            |
| Axis predictivity button | Yes                                                                                   | No                                |
| Vector representation    | Yes (unit circle + arrows)                                                            | No                                |
| Class means              | Only if [`biplotEZ::means()`](https://rdrr.io/pkg/biplotEZ/man/means.html) was called | Always (auto-called)              |
| Polygons                 | On user’s pair only                                                                   | On user’s pair only               |
| `means()` auto-called    | No                                                                                    | Yes                               |
| Number of pairs          | Always 3                                                                              | Up to 3, depends on `min(g-1, p)` |

### Class means in CVA

CVA biplots always display class means.
[`wrap_bipl5.CVA()`](https://www.bipl5.co.za/reference/wrap_bipl5.CVA.md)
ensures `biplotEZ::means(x)` is called before building mdsDisplays. In
`build_one_mdsDisplay()`, class mean coordinates are sourced from
`ez_obj$Zmeans` (the biplotEZ CVA object’s class means in the correct
canonical variate space). If `Zmeans` is unavailable, they are computed
as `colMeans(ez_obj$Z[group == g, ])` per group.

**Important serialization detail:** Class mean traces are single-point
scatter traces. Their `x` and `y` coordinates must be wrapped in
[`list()`](https://rdrr.io/r/base/list.html) (e.g., `x = list(Z[i, 1])`)
so that they serialize to JSON arrays (`[1.5]`) rather than bare scalars
(`1.5`). Bare scalars cause `Plotly.react()` to fail silently for
secondary mdsDisplays that bypass R’s
[`plotly::add_trace()`](https://rdrr.io/pkg/plotly/man/add_trace.html)
processing.

------------------------------------------------------------------------

## 6) `build_one_mdsDisplay()` — the unified mdsDisplay builder

**File:** `R/wrap_bipl5_helper.R`

This function is the **single entry point** for constructing a
mdsDisplay for any dimension pair, whether PCA or CVA. Both
[`wrap_bipl5.PCA()`](https://www.bipl5.co.za/reference/wrap_bipl5.PCA.md)
and
[`wrap_bipl5.CVA()`](https://www.bipl5.co.za/reference/wrap_bipl5.CVA.md)
call it.

### Signature

``` r
build_one_mdsDisplay(
  ez_obj,              # biplotEZ object for this specific dimension pair
  group,               # factor of group memberships
  color,               # character vector of group colours
  symbol,              # character vector of plotly marker symbols
  x_ref,               # the primary biplotEZ object (for polygon data + means aesthetics)
  include_polygons,    # TRUE only for the user's primary pair
  dim_prefix = "PC",   # "PC" or "CV"
  ax_pred = TRUE,      # include axis predictivity scaffolding?
  vec_dis = TRUE       # include unit circle + vector annotations?
)
```

### Layer construction order

1.  **Scaffolding mdsDisplay** — axis title, layout shell
2.  **Polygons** (if `include_polygons = TRUE`) — alpha bags,
    concentration ellipses from `x_ref`
3.  **Reconstructed values** (`obtain_xhat()`) and **axis coordinates**
    (`axes_coordinates()`)
4.  **Sample points** — observation scatter traces with hovertext
    (actual vs predicted values, sample predictivity)
5.  **Class means** (if `x_ref$class.means` is `TRUE`) — one
    single-point scatter trace per group
6.  **Linear axes** — calibrated biplot axes with tick marks and labels
7.  **Unit circle + vector annotations** (if `vec_dis = TRUE`, PCA only)
8.  **Translated density axes (TDA)** — shifted axes with kernel density
    overlays
9.  **Slider controls** — step positions for the axis translation slider
10. **Data object** — numeric coordinates bundled into a `bipl5_data`
11. **Final assembly** — `new_bipl5_mdsDisplay(bundle, data)`

### The `x_ref` parameter

`x_ref` is always the **user’s primary biplotEZ object**. It provides: -
Polygon data (`alpha.bags`, `conc.ellipses`) — only valid in the primary
pair’s coordinate space. - Class means **aesthetics** (`means.aes$pch`,
`means.aes$col`) — shared across all pairs. - The `class.means` flag —
determines whether class means are plotted at all.

Class mean **coordinates** come from `ez_obj$Zmeans` (the current pair’s
biplotEZ object), not from `x_ref`. This ensures each pair shows means
in the correct 2D space.

------------------------------------------------------------------------

## 7) `plot.bipl5_biplot()` — rendering the widget

**File:** `R/wrap_bipl5.R`, line ~448

### How it works

1.  **Detect available mdsDisplays** by scanning `pc_info` for non-NULL
    entries.
2.  **Create plotly scaffolding** — empty plotly widget with update
    menus, dropdowns, slider.
    - For CVA: `vec_dis = FALSE`, `ax_pred = FALSE` (no vector/fit
      buttons).
    - PC/CV dropdown buttons are trimmed to only available mdsDisplays.
3.  **Render the first mdsDisplay** directly into the plotly widget via
    [`plotly::add_trace()`](https://rdrr.io/pkg/plotly/man/add_trace.html)
    and `plotly::layout()` (annotations).
4.  **Build `mdsDisplay_for_js`** — a named list keyed by display label
    (e.g., `"PC 1 & 2"`):
    - **First mdsDisplay**: only `slider_info` and `fit_table` are
      passed (traces are already in the plotly graph).
    - **Secondary mdsDisplays**: the full `$mdsDisplay` is passed
      (including `trace_data`, `layout`, `slider_info`).
5.  **Build `fm_mdsDisplay`** — fit measures traces (PCA only; `NULL`
    for CVA).
6.  **Attach JavaScript** via `insert_linear_js_v1()` which calls
    [`htmlwidgets::onRender()`](https://rdrr.io/pkg/htmlwidgets/man/onRender.html).

### Why the first mdsDisplay is treated differently

R’s
[`plotly::add_trace()`](https://rdrr.io/pkg/plotly/man/add_trace.html)
processes traces properly (wraps scalars to arrays, normalises data
structures). Secondary mdsDisplays go through JSON serialization and are
injected directly into `Plotly.react()` by JavaScript. This is why trace
data for secondary mdsDisplays must use JSON-safe formats (e.g.,
`list(value)` instead of bare scalars for single-point traces).

------------------------------------------------------------------------

## 8) `extract()` — drilling into a bipl5_biplot

**File:** `R/wrap_bipl5.R`, line ~923

[`extract()`](https://www.bipl5.co.za/reference/extract.md) is an S3
generic with a `bipl5_biplot` method. It provides three calling styles
for inspecting or subsetting a biplot object:

### Style 1: mdsDisplay subset (returns a plottable bipl5_biplot)

``` r
bp <- biplot(iris) |> PCA() |> wrap_bipl5()
sub <- extract(bp, mdsDisplay_12)
plot(sub)  # plots only PC 1 & 2
```

When a bare mdsDisplay name is given,
[`extract()`](https://www.bipl5.co.za/reference/extract.md) calls
`subset_biplot()` internally to return a **new `bipl5_biplot`**
containing only that mdsDisplay (plus its fit table). The result is
fully plottable — the PC toggle dropdown simply has fewer options.

### Style 2: Two-level extraction (from + what)

``` r
extract(bp, from = mdsDisplay_12, what = Data)
# Returns the bipl5_data object for PC 1 & 2

extract(bp, from = mdsDisplay_12, what = fit_qual)
# Returns: "PC 1 & 2: 82.3%"
```

### Style 3: Arbitrary depth via `$` path expression

``` r
extract(bp, mdsDisplay_12$Data$sample_coordinates)
# Returns the n x 2 matrix of observation scores for PC 1 & 2

extract(bp, mdsDisplay_13$Data$axes_coordinates)
# Returns list of axis coordinate matrices for PC 1 & 3
```

The `$` expression is walked via `deparse_path()` which recursively
decomposes the expression into a character vector of field names.

**Note:** All arguments use **non-standard evaluation** (bare names, not
strings). The mdsDisplay names follow the `mdsDisplay_XY` convention.

------------------------------------------------------------------------

## 9) `remove_mdsDisplay()` — dropping a dimension pair

**File:** `R/wrap_bipl5.R`, line ~997

``` r
bp2 <- remove_mdsDisplay(bp, mdsDisplay_23)
```

Removes the specified mdsDisplay from the `bipl5_biplot` and returns a
new object. Validates that: - The mdsDisplay name exists in `pc_info`. -
The mdsDisplay is not `NULL`. - At least one mdsDisplay remains after
removal.

Internally delegates to `subset_biplot()` with the complement set.

------------------------------------------------------------------------

## 10) `append_mdsDisplay()` — adding a new dimension pair

**File:** `R/wrap_bipl5.R`, line ~1059

``` r
bp2 <- append_mdsDisplay(bp, eigenvectors = c(4, 5))
```

Adds a new mdsDisplay for an arbitrary dimension pair to an existing
`bipl5_biplot`. Works for both PCA and CVA.

### Validation

- `eigenvectors` must be a numeric vector of length 2.
- Both indices must be between 1 and `p` (number of variables).
- The pair must not already exist (checked against `pc_info`).
- The pair is automatically sorted (e.g., `c(5, 3)` becomes `c(3, 5)`).

### Build process

1.  Detects biplot type from the secondary class (`"pca"` or `"cva"`).
2.  Creates a fresh biplotEZ object for the new pair:
    - PCA:
      `biplotEZ::biplot() |> PCA(e.vects = pcs, correlation.biplot = ...) |> axes() |> fit.measures()`
    - CVA:
      `biplotEZ::biplot() |> CVA(classes = ..., e.vects = pcs) |> axes() |> fit.measures()`,
      plus un-centering/un-scaling of `X`.
3.  Calls `build_one_mdsDisplay()` with `include_polygons = FALSE`
    (polygon coordinates are only valid in the primary pair’s space).
4.  For PCA: builds a new fit table for the pair and appends it to
    `fit_measures`.
5.  Extends `pc_info` with the new entry.
6.  Returns a new `bipl5_biplot` containing all original mdsDisplays
    plus the new one.

### Example: extending beyond standard pairs

``` r
bp <- biplot(state.x77) |> PCA() |> wrap_bipl5()
bp  # has mdsDisplay_12, mdsDisplay_13, mdsDisplay_23

bp2 <- append_mdsDisplay(bp, c(4, 5))
bp2  # has mdsDisplay_12, mdsDisplay_13, mdsDisplay_23, mdsDisplay_45
plot(bp2)  # dropdown now has 4 options
```

------------------------------------------------------------------------

## 11) `subset_biplot()` — internal subsetting engine

**File:** `R/wrap_bipl5.R`, line ~850

Not exported. Used by
[`extract()`](https://www.bipl5.co.za/reference/extract.md) (mdsDisplay
subset style),
[`remove_mdsDisplay()`](https://www.bipl5.co.za/reference/remove_mdsDisplay.md),
and
[`append_mdsDisplay()`](https://www.bipl5.co.za/reference/append_mdsDisplay.md).

``` r
subset_biplot(bp, keep = c("mdsDisplay_12", "mdsDisplay_23"))
```

Creates a new `bipl5_biplot` containing only the specified mdsDisplays.
Also subsets: - `fit_measures`: keeps only the fit tables for retained
pairs; shared measures (CumPred, CumAd, VarExp, Scree) are always
preserved. - `pc_info`: filtered to retained entries. - Secondary class
(`"pca"` / `"cva"`) is preserved.

------------------------------------------------------------------------

## 12) Hovertext and the X un-scaling requirement

### The problem

biplotEZ internally centers and scales `x$X` during PCA/CVA processing.
If left as-is, the hovertext (which shows “Actual” values from `x$X`)
would display transformed values instead of the user’s raw data.

### The fix

Both
[`wrap_bipl5.PCA()`](https://www.bipl5.co.za/reference/wrap_bipl5.PCA.md)
and
[`wrap_bipl5.CVA()`](https://www.bipl5.co.za/reference/wrap_bipl5.CVA.md)
reverse the transformation before building mdsDisplays:

``` r
# Un-scale
if (x$scaled) {
  x$X <- scale(x$X, center = FALSE, scale = 1 / x$sd)
}
# Un-center
if (x$center) {
  x$X <- scale(x$X, -x$means, scale = FALSE)
}
```

This must happen in **three places**: 1. The primary `wrap_bipl5.*()`
method body. 2. The loop that builds secondary biplotEZ objects (for
non-primary pairs). 3.
[`append_mdsDisplay()`](https://www.bipl5.co.za/reference/append_mdsDisplay.md)
when constructing a CVA mdsDisplay.

The “Predicted” column in hovertext is handled by `obtain_xhat()`, which
already applies the inverse transformation internally.

------------------------------------------------------------------------

## 13) High-level architecture: “layers” + “mdsDisplays” + “events”

### 13.1 Layered construction (R-side)

The biplot is built “in layers”. Historically those layers were added
directly using plotly verbs: - `add_trace()` - `add_annotations()` -
[`layout()`](https://rdrr.io/r/graphics/layout.html)

In the newer architecture, each layer has a **mdsDisplay equivalent**: -
**Old style**: `insert_*()` functions mutate a `plotly` object. -
**mdsDisplay style**: `add_*_mdsDisplay()` / `insert_*_mdsDisplay()`
functions mutate a **mdsDisplay list** with: - `trace_data` (array of
traces) - `layout` (layout, including `annotations`) - optional
additional named mdsDisplay elements (e.g., `fit_table`, fit graphs,
slider state)

This enables: - fast switching between PC/CV pairs (swap mdsDisplays) -
avoiding reconstructing all layers repeatedly - JS-side dynamic
injection/removal of precomputed traces (Fit panel, tables, etc.)

### 13.2 mdsDisplays keyed by display label (JS side)

`data.mdsDisplays` is a JS-visible object shaped like:

``` js
data.mdsDisplays = {
  "PC 1 & 2": {           // or "CV 1 & 2" for CVA
    trace_data: [...],     // biplot traces (only for secondary mdsDisplays)
    layout: {...},         // layout + annotations (only for secondary mdsDisplays)
    bipl5: {...},          // persisted interaction state
    fit_table: [...],      // optional prebuilt table traces (PCA only)
    slider_info: {         // slider state
      slider_pos: [...],
      slider_axis_idx: 0,
      step_size: <number>,
      axis_chosen: <boolean>
    }
  },
  "PC 1 & 3": { ... },
  "PC 2 & 3": { ... }
}
```

**Important:** The first mdsDisplay initially has **no** `trace_data` or
`layout` in JS — those traces are already in the plotly widget from R’s
`add_trace()`. The first time the user toggles away, the current
`el.data` traces are captured and saved into
`data.mdsDisplays[oldKey].trace_data`.

**Important:** When saving back into `data.mdsDisplays[oldKey]`, do not
discard other elements. Use merging
(e.g. `Object.assign({}, prev, {trace_data, layout, bipl5})`) to
preserve existing keys like `fit_table`.

### 13.3 JS event layer

JS attaches event handlers to the rendered plotly element `el` via:

- `plotly_buttonclicked` for updatemenu buttons & dropdowns
- `plotly_legendclick` for legend toggles
- `plotly_click` for observation click → prediction lines
- optional: `plotly_hover`, `plotly_unhover` for class mean hover
  effects
- optional: `plotly_sliderchange` for slider-driven axis translation

All logic depends on trace/annotation **tagging conventions** (meta /
legendgroup / customdata). These conventions must remain stable.

------------------------------------------------------------------------

## 14) The `el.bipl5` runtime state object (JS)

The JS binding initializes:

``` js
el.bipl5 = {
  clicked: false,           // is there currently an active prediction selection?
  rel_but: [0, 0, 0, 0, 0], // button toggle state
  is_visible: true,         // whether fit panel is visible
  vect_visible: 0,          // vector display active?
  but_names: ["PC", "AxisStats", "TransAxes", "vecload", "EditAxes"],
  currentPCKey: _initPCKey, // e.g., "PC 1 & 2" or "CV 1 & 3"
  currentFMKey: "Cum. Predictivity"
};
```

### Design principle

- `el.bipl5` stores **interaction state for the currently rendered
  plot**
- `data.mdsDisplays[PCKey].bipl5` stores state snapshots per PC/CV
  display
- `is_visible` is copied across toggles so the fit panel state persists
- `currentFMKey` is copied across toggles so the selected fit measure
  persists

------------------------------------------------------------------------

## 15) Tagging conventions (critical)

### 15.1 `meta` is the primary tag channel

A trace or annotation may have: - `meta: "axis"` (string), OR -
`meta: ["FitPanel", "Scree Plot"]` (array)

JS must therefore use a robust accessor:

``` js
function metaTag(obj){
  if (Array.isArray(obj.meta)) return obj.meta[0];
  if (typeof obj.meta === "string") return obj.meta;
  return null;
}

function hasMeta(obj, key){
  if (!obj) return false;
  if (typeof obj.meta === "string") return obj.meta === key;
  if (Array.isArray(obj.meta)) return obj.meta.includes(key);
  return false;
}
```

**Rule:** If you introduce a new feature category, add it as a second
tag: - `meta: ["FitPanel", "<FeatureName>"]`

### 15.2 Core meta tags used throughout the codebase

#### Biplot core

- `meta: "data"` — observation traces (markers)
- `meta: "ClassMean"` — class mean traces (always `list("ClassMean")` in
  R → `["ClassMean"]` in JSON)
- `meta: "polygon"` — alpha bags / ellipses polygons
- `meta: "axis"` — linear calibrated axes traces (Ax\*)
- `meta: "OuterCircle"` — outer circle boundary
- `meta: "veccircle"` — unit circle (often hidden until vector display)
- `meta: "vecload"` — vector display annotations/arrows
  (layout.annotations)

#### Translated density axes (TDA)

- `meta: "ExpAx"` — translated axes traces and their annotations
- `meta: "density"` — density traces tied to translated axes

#### Fit panel (Measures of Fit) — PCA only

Typically encoded as: - `meta: ["FitPanel", "Cum. Predictivity"]` -
`meta: ["FitPanel", "Cum. Adequacy"]` -
`meta: ["FitPanel", "Scree Plot"]` -
`meta: ["FitPanel", "Variance Explained"]` -
`meta: ["FitPanel", "Summary Table"]`

#### Prediction lines

- `meta: "predict"` — prediction traces and prediction annotations

------------------------------------------------------------------------

## 16) Naming conventions: legendgroup and customdata

### 16.1 Axis trace legendgroup patterns

These strings are parsed in JS and must follow a stable convention.

- Linear axes:
  - `legendgroup: "Ax1"`, `"Ax2"`, … `"Axp"`
- Translated axes:
  - `legendgroup: "ExpAx1"`, `"ExpAx2"`, … `"ExpAxp"`
- Data points group:
  - `legendgroup: "data"`
- Class means group:
  - `legendgroup: "ClassMean"`

### 16.2 Parsing legendgroup in JS

A robust parser should support both Ax and ExpAx:

``` js
function axisNameFromLegendgroup(lg){
  const m = (typeof lg === "string") ? lg.match(/^(ExpAx|Ax)(\d+)$/) : null;
  return m ? { type: m[1], num: Number(m[2]), axis: lg } : null;
}
```

### 16.3 `customdata` conventions (trace-side)

#### Observations (`meta:"data"`)

- `customdata: <integer row index>` Used to identify the observation for
  prediction lines and annotation labels.

#### Axes (`meta:"axis"` or `meta:"ExpAx"`)

- `customdata: <vector of tick values>` or `zhat` values on the axis.
  Used for prediction line interpolation.

#### Density traces (`meta:"density"`)

Two patterns exist: - `customdata: "legendentry"` for a single
legend-only trace used to toggle a class - `customdata: ["ExpAx3", ...]`
or simply `"ExpAx3"` depending on implementation

### 16.4 `customdata` conventions (annotation-side)

- Axis tick annotations:
  - `meta:"Ax"` or `meta:"ExpAx"` and `customdata: <axis number>` (1..p)
- Prediction label annotations:
  - `meta:"predict"` and `customdata: <axis number>` (1..p) This allows
    legend-click toggles to hide/show both axis ticks and prediction
    labels consistently.

------------------------------------------------------------------------

## 17) Layout conventions

### 17.1 Axes IDs

- Biplot:
  - x-axis: `xaxis` (`x`)
  - y-axis: `yaxis` (`y`)
- Fit panel:
  - xaxis3 / yaxis3 for fit plots (domain typically right side)
- Fit table:
  - plotly table traces use `domain` rather than `xaxis/yaxis`, but
    still occupy space based on layout `xaxis.domain` / `yaxis.domain`
    changes.

### 17.2 Domain management (show/hide fit panel)

A common pattern: - Fit panel hidden: - `xaxis.domain = [0, 1]` - Fit
panel visible: - `xaxis.domain = [0, 0.5]` - `xaxis3.domain = [0.65, 1]`
(or similar) - `el.layout.updatemenus[2].visible = true` to show Fit
dropdown

### 17.3 Updatemenus naming (must remain stable)

In scaffolding: - Updatemenus\[0\]: top buttons (`AxisStats`,
`TransAxes`, `vecload`, `EditAxes`) - Updatemenus\[1\]: PC/CV dropdown
(`name: "PC_toggle"`) - Updatemenus\[2\]: Fit dropdown
(`name: "Fit_toggle"`) - Updatemenus\[3\]: Axis selector dropdown for
slider (`name: "Slider_toggle"`)

Buttons use `method="skip"` and `execute=FALSE` so Plotly does not
auto-modify traces; JS handles `plotly_buttonclicked`.

------------------------------------------------------------------------

## 18) Fit panel switching (PCA only)

Fit panel traces are **not** part of the biplot layers and should be
treated as independent.

Principles: - PC switching should not wipe RHS Fit panel traces. - RHS
content should persist across PC toggles. - Exception: Summary Table is
PC-dependent and must update when PC changes.

Recommended approach: - Tag all RHS traces with `"FitPanel"` in
`meta`. - When toggling PC: - preserve current fit traces/table - swap
only non-fit traces + biplot annotations - if current FM mode is
`"Summary Table"`, rebuild RHS table from the new PC mdsDisplay.

CVA biplots have no fit panel (`fit_measures = NULL`, `has_fm = FALSE`).
The fit panel button is hidden by
[`plot.bipl5_biplot()`](https://www.bipl5.co.za/reference/plot.bipl5_biplot.md)
via `ax_pred = has_fm` in the scaffolding.

------------------------------------------------------------------------

## 19) Prediction lines (“click observation”)

Workflow: 1. User clicks an observation (`plotly_click`) in the biplot.
2. JS computes orthogonal projections from the clicked point onto each
axis. 3. JS adds: - optional **legend-only** trace for a single legend
entry - one trace per axis (showlegend false) - one annotation per axis
containing the predicted tick value

Tagging rules: - Prediction traces: `meta:"predict"` - Prediction
annotations: `meta:"predict"` - Prediction annotations use
`customdata = axisNum` so they can hide with axes.

Removal: - remove all traces with `meta:"predict"` - remove all
annotations with `meta:"predict"`

------------------------------------------------------------------------

## 20) Slider-driven parallel translation (translated axes)

### 20.1 UI

- `TransAxes` enables translated-axis traces and reveals an intermediate
  `Edit: Axes` button.
- Clicking `Edit: Axes` shows only the axis dropdown first (no slider
  yet).
- The dropdown initially contains a prompt entry: `"Select Axis"`.
- Once a real axis is selected, the prompt is removed and the slider
  appears for that axis.
- Turning `Edit: Axes` off hides dropdown + slider and re-arms the
  `"Select Axis"` prompt for next use.

### 20.2 State storage

Per PC/CV mdsDisplay:

``` js
mdsDisplay.slider_info = {
  slider_pos: [<step idx per axis>],
  slider_axis_idx: 0,
  step_size: <number>,
  axis_chosen: <boolean>
}
```

- `axis_chosen === false`: prompt mode (`"Select Axis"` visible), slider
  hidden.
- `axis_chosen === true`: a concrete axis is selected; slider visible
  when edit mode is on.
- This state is saved/restored per PC/CV mdsDisplay when switching via
  `PC_toggle`.

### 20.3 Event handling

- `plotly_buttonclicked` with `menu.name === "Slider_toggle"`:
  - if `"Select Axis"` is picked: keep slider hidden.
  - if a real axis is picked:
    - set `slider_axis_idx`
    - restore slider step from `slider_pos[axis]`
    - show slider
    - if that axis is hidden (`legendonly`), emit a `plotly_legendclick`
      for that axis to make it visible.
- `plotly_sliderchange`:
  - `dist = (newActive - prevActive) * step_size` (using
    mdsDisplay-local state)
  - shift:
    - selected `ExpAx` trace(s)
    - linked density traces
    - matching `ExpAx` and `predict` annotations
    - selected prediction-line endpoint (if present)

Use a single `Plotly.update()` when possible.

------------------------------------------------------------------------

## 21) R-side biplotEZ object fields

### Common fields (PCA and CVA)

- `x$X` : data matrix (un-scaled/un-centered by `wrap_bipl5` for
  hovertext)
- `x$raw.X` : original data matrix (untouched, used to construct
  secondary biplotEZ objects)
- `x$Z` : observation coordinates in biplot space (scores, n x 2)
- `x$p`, `x$n` : number of variables and observations
- `x$eigenvalues` : eigenvalues from the decomposition
- `x$e.vects` : basis selection (which PC/CV pair)
- `x$axes` : axis aesthetics (tick label colours, etc.)
- `x$group.aes` : factor of group memberships
- `x$samples` : sample aesthetics (`$col`, `$pch`)
- `x$center`, `x$scaled` : logical flags for centering/scaling
- `x$means`, `x$sd` : column means and standard deviations (for
  un-scaling)
- `x$sample.predictivity` : per-observation predictivity (PCA)
- `x$within.class.sample.predictivity` : per-observation predictivity
  (CVA)

### PCA-specific

- `x$Vr` : variable loadings matrix (used for vector representation)
- `x$correlation.biplot` : logical flag

### CVA-specific

- `x$Zmeans` : class mean coordinates in canonical variate space (g x 2
  matrix)
- `x$class.means` : logical flag (always TRUE after `means()` is called)
- `x$means.aes` : class means aesthetics (`$pch`, `$col`)
- `x$alpha.bags`, `x$conc.ellipses` : polygon coordinate data (only in
  primary pair’s space)

------------------------------------------------------------------------

## 22) mdsDisplay functions mirror legacy insert\_\* functions

mdsDisplay functions should: - accept `(mdsDisplay, x, ...)` - return
modified `mdsDisplay` - append to: - `mdsDisplay$trace_data` -
`mdsDisplay$layout$annotations` - or feature-specific keys
(e.g. `mdsDisplay$fit_table`)

Key mdsDisplay functions in `R/build_secondary_biplots.R`:

| Function                            | What it adds                               | Meta tag                 |
|-------------------------------------|--------------------------------------------|--------------------------|
| `insert_Z_coo_mdsDisplay()`         | Observation scatter traces (one per group) | `"data"`                 |
| `insert_class_means_mdsDisplay()`   | Class mean point traces (one per group)    | `"ClassMean"`            |
| `insert_linear_axes_mdsDisplay()`   | Calibrated axis traces + tick annotations  | `"axis"` / `"Ax"`        |
| `insert_polygon_EZ_mdsDisplay()`    | Alpha bag / ellipse polygon traces         | `"polygon"`              |
| `insert_unit_circle_mdsDisplay()`   | Unit circle trace                          | `"veccircle"`            |
| `insert_vector_annots_mdsDisplay()` | Vector loading arrow annotations           | `"vecload"`              |
| `add_TDA_mdsDisplay()`              | Translated axes + density traces           | `"ExpAx"` / `"density"`  |
| `add_table_mdsDisplay()`            | Summary table traces                       | `"FitPanel"`             |
| `slider_control_mdsDisplay()`       | Slider step positions                      | (modifies `slider_info`) |

------------------------------------------------------------------------

## 23) JS dependency integration

### 23.1 JS file as dependency

`inst/htmlwidgets/bipl5_plotly.js` contains:

``` js
(function(){
  window.bipl5Attach = function(el, x, data){
    // initialize el.bipl5 and attach event handlers
  };
})();
```

### 23.2 R dependency helper and bridge

`R/deps.R` contains `insert_linear_js_v1()` which bridges R mdsDisplays
to JS:

``` r
insert_linear_js_v1(p_ly, p, cols, mdsDisplay, fm_mdsDisplay, initial_pc_key)
```

The `data` list passed to
[`htmlwidgets::onRender()`](https://rdrr.io/pkg/htmlwidgets/man/onRender.html): -
`data.p` — number of variables - `data.cols` — axis tick label colours -
`data.class_mean_hover` — `FALSE` (hover effects for class means) -
`data.mdsDisplays` — the named mdsDisplay list (keyed by display
label) - `data.fm_mdsDisplay` — fit measures traces (`NULL` for CVA) -
`data.initialPCKey` — the display label of the first mdsDisplay (e.g.,
`"CV 1 & 3"`)

### 23.3 Common failure

`Uncaught TypeError: window.bipl5Attach is not a function` means the
dependency was not loaded (JS not attached, wrong `src`, file not in
`inst/htmlwidgets`, or not included when using `load_all()`).

------------------------------------------------------------------------

## 24) Debugging and invariants

### Hard invariants

- Ax legendgroups are `"Ax<number>"`
- ExpAx legendgroups are `"ExpAx<number>"`
- Axis tick annotations:
  - `meta: "Ax"` / `"ExpAx"`
  - `customdata: axisNum`
- Prediction annotations:
  - `meta:"predict"`
  - `customdata: axisNum`
- Fit panel traces should include `"FitPanel"` in meta
- Single-point traces (class means) must use `list(value)` for x/y
  coordinates to serialize as JSON arrays

### PCA vs CVA invariants

- CVA: `fit_measures` is always `NULL`, `has_fm` is always `FALSE`
- CVA: `vec_dis = FALSE` (no vector representation)
- CVA: `ax_pred = FALSE` (no axis predictivity button)
- CVA: `dim_prefix = "CV"`, display labels use “CV” prefix
- CVA: `class.means` is always `TRUE` (auto-called in
  [`wrap_bipl5.CVA()`](https://www.bipl5.co.za/reference/wrap_bipl5.CVA.md))

------------------------------------------------------------------------

## 25) Quick reference: meta tags at a glance

| Component        | Trace meta                     | Annotation meta | legendgroup   | customdata                      |
|------------------|--------------------------------|-----------------|---------------|---------------------------------|
| Data points      | `"data"`                       | —               | `"data"`      | row index                       |
| Class means      | `["ClassMean"]`                | —               | `"ClassMean"` | group index                     |
| Linear axis      | `"axis"`                       | `"Ax"`          | `"Ax#"`       | axis: zhat vector; ann: axisNum |
| Outer circle     | `"OuterCircle"`                | —               | —             | —                               |
| Unit circle      | `"veccircle"`                  | —               | —             | —                               |
| Vector arrows    | —                              | `"vecload"`     | —             | —                               |
| Translated axes  | `"ExpAx"`                      | `"ExpAx"`       | `"ExpAx#"`    | axis: tick values; ann: axisNum |
| Densities        | `"density"`                    | —               | group name    | references `"ExpAx#"`           |
| Prediction lines | `"predict"`                    | `"predict"`     | often `"Ax#"` | ann customdata = axisNum        |
| Fit panel plots  | `["FitPanel", ...]`            | —               | `"FitPanel"`  | —                               |
| Summary table    | `["FitPanel","Summary Table"]` | —               | `"FitPanel"`  | —                               |

------------------------------------------------------------------------

## 26) Quick reference: complete user API

``` r
# ── Construction ──────────────────────────────────────────────
bp <- biplot(iris) |> PCA() |> wrap_bipl5()
bp <- biplot(iris[,1:4]) |> CVA(classes = iris[,5]) |> wrap_bipl5()

# ── Inspection ────────────────────────────────────────────────
bp                    # coloured tree diagram
bp$mdsDisplay_12         # print a single mdsDisplay
bp$mdsDisplay_12$Data    # print the data sub-object

# ── Plotting ──────────────────────────────────────────────────
plot(bp)              # full interactive plotly widget

# ── Extraction ────────────────────────────────────────────────
extract(bp, mdsDisplay_12)                            # subset → plottable bipl5_biplot
extract(bp, from = mdsDisplay_12, what = Data)        # → bipl5_data
extract(bp, mdsDisplay_12$Data$sample_coordinates)    # → n x 2 matrix

# ── Modification ──────────────────────────────────────────────
bp2 <- remove_mdsDisplay(bp, mdsDisplay_23)              # drop a pair
bp3 <- append_mdsDisplay(bp, eigenvectors = c(4, 5))  # add a new pair
```

------------------------------------------------------------------------

## 27) Final note

This package is **event-driven**: the JS logic is only as reliable as
the trace/annotation metadata conventions.

When modifying R builders or mdsDisplay functions, preserve: - `meta`
values - `legendgroup` patterns - `customdata` semantics - layout axis
IDs and updatemenus naming - JSON array format for single-point traces
(use `list(value)` not bare scalars)

If you must change any of these, update the corresponding JS
search/parsing functions in `inst/htmlwidgets/bipl5_plotly.js`.
