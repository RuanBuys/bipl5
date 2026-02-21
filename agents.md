# agents.md — bipl5 (R + Plotly.js) Agent Guide

This file explains **what this package does**, how the **biplot object** is structured, and how the **R payload layer** maps to the **Plotly.js event layer**.

It is written for contributors (human or agent) who need to modify:
- the **R-side layer builders** (traces + layout + payloads), and/or
- the **JavaScript bindings** (event handlers + dynamic updates).

---

## 1) What the package does

`bipl5` builds **interactive PCA biplots** using **plotly** via **htmlwidgets**.

Key features:
- **Biplot display (LHS)** with observation points and multiple visual layers:
  - data points
  - optional polygons (alpha bags / ellipses)
  - class means (optional)
  - **linear axes** (calibrated axes)
  - **translated density axes (TDA)** (“ExpAx” axes + densities)
  - vector representation (optional)
  - unit circle / outer circle layers
- **Fit panel (RHS)** shown/hidden via the “Measures of Fit” button:
  - cumulative predictivity
  - cumulative adequacy
  - scree plot
  - proportion variance explained
  - summary table (special case)
- **PC toggle** dropdown to switch between PC pairs:
  - PC 1 & 2 (default initial view)
  - PC 1 & 3
  - PC 2 & 3
- Event-driven interactions in JS:
  - legend clicks toggle axes / layers / densities
  - clicking an observation draws “prediction lines” to axes
  - translated axes toggle between normal axes (“Ax”) and translated axes (“ExpAx”)
  - optional slider-driven **parallel axis translation** (for ExpAx)

---

## 2) Repository structure (conceptual)

Typical structure (adjust if your repo differs):
- `R/` — R functions that build plotly objects and payloads
- `inst/htmlwidgets/bipl5_plotly.js` — JS dependency attached via `htmlDependency()`
- `R/dependencies.R` (or similar) — `bipl5_dependency()` returning htmlDependency
- `R/widget.R` / plot functions — where `htmlwidgets::onRender()` or widget binding is applied

---

## 3) High-level architecture: “layers” + “payloads” + “events”

### 3.1 Layered construction (R-side)
The biplot is built “in layers”. Historically those layers were added directly using plotly verbs:
- `add_trace()`
- `add_annotations()`
- `layout()`

In the newer architecture, each layer has a **payload equivalent**:
- **Old style**: `insert_*()` functions mutate a `plotly` object.
- **Payload style**: `add_*_payload()` functions mutate a **payload list** with:
  - `trace_data` (array of traces)
  - `layout` (layout, including `annotations`)
  - optional additional named payload elements (e.g., `fit_table`, fit graphs, slider state)

This enables:
- fast switching between PC pairs (swap payloads)
- avoiding reconstructing all layers repeatedly
- JS-side dynamic injection/removal of precomputed traces (Fit panel, tables, etc.)

### 3.2 Payloads keyed by PC display
`data.payloads` is a JS-visible object shaped like:

```js
data.payloads = {
  "PC 1 & 2": {
    trace_data: [...],  // biplot traces for PC 1 & 2 (and sometimes fit traces)
    layout: {...},      // layout + annotations for PC 1 & 2
    bipl5: {...},       // optional persisted state
    fit_table: [...],   // optional prebuilt table traces
    slider_info: {      // optional slider state
      slider_pos: [...],
      slider_axis_idx: 0,
      step_size: <number>
    }
  },
  "PC 1 & 3": { ... },
  "PC 2 & 3": { ... }
}
```

**Important:** when saving back into `data.payloads[oldKey]`, do not discard other elements.
Use merging (e.g. `Object.assign({}, prev, {trace_data, layout, bipl5})`) to preserve existing keys like `fit_table`.

### 3.3 JS event layer
JS attaches event handlers to the rendered plotly element `el` via:

- `plotly_buttonclicked` for updatemenu buttons & dropdowns
- `plotly_legendclick` for legend toggles
- `plotly_click` for observation click → prediction lines
- optional: `plotly_hover`, `plotly_unhover` for class mean hover effects
- optional: `plotly_sliderchange` for slider-driven axis translation

All logic depends on trace/annotation **tagging conventions** (meta / legendgroup / customdata).
These conventions must remain stable.

---

## 4) The `el.bipl5` runtime state object (JS)

The JS binding initializes:

```js
el.bipl5 = {
  clicked: false,     // is there currently an active prediction selection?
  rel_but: [0,0,0,0], // button toggles state: ["PC","AxisStats","TransAxes","vecload"]
  is_visible: 0,      // whether fit panel is visible (domain changes)
  table_visible: 0,
  table2_visible: 1,
  vect_visible: 0,    // vector display active?
  but_names: ["PC","AxisStats","TransAxes","vecload"],
  currentPCKey: "PC 1 & 2",
  currentFMKey: "Cum. Predictivity"
};
```

### Design principle
- `el.bipl5` stores **interaction state for the currently rendered plot**
- `data.payloads[PCKey].bipl5` may store state snapshots per PC display
- some state is shared across PC payloads (e.g., hidden axis status) if continuity is desired

---

## 5) Tagging conventions (critical)

### 5.1 `meta` is the primary tag channel
A trace or annotation may have:
- `meta: "axis"` (string), OR
- `meta: ["FitPanel", "Scree Plot"]` (array)

JS must therefore use a robust accessor:

```js
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

**Rule:** If you introduce a new feature category, add it as a second tag:
- `meta: ["FitPanel", "<FeatureName>"]`

### 5.2 Core meta tags used throughout the codebase

#### Biplot core
- `meta: "data"` — observation traces (markers)
- `meta: "ClassMean"` — class mean traces
- `meta: "polygon"` — alpha bags / ellipses polygons
- `meta: "axis"` — linear calibrated axes traces (Ax*)
- `meta: "OuterCircle"` — outer circle boundary
- `meta: "veccircle"` — unit circle (often hidden until vector display)
- `meta: "vecload"` — vector display annotations/arrows (layout.annotations)

#### Translated density axes (TDA)
- `meta: "ExpAx"` — translated axes traces and their annotations
- `meta: "density"` — density traces tied to translated axes

#### Fit panel (Measures of Fit)
Typically encoded as:
- `meta: ["FitPanel", "Cum. Predictivity"]`
- `meta: ["FitPanel", "Cum. Adequacy"]`
- `meta: ["FitPanel", "Scree Plot"]`
- `meta: ["FitPanel", "Variance Explained"]`
- `meta: ["FitPanel", "Summary Table"]`
or for legacy traces:
- `meta: ["axis_pred", ...]`

#### Prediction lines
- `meta: "predict"` — prediction traces and prediction annotations

---

## 6) Naming conventions: legendgroup and customdata

### 6.1 Axis trace legendgroup patterns
These strings are parsed in JS and must follow a stable convention.

- Linear axes:
  - `legendgroup: "Ax1"`, `"Ax2"`, … `"Axp"`
- Translated axes:
  - `legendgroup: "ExpAx1"`, `"ExpAx2"`, … `"ExpAxp"`
- Data points group:
  - `legendgroup: "data"`
- Axis predictivity group:
  - `legendgroup: "AxPred"` (legacy fit graph group)
- Prediction legend-only trace group (if used):
  - `legendgroup: "Pred"` (or similar)

### 6.2 Parsing legendgroup in JS
A robust parser should support both Ax and ExpAx:

```js
function axisNameFromLegendgroup(lg){
  const m = (typeof lg === "string") ? lg.match(/^(ExpAx|Ax)(\d+)$/) : null;
  return m ? { type: m[1], num: Number(m[2]), axis: lg } : null;
}
```

This supports `"Ax11"` as well as `"ExpAx11"`.

### 6.3 `customdata` conventions (trace-side)

#### Observations (`meta:"data"`)
- `customdata: <integer row index>`
Used to identify the observation for prediction lines and annotation labels.

#### Axes (`meta:"axis"` or `meta:"ExpAx"`)
- `customdata: <vector of tick values>` or `zhat` values on the axis.
Used for prediction line interpolation.

#### Density traces (`meta:"density"`)
Two patterns exist:
- `customdata: "legendentry"` for a single legend-only trace used to toggle a class
- `customdata: ["ExpAx3", ...]` or simply `"ExpAx3"` depending on implementation

JS code must guard both:
- array form for `customdata[0]`
- string form for `customdata`

Suggested accessor:

```js
function customAxisRef(tr){
  if (!tr) return null;
  if (Array.isArray(tr.customdata)) return tr.customdata[0] ?? null;
  if (typeof tr.customdata === "string") return tr.customdata;
  return null;
}
```

### 6.4 `customdata` conventions (annotation-side)
- Axis tick annotations:
  - `meta:"Ax"` or `meta:"ExpAx"` and `customdata: <axis number>` (1..p)
- Prediction label annotations:
  - `meta:"predict"` and `customdata: <axis number>` (1..p)
This allows legend-click toggles to hide/show both axis ticks and prediction labels consistently.

---

## 7) Layout conventions

### 7.1 Axes IDs
- Biplot:
  - x-axis: `xaxis` (`x`)
  - y-axis: `yaxis` (`y`)
- Fit panel:
  - xaxis3 / yaxis3 for fit plots (domain typically right side)
- Fit table:
  - plotly table traces use `domain` rather than `xaxis/yaxis`, but still occupy space based on layout `xaxis.domain` / `yaxis.domain` changes.

### 7.2 Domain management (show/hide fit panel)
A common pattern:
- Fit panel hidden:
  - `xaxis.domain = [0, 1]`
- Fit panel visible:
  - `xaxis.domain = [0, 0.5]`
  - `xaxis3.domain = [0.65, 1]` (or similar)
  - `el.layout.updatemenus[2].visible = true` to show Fit dropdown

Prefer `Plotly.relayout(el, patch)` rather than mutating layout objects without relayout.

### 7.3 Updatemenus naming (must remain stable)
In scaffolding:
- Updatemenus[0]: top buttons (`AxisStats`, `TransAxes`, `vecload`, `EditAxes`)
- Updatemenus[1]: PC dropdown (`name: "PC_toggle"`)
- Updatemenus[2]: Fit dropdown (`name: "Fit_toggle"`)
- Updatemenus[3]: Axis selector dropdown for slider (`name: "Slider_toggle"`)

Buttons use `method="skip"` and `execute=FALSE` so Plotly does not auto-modify traces; JS handles `plotly_buttonclicked`.

---

## 8) Fit panel switching

Fit panel traces are **not** part of the biplot layers and should be treated as independent.

Principles:
- PC switching should not wipe RHS Fit panel traces.
- RHS content should persist across PC toggles.
- Exception: Summary Table is PC-dependent and must update when PC changes.

Recommended approach:
- Tag all RHS traces with `"FitPanel"` in `meta`.
- When toggling PC:
  - preserve current fit traces/table
  - swap only non-fit traces + biplot annotations
  - if current FM mode is `"Summary Table"`, rebuild RHS table from the new PC payload.

---

## 9) Prediction lines (“click observation”)

Workflow:
1. User clicks an observation (`plotly_click`) in the biplot.
2. JS computes orthogonal projections from the clicked point onto each axis.
3. JS adds:
   - optional **legend-only** trace for a single legend entry
   - one trace per axis (showlegend false)
   - one annotation per axis containing the predicted tick value

Tagging rules:
- Prediction traces: `meta:"predict"`
- Prediction annotations: `meta:"predict"`
- Prediction annotations use `customdata = axisNum` so they can hide with axes.

Removal:
- remove all traces with `meta:"predict"`
- remove all annotations with `meta:"predict"`

---

## 10) Slider-driven parallel translation (translated axes)

### 10.1 UI
- `TransAxes` enables translated-axis traces and reveals an intermediate `Edit: Axes` button.
- Clicking `Edit: Axes` shows only the axis dropdown first (no slider yet).
- The dropdown initially contains a prompt entry: `"Select Axis"`.
- Once a real axis is selected, the prompt is removed and the slider appears for that axis.
- Turning `Edit: Axes` off hides dropdown + slider and re-arms the `"Select Axis"` prompt for next use.

### 10.2 State storage
Per PC payload:
```js
payload.slider_info = {
  slider_pos: [<step idx per axis>],
  slider_axis_idx: 0,
  step_size: <number>,
  axis_chosen: <boolean>
}
```
- `axis_chosen === false`: prompt mode (`"Select Axis"` visible), slider hidden.
- `axis_chosen === true`: a concrete axis is selected; slider visible when edit mode is on.
- This state is saved/restored per PC payload when switching via `PC_toggle`.

### 10.3 Event handling
- `plotly_buttonclicked` with `menu.name === "Slider_toggle"`:
  - if `"Select Axis"` is picked: keep slider hidden.
  - if a real axis is picked:
    - set `slider_axis_idx`
    - restore slider step from `slider_pos[axis]`
    - show slider
    - if that axis is hidden (`legendonly`), emit a `plotly_legendclick` for that axis to make it visible.
- `plotly_sliderchange`:
  - `dist = (newActive - prevActive) * step_size` (using payload-local state)
  - shift:
    - selected `ExpAx` trace(s)
    - linked density traces
    - matching `ExpAx` and `predict` annotations
    - selected prediction-line endpoint (if present)

Use a single `Plotly.update()` when possible.

---

## 11) R-side object structure (PCA biplot)

A typical PCA biplot object (`x`) used to build the plot contains:
- `x$X` : original data matrix
- `x$Z` : observation coordinates in biplot space (scores)
- `x$p`, `x$n`
- `x$eigenvalues`
- `x$e.vects` : basis selection for PC pairs
- `x$axes` : axis aesthetics (colors, tick label colors, etc.)
- `x$group.aes`, `x$samples`
- optional:
  - polygons (alpha bags / ellipses)
  - class means
  - vector display (`x$Vr`)
  - translated axes data (TDA)

---

## 12) Payload functions mirror legacy insert_* functions

Payload functions should:
- accept `(payload, x, ...)`
- return modified `payload`
- append to:
  - `payload$trace_data`
  - `payload$layout$annotations`
  - or feature-specific keys (e.g. `payload$fit_table`)

---

## 13) JS dependency integration

### 13.1 JS file as dependency
`inst/htmlwidgets/bipl5_plotly.js` contains:

```js
(function(){
  window.bipl5Attach = function(el, x, data){
    // initialize el.bipl5 and attach event handlers
  };
})();
```

### 13.2 R dependency helper
```r
bipl5_dependency <- function() {
  htmltools::htmlDependency(
    name    = "bipl5-plotly",
    version = as.character(utils::packageVersion("bipl5")),
    src     = c(file = "htmlwidgets"),
    script  = "bipl5_plotly.js",
    package = "bipl5"
  )
}
```

### 13.3 Common failure
`Uncaught TypeError: window.bipl5Attach is not a function`
means the dependency was not loaded (JS not attached, wrong `src`, file not in `inst/htmlwidgets`, or not included when using `load_all()`).

---

## 14) Debugging and invariants

### Hard invariants
- Ax legendgroups are `"Ax<number>"`
- ExpAx legendgroups are `"ExpAx<number>"`
- Axis tick annotations:
  - `meta: "Ax"` / `"ExpAx"`
  - `customdata: axisNum`
- Prediction annotations:
  - `meta:"predict"`
  - `customdata: axisNum`
- Fit panel traces should include `"FitPanel"` in meta (recommended)

---

## 15) Quick reference: meta tags at a glance

| Component | Trace meta | Annotation meta | legendgroup | customdata |
|---|---|---|---|---|
| Data points | `"data"` | — | `"data"` | row index |
| Class means | `"ClassMean"` | — | `"ClassMean"` | group index |
| Linear axis | `"axis"` | `"Ax"` | `"Ax#"` | axis: zhat vector; ann: axisNum |
| Outer circle | `"OuterCircle"` | — | — | — |
| Unit circle | `"veccircle"` | — | — | — |
| Vector arrows | — | `"vecload"` | — | — |
| Translated axes | `"ExpAx"` | `"ExpAx"` | `"ExpAx#"` | axis: tick values; ann: axisNum |
| Densities | `"density"` | — | group name | references `"ExpAx#"` |
| Prediction lines | `"predict"` | `"predict"` | often `"Ax#"` | ann customdata = axisNum |
| Fit panel plots | `["FitPanel", ...]` | — | `"FitPanel"` | — |
| Summary table | `["FitPanel","Summary Table"]` | — | `"FitPanel"` | — |

---

## 16) Final note

This package is **event-driven**: the JS logic is only as reliable as the trace/annotation metadata conventions.

When modifying R builders or payload functions, preserve:
- `meta` values
- `legendgroup` patterns
- `customdata` semantics
- layout axis IDs and updatemenus naming

If you must change any of these, update the corresponding JS search/parsing functions in `inst/htmlwidgets/bipl5_plotly.js`.
