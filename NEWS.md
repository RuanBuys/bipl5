# bipl5 (development version)

* Additional arguments for label placement and line width
* New `wrap_bipl5_gg()` renders a biplotEZ object as a static ggplot2 biplot,
  the ggplot2 counterpart of `wrap_bipl5()`. PCA, CVA, regression and PCO
  biplots are supported, including the curved spline axes of a PCO biplot.
* New `geom_calibrated_axis()` layer (and the underlying `GeomCalibratedAxis`
  ggproto object) draws calibrated Gower axes in ggplot2. The axis geometry
  follows `gggda::geom_axis()`, the layer behind `ordr::geom_cols_axis()`, but
  the marker values and positions are taken from
  `biplotEZ::axes_coordinates()` rather than being re-derived from a
  centre/scale pair.

# bipl5 1.1

* Fixed a few bugs in the functions.
* Added functionality for missing values.

# bipl5 1.0.0

* Initial CRAN submission.

* Currently support PCA biplots with translated density axes
