"""
bipl5 — Interactive biplots for Python.

A Python port of the bipl5 R package, providing calibrated-axis biplots
rendered as interactive plotly widgets.

Public API
----------
init_biplot      Create a bipl5 specification from data.
scale_mds        Perform ordination and build a biplot.
format_samples   Reformat sample aesthetics (colour / symbol).
append_mds_display  Add a PC-pair display to an existing biplot.
remove_mds_display  Remove a PC-pair display.
extract          Nested attribute access on a biplot.

Classes
-------
Bipl5Spec        Pre-ordination specification.
Bipl5Biplot      Top-level biplot container.
Bipl5MdsDisplay  One PC-pair display.
Bipl5Data        Coordinate container for one display.
Bipl5FitMeasures Fit-panel traces (PCA only).
Bipl5Fit         Single extractable fit graph.
MdsDisplayBuilder  Low-level mutable builder for plotly traces.
"""

from ._spec import Bipl5Spec, init_biplot
from ._biplot import Bipl5Biplot
from ._data import Bipl5Data
from ._mds_display import Bipl5MdsDisplay, MdsDisplayBuilder
from ._fit import Bipl5FitMeasures, Bipl5Fit
from ._scale_mds import scale_mds
from ._format_samples import format_samples
from ._display_ops import append_mds_display, remove_mds_display
from ._extract import extract

__all__ = [
    # Functions
    "init_biplot",
    "scale_mds",
    "format_samples",
    "append_mds_display",
    "remove_mds_display",
    "extract",
    # Classes
    "Bipl5Spec",
    "Bipl5Biplot",
    "Bipl5MdsDisplay",
    "MdsDisplayBuilder",
    "Bipl5Data",
    "Bipl5FitMeasures",
    "Bipl5Fit",
]
