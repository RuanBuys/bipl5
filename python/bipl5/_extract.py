"""
extract() — nested attribute access on a Bipl5Biplot.

R equivalent: extract()  (wrap_bipl5.R)

Supports multiple access styles:
  1. extract(bp, "mdsDisplay_12")             -> subset biplot to that display
  2. extract(bp, "mdsDisplay_12.Data.sample_coordinates") -> nested dot access
  3. extract(bp, from_="fit_measures", what="CumPred")    -> fit measure
"""

from __future__ import annotations

from typing import Any

from ._biplot import Bipl5Biplot
from ._fit import Bipl5Fit, Bipl5FitMeasures
from ._mds_display import Bipl5MdsDisplay


def extract(
    biplot: Bipl5Biplot,
    expr: str | None = None,
    from_: str | None = None,
    what: str | None = None,
) -> Any:
    """Extract a nested object from a :class:`Bipl5Biplot`.

    Parameters
    ----------
    biplot : Bipl5Biplot
        The biplot to extract from.
    expr : str | None
        Dot-separated path, e.g. ``"mdsDisplay_12"`` or
        ``"mdsDisplay_12.data.sample_coordinates"``.
    from_ : str | None
        Top-level key (e.g. ``"fit_measures"``).
    what : str | None
        Sub-key within *from_* (e.g. ``"CumPred"``).

    Returns
    -------
    Any
        The extracted object.  When extracting a single mdsDisplay by name,
        returns a subset :class:`Bipl5Biplot` containing only that display.
        When extracting fit measures, returns a :class:`Bipl5Fit`.
    """
    # Style 3: from_/what keyword access
    if from_ is not None:
        return _extract_from_what(biplot, from_, what)

    if expr is None:
        raise ValueError("extract() requires either 'expr' or 'from_'.")

    parts = expr.split(".")

    # Style 1: just an mdsDisplay name -> subset biplot
    if len(parts) == 1 and parts[0] in biplot:
        return _subset_biplot(biplot, parts[0])

    # Style 2: dot-separated nested access
    return _walk_path(biplot, parts)


def _subset_biplot(biplot: Bipl5Biplot, name: str) -> Bipl5Biplot:
    """Return a new Bipl5Biplot containing only the specified mdsDisplay."""
    display = biplot[name]
    return Bipl5Biplot(
        mds_displays={name: display},
        fit_measures=biplot.fit_measures,
        meta=dict(biplot.meta),
        biplot_type=biplot.biplot_type,
    )


def _walk_path(biplot: Bipl5Biplot, parts: list[str]) -> Any:
    """Walk a dot-separated path through the biplot object hierarchy."""
    obj: Any = biplot

    for part in parts:
        if isinstance(obj, Bipl5Biplot):
            if part in obj:
                obj = obj[part]
            elif part == "fit_measures":
                obj = obj.fit_measures
            elif part == "meta":
                obj = obj.meta
            else:
                raise KeyError(f"'{part}' not found on Bipl5Biplot.")

        elif isinstance(obj, Bipl5MdsDisplay):
            if part == "data" or part == "Data":
                obj = obj.data
            elif part == "mds_display" or part == "mdsDisplay":
                obj = obj.mds_display
            elif part == "fit_qual":
                obj = obj.fit_qual
            elif part == "m":
                obj = obj.m
            elif part == "shift":
                obj = obj.shift
            else:
                raise KeyError(f"'{part}' not found on Bipl5MdsDisplay.")

        elif isinstance(obj, dict):
            if part not in obj:
                raise KeyError(f"'{part}' not found.")
            obj = obj[part]

        elif hasattr(obj, part):
            obj = getattr(obj, part)

        else:
            raise KeyError(f"Cannot resolve '{part}' on {type(obj).__name__}.")

    return obj


def _extract_from_what(biplot: Bipl5Biplot, from_: str, what: str | None) -> Any:
    """Handle from_=/what= style extraction."""
    if from_ == "fit_measures":
        fm = biplot.fit_measures
        if fm is None:
            raise ValueError("This biplot has no fit_measures.")

        if what is None:
            return fm

        # Map common names to attributes
        attr_map = {
            "CumPred": "cum_pred",
            "CumAd": "cum_ad",
            "VarExp": "var_exp",
            "Scree": "scree",
        }

        if what in attr_map:
            trace_data = getattr(fm, attr_map[what])
            return Bipl5Fit(trace_data=trace_data, fit_name=what)

        if what in fm.fit_tables:
            return fm.fit_tables[what]

        raise KeyError(f"'{what}' not found in fit_measures.")

    # Generic fallback: treat from_ as an mdsDisplay name
    if from_ in biplot:
        display = biplot[from_]
        if what is not None:
            return _walk_path(biplot, [from_, what])
        return display

    raise KeyError(f"'{from_}' not found on biplot.")
