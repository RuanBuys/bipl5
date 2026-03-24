"""
Fit-measure containers.

R equivalents:
  - bipl5_fitmeasures  (new_bipl5_fitmeasures)
  - bipl5_fit          (new_bipl5_fit)
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any

from ._helpers import ft_label


# ---------------------------------------------------------------------------
# Bipl5FitMeasures — collection of fit panels (PCA only)
# ---------------------------------------------------------------------------

@dataclass
class Bipl5FitMeasures:
    """Plotly traces for the right-hand-side fit panel in PCA biplots.

    Attributes
    ----------
    cum_pred : list
        Cumulative predictivity traces.
    cum_ad : list
        Cumulative adequacy traces.
    var_exp : list
        Variance explained traces.
    scree : list
        Scree plot traces.
    fit_tables : dict[str, Any]
        Per-PC-pair marginal fit tables, keyed by ``fit_table_12`` etc.
    """

    cum_pred: list[dict[str, Any]] = field(default_factory=list)
    cum_ad: list[dict[str, Any]] = field(default_factory=list)
    var_exp: list[dict[str, Any]] = field(default_factory=list)
    scree: list[dict[str, Any]] = field(default_factory=list)
    fit_tables: dict[str, Any] = field(default_factory=dict)

    def __repr__(self) -> str:
        ft_keys = list(self.fit_tables.keys())
        lines = ["bipl5_fitmeasures"]
        for name in ("CumPred", "CumAd", "VarExp", "Scree"):
            lines.append(f"  {name}")
        for ft in ft_keys:
            lines.append(f"  {ft} ({ft_label(ft)})")
        return "\n".join(lines)


# ---------------------------------------------------------------------------
# Bipl5Fit — single extractable fit graph
# ---------------------------------------------------------------------------

@dataclass
class Bipl5Fit:
    """A single fit-measure graph, plottable independently.

    Attributes
    ----------
    trace_data : list
        Plotly trace dicts for this fit graph.
    fit_name : str | None
        Storage key, e.g. ``"CumPred"``.
    """

    trace_data: list[dict[str, Any]] = field(default_factory=list)
    fit_name: str | None = None

    def __repr__(self) -> str:
        n = len(self.trace_data)
        return f"bipl5_fit('{self.fit_name}', {n} traces)"
